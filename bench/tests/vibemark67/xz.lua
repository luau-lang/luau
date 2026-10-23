local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

--------------------------------------------------------------------------------
-- CRC32 and CRC64 tables
--------------------------------------------------------------------------------

local CRC32_TABLE = table.create(256, 0)
local CRC64_TABLE = {}

do
    for i = 0, 255 do
        local crc = i
        for _ = 1, 8 do
            if bit32.band(crc, 1) == 1 then
                crc = bit32.bxor(bit32.rshift(crc, 1), 0xEDB88320)
            else
                crc = bit32.rshift(crc, 1)
            end
        end
        CRC32_TABLE[i] = crc
    end

    -- CRC64 uses 64-bit polynomial 0xC96C5795D7870F42
    -- We store as {high32, low32} pairs
    for i = 0, 255 do
        local lo = i
        local hi = 0
        for _ = 1, 8 do
            if bit32.band(lo, 1) == 1 then
                local newlo = bit32.bxor(bit32.rshift(lo, 1), bit32.lshift(hi, 31))
                local newhi = bit32.rshift(hi, 1)
                lo = bit32.bxor(newlo, 0xD7870F42)
                hi = bit32.bxor(newhi, 0xC96C5795)
            else
                local newlo = bit32.bxor(bit32.rshift(lo, 1), bit32.lshift(hi, 31))
                local newhi = bit32.rshift(hi, 1)
                lo = newlo
                hi = newhi
            end
        end
        CRC64_TABLE[i] = { hi, lo }
    end
end

local function crc32(data: string, init: number?): number
    local crc = bit32.bxor(init or 0, 0xFFFFFFFF)
    for i = 1, #data do
        local byte = string.byte(data, i)
        crc = bit32.bxor(bit32.rshift(crc, 8), CRC32_TABLE[bit32.band(bit32.bxor(crc, byte), 0xFF)])
    end
    return bit32.bxor(crc, 0xFFFFFFFF)
end

local function crc32_buf(buf: buffer, offset: number, len: number): number
    local crc = 0xFFFFFFFF
    for i = 0, len - 1 do
        local byte = buffer.readu8(buf, offset + i)
        crc = bit32.bxor(bit32.rshift(crc, 8), CRC32_TABLE[bit32.band(bit32.bxor(crc, byte), 0xFF)])
    end
    return bit32.bxor(crc, 0xFFFFFFFF)
end

local function crc64(data: string): (number, number)
    local lo = 0xFFFFFFFF
    local hi = 0xFFFFFFFF
    for i = 1, #data do
        local byte = string.byte(data, i)
        local idx = bit32.band(bit32.bxor(lo, byte), 0xFF)
        local entry = CRC64_TABLE[idx]
        local newlo = bit32.bxor(bit32.bxor(bit32.rshift(lo, 8), bit32.lshift(hi, 24)), entry[2])
        local newhi = bit32.bxor(bit32.rshift(hi, 8), entry[1])
        lo = newlo
        hi = newhi
    end
    return bit32.bxor(hi, 0xFFFFFFFF), bit32.bxor(lo, 0xFFFFFFFF)
end

local function crc64_bytes(data: string): string
    local hi, lo = crc64(data)
    return string.pack("<I4I4", lo, hi)
end

--------------------------------------------------------------------------------
-- Bit reader for LZMA range decoding
--------------------------------------------------------------------------------

local RC_TOP = 0x1000000  -- 1 << 24
local RC_BIT_MODEL_TOTAL = 2048  -- 1 << 11
local RC_MOVE_BITS = 5

--------------------------------------------------------------------------------
-- LZMA Decoder
--------------------------------------------------------------------------------

local LZMA_NUM_REPS = 4
local LZMA_NUM_STATES = 12
local LZMA_NUM_LIT_STATES = 7
local LZMA_NUM_POS_BITS_MAX = 4
local LZMA_NUM_POS_STATES_MAX = 16  -- 1 << 4
local LZMA_MATCH_LEN_MIN = 2
local LZMA_NUM_ALIGN_BITS = 4
local LZMA_NUM_FULL_DISTANCES = 128  -- 1 << (kNumPosSlotBits + kEndPosModelIndex / 2)
local LZMA_END_POS_MODEL_INDEX = 14
local LZMA_NUM_POS_SLOT_BITS = 6
local LZMA_NUM_LEN_TO_POS_STATES = 4

local function create_probs(size: number): { number }
    local t = table.create(size, 1024)
    for i = 1, size do
        t[i] = 1024  -- RC_BIT_MODEL_TOTAL / 2
    end
    return t
end

local function lzma_state_update_literal(state: number): number
    if state < 4 then return 0
    elseif state < 10 then return state - 3
    else return state - 6 end
end

local function lzma_state_update_match(state: number): number
    if state < 7 then return 7 else return 10 end
end

local function lzma_state_update_rep(state: number): number
    if state < 7 then return 8 else return 11 end
end

local function lzma_state_update_short_rep(state: number): number
    if state < 7 then return 9 else return 11 end
end


--------------------------------------------------------------------------------
-- LZMA2 Decoder
--------------------------------------------------------------------------------

local function lzma2_decode(input: string, input_offset: number, expected_uncompressed_size: number, dict_size: number): string
    local pos = input_offset
    local output_parts = {}
    local total_output = 0

    -- Persistent LZMA state across chunks
    local lc = 3
    local lp = 0
    local pb = 2
    local pos_mask = bit32.lshift(1, pb) - 1
    local lit_pos_mask = bit32.lshift(1, lp) - 1

    -- Dictionary
    local dict_buf = buffer.create(dict_size)
    local dict_pos = 0
    local dict_total_written = 0

    -- Cumulative uncompressed position (for pos_state/lit_state calculations)
    local uncompressed_pos = 0

    -- Probability tables (persistent across non-reset chunks)
    local is_match, is_rep, is_rep_g0, is_rep_g1, is_rep_g2, is_rep0_long
    local pos_slot_probs, pos_special, pos_align
    local lit_probs
    local len_choice, len_choice2, len_low, len_mid, len_high
    local rep_len_choice, rep_len_choice2, rep_len_low, rep_len_mid, rep_len_high
    local state, rep0, rep1, rep2, rep3

    -- Range decoder state (shared across decode operations within a chunk)
    local rc_range, rc_code, rc_inp_pos

    local function reset_lzma_state()
        is_match = create_probs(LZMA_NUM_STATES * LZMA_NUM_POS_STATES_MAX)
        is_rep = create_probs(LZMA_NUM_STATES)
        is_rep_g0 = create_probs(LZMA_NUM_STATES)
        is_rep_g1 = create_probs(LZMA_NUM_STATES)
        is_rep_g2 = create_probs(LZMA_NUM_STATES)
        is_rep0_long = create_probs(LZMA_NUM_STATES * LZMA_NUM_POS_STATES_MAX)
        pos_slot_probs = {}
        for i = 0, LZMA_NUM_LEN_TO_POS_STATES - 1 do
            pos_slot_probs[i] = create_probs(64)
        end
        pos_special = create_probs(LZMA_NUM_FULL_DISTANCES - LZMA_END_POS_MODEL_INDEX)
        pos_align = create_probs(16)
        local num_lit_probs = 768 * bit32.lshift(1, lc + lp)
        lit_probs = create_probs(num_lit_probs)
        len_choice = create_probs(1)
        len_choice2 = create_probs(1)
        len_low = {}
        len_mid = {}
        for i = 0, LZMA_NUM_POS_STATES_MAX - 1 do
            len_low[i] = create_probs(8)
            len_mid[i] = create_probs(8)
        end
        len_high = create_probs(256)
        rep_len_choice = create_probs(1)
        rep_len_choice2 = create_probs(1)
        rep_len_low = {}
        rep_len_mid = {}
        for i = 0, LZMA_NUM_POS_STATES_MAX - 1 do
            rep_len_low[i] = create_probs(8)
            rep_len_mid[i] = create_probs(8)
        end
        rep_len_high = create_probs(256)
        state = 0
        rep0 = 0
        rep1 = 0
        rep2 = 0
        rep3 = 0
    end

    reset_lzma_state()

    -- Dict helpers
    local function dict_get(dist: number): number
        return buffer.readu8(dict_buf, (dict_pos - dist - 1) % dict_size)
    end

    local function dict_put_byte(byte: number)
        buffer.writeu8(dict_buf, dict_pos, byte)
        dict_pos = dict_pos + 1
        if dict_pos >= dict_size then
            dict_pos = 0
        end
        dict_total_written = dict_total_written + 1
    end

    -- Range decoder helpers (defined once, use rc_range/rc_code/rc_inp_pos upvalues)
    local function normalize()
        if rc_range < RC_TOP then
            rc_range = bit32.lshift(rc_range, 8)
            rc_code = bit32.lshift(rc_code, 8) + string.byte(input, rc_inp_pos)
            rc_inp_pos = rc_inp_pos + 1
        end
    end

    local function decode_bit(probs, idx): number
        normalize()
        local prob = probs[idx]
        local bound = bit32.rshift(rc_range, 11) * prob
        if rc_code < bound then
            rc_range = bound
            probs[idx] = prob + bit32.rshift(2048 - prob, 5)
            return 0
        else
            rc_range = rc_range - bound
            rc_code = rc_code - bound
            probs[idx] = prob - bit32.rshift(prob, 5)
            return 1
        end
    end

    local function decode_bit_tree(probs, num_bits): number
        local m = 1
        for _ = 1, num_bits do
            m = m * 2 + decode_bit(probs, m + 1)
        end
        return m - bit32.lshift(1, num_bits)
    end

    local function decode_bit_tree_reverse(probs, offset, num_bits): number
        local m = 1
        local result = 0
        for i = 0, num_bits - 1 do
            local b = decode_bit(probs, offset + m)
            m = m * 2 + b
            result = result + bit32.lshift(b, i)
        end
        return result
    end

    local function decode_len(choice, choice2, low, mid, high, ps): number
        if decode_bit(choice, 1) == 0 then
            return decode_bit_tree(low[ps], 3)
        elseif decode_bit(choice2, 1) == 0 then
            return 8 + decode_bit_tree(mid[ps], 3)
        else
            return 16 + decode_bit_tree(high, 8)
        end
    end

    while true do
        local control = string.byte(input, pos)
        pos = pos + 1

        if control == 0 then
            break
        end

        if control == 1 or control == 2 then
            if control == 1 then
                dict_pos = 0
                dict_total_written = 0
            end
            local unpack_size = bit32.lshift(string.byte(input, pos), 8) + string.byte(input, pos + 1) + 1
            pos = pos + 2
            local chunk = input:sub(pos, pos + unpack_size - 1)
            pos = pos + unpack_size
            table.insert(output_parts, chunk)
            for i = 1, #chunk do
                dict_put_byte(string.byte(chunk, i))
            end
            total_output = total_output + unpack_size
            uncompressed_pos = uncompressed_pos + unpack_size
        elseif control >= 0x80 then
            local unpack_high = bit32.band(control, 0x1F)
            local do_reset_dict = control >= 0xE0
            local do_reset_state = control >= 0xA0
            local do_new_props = control >= 0xC0

            local unpack_size = bit32.lshift(unpack_high, 16) + bit32.lshift(string.byte(input, pos), 8) + string.byte(input, pos + 1) + 1
            pos = pos + 2
            local pack_size = bit32.lshift(string.byte(input, pos), 8) + string.byte(input, pos + 1) + 1
            pos = pos + 2

            if do_new_props then
                local props_byte = string.byte(input, pos)
                pos = pos + 1
                lc = props_byte % 9
                local rem = math.floor(props_byte / 9)
                lp = rem % 5
                pb = math.floor(rem / 5)
                pos_mask = bit32.lshift(1, pb) - 1
                lit_pos_mask = bit32.lshift(1, lp) - 1
            end

            if do_reset_dict then
                dict_pos = 0
                dict_total_written = 0
                uncompressed_pos = 0
            end

            if do_reset_state then
                reset_lzma_state()
                uncompressed_pos = 0
            end

            -- Initialize range decoder for this chunk
            rc_range = 0xFFFFFFFF
            rc_code = 0
            rc_inp_pos = pos + 1  -- skip leading 0 byte
            for _ = 1, 4 do
                rc_code = bit32.lshift(rc_code, 8) + string.byte(input, rc_inp_pos)
                rc_inp_pos = rc_inp_pos + 1
            end


            -- Allocate output buffer for this chunk
            local output_buf = buffer.create(unpack_size)
            local output_pos = 0

            -- Decode this LZMA chunk
            while output_pos < unpack_size do
                local total_pos = uncompressed_pos + output_pos
                local cur_pos_state = bit32.band(total_pos, pos_mask)
                local sidx = state * LZMA_NUM_POS_STATES_MAX + cur_pos_state

                local is_match_bit = decode_bit(is_match, sidx + 1)
                if is_match_bit == 0 then
                    local prev_byte = 0
                    if dict_total_written + output_pos > 0 then
                        prev_byte = dict_get(0)
                    end
                    local lit_st = bit32.lshift(bit32.band(total_pos, lit_pos_mask), lc) + bit32.rshift(prev_byte, 8 - lc)
                    local po = lit_st * 768

                    local symbol
                    if state >= LZMA_NUM_LIT_STATES then
                        local match_byte = dict_get(rep0)
                        symbol = 1
                        repeat
                            local mb = bit32.band(bit32.rshift(match_byte, 7), 1)
                            match_byte = bit32.band(bit32.lshift(match_byte, 1), 0xFF)
                            local b = decode_bit(lit_probs, po + 256 * (1 + mb) + symbol + 1)
                            symbol = symbol * 2 + b
                            if mb ~= b then break end
                        until symbol >= 256
                        while symbol < 256 do
                            symbol = symbol * 2 + decode_bit(lit_probs, po + symbol + 1)
                        end
                    else
                        symbol = 1
                        while symbol < 256 do
                            symbol = symbol * 2 + decode_bit(lit_probs, po + symbol + 1)
                        end
                    end

                    local byte = bit32.band(symbol, 0xFF)
                    buffer.writeu8(output_buf, output_pos, byte)
                    output_pos = output_pos + 1
                    dict_put_byte(byte)
                    state = lzma_state_update_literal(state)
                else
                    local len
                    if decode_bit(is_rep, state + 1) == 0 then
                        len = decode_len(len_choice, len_choice2, len_low, len_mid, len_high, cur_pos_state)
                        state = lzma_state_update_match(state)

                        local ls = len
                        if ls >= LZMA_NUM_LEN_TO_POS_STATES then
                            ls = LZMA_NUM_LEN_TO_POS_STATES - 1
                        end

                        local psv = decode_bit_tree(pos_slot_probs[ls], LZMA_NUM_POS_SLOT_BITS)

                        if psv >= 4 then
                            local ndb = bit32.rshift(psv, 1) - 1
                            local dist = bit32.lshift(bit32.band(psv, 1) + 2, ndb)

                            if psv < LZMA_END_POS_MODEL_INDEX then
                                dist = dist + decode_bit_tree_reverse(pos_special, dist - psv, ndb)
                            else
                                local nf = ndb - LZMA_NUM_ALIGN_BITS
                                local fv = 0
                                for j = nf - 1, 0, -1 do
                                    normalize()
                                    rc_range = bit32.rshift(rc_range, 1)
                                    rc_code = rc_code - rc_range
                                    local t = -bit32.rshift(rc_code, 31)
                                    local bv = bit32.band(bit32.bxor(t, -1), 1)
                                    rc_code = rc_code + bit32.band(rc_range, t)
                                    fv = fv + bit32.lshift(bv, j)
                                end
                                dist = dist + bit32.lshift(fv, LZMA_NUM_ALIGN_BITS)
                                dist = dist + decode_bit_tree_reverse(pos_align, 1, LZMA_NUM_ALIGN_BITS)
                            end
                            rep3 = rep2
                            rep2 = rep1
                            rep1 = rep0
                            rep0 = dist
                        else
                            rep3 = rep2
                            rep2 = rep1
                            rep1 = rep0
                            rep0 = psv
                        end
                    else
                        if decode_bit(is_rep_g0, state + 1) == 0 then
                            if decode_bit(is_rep0_long, sidx + 1) == 0 then
                                state = lzma_state_update_short_rep(state)
                                local byte = dict_get(rep0)
                                buffer.writeu8(output_buf, output_pos, byte)
                                output_pos = output_pos + 1
                                dict_put_byte(byte)
                                continue
                            end
                        else
                            local tmp
                            if decode_bit(is_rep_g1, state + 1) == 0 then
                                tmp = rep1
                            else
                                if decode_bit(is_rep_g2, state + 1) == 0 then
                                    tmp = rep2
                                else
                                    tmp = rep3
                                    rep3 = rep2
                                end
                                rep2 = rep1
                            end
                            rep1 = rep0
                            rep0 = tmp
                        end

                        len = decode_len(rep_len_choice, rep_len_choice2, rep_len_low, rep_len_mid, rep_len_high, cur_pos_state)
                        state = lzma_state_update_rep(state)
                    end

                    local actual_len = len + LZMA_MATCH_LEN_MIN
                    local remaining = unpack_size - output_pos
                    if actual_len > remaining then
                        actual_len = remaining
                    end
                    for _ = 1, actual_len do
                        local byte = dict_get(rep0)
                        buffer.writeu8(output_buf, output_pos, byte)
                        output_pos = output_pos + 1
                        dict_put_byte(byte)
                    end
                end
            end

            pos = pos + pack_size
            table.insert(output_parts, buffer.tostring(output_buf))
            total_output = total_output + unpack_size
            uncompressed_pos = uncompressed_pos + unpack_size
        else
            error("Invalid LZMA2 control byte: " .. control)
        end
    end

    return table.concat(output_parts)
end

--------------------------------------------------------------------------------
-- XZ Stream Decoder
--------------------------------------------------------------------------------

local XZ_MAGIC = "\xFD7zXZ\x00"
local XZ_FOOTER_MAGIC = "YZ"

local function read_multibyte(data: string, pos: number): (number, number)
    local val = 0
    local shift = 0
    while true do
        local byte = string.byte(data, pos)
        pos = pos + 1
        val = val + bit32.lshift(bit32.band(byte, 0x7F), shift)
        if bit32.band(byte, 0x80) == 0 then
            break
        end
        shift = shift + 7
    end
    return val, pos
end

local function xz_decompress(input: string): string
    local pos = 1

    -- Stream Header (12 bytes)
    local magic = input:sub(pos, pos + 5)
    assert(magic == XZ_MAGIC, "Invalid XZ magic")
    pos = pos + 6

    -- Stream flags
    local flag1 = string.byte(input, pos)
    local flag2 = string.byte(input, pos + 1)
    assert(flag1 == 0, "Invalid stream flag byte 1")
    local check_type = bit32.band(flag2, 0x0F)
    pos = pos + 2

    -- CRC32 of stream flags
    local flags_crc = string.unpack("<I4", input, pos)
    pos = pos + 4

    local output_parts = {}
    local block_sizes = {}
    local block_uncompressed_sizes = {}

    -- Decode blocks
    while true do
        -- Peek at block header size byte
        local header_size_byte = string.byte(input, pos)
        if header_size_byte == 0 then
            -- Index indicator
            pos = pos + 1
            break
        end

        local block_header_size = (header_size_byte + 1) * 4
        local block_header_start = pos
        pos = pos + 1

        -- Block flags
        local block_flags = string.byte(input, pos)
        pos = pos + 1
        local num_filters = bit32.band(block_flags, 3) + 1
        local has_compressed_size = bit32.band(block_flags, 0x40) ~= 0
        local has_uncompressed_size = bit32.band(block_flags, 0x80) ~= 0

        local compressed_size_field, uncompressed_size_field

        if has_compressed_size then
            compressed_size_field, pos = read_multibyte(input, pos)
        end
        if has_uncompressed_size then
            uncompressed_size_field, pos = read_multibyte(input, pos)
        end

        -- Filters
        local filters = {}
        for i = 1, num_filters do
            local filter_id
            filter_id, pos = read_multibyte(input, pos)
            local props_size
            props_size, pos = read_multibyte(input, pos)
            local filter_props = input:sub(pos, pos + props_size - 1)
            pos = pos + props_size
            table.insert(filters, { id = filter_id, props = filter_props })
        end

        -- Skip padding to 4-byte alignment
        local header_data_size = pos - block_header_start
        local padding_needed = block_header_size - header_data_size - 4  -- -4 for CRC32
        pos = pos + padding_needed

        -- Block header CRC32
        local block_header_crc = string.unpack("<I4", input, pos)
        pos = pos + 4

        -- Compressed data
        local data_start = pos
        local dict_size = 0

        -- Parse LZMA2 filter properties
        for _, filter in ipairs(filters) do
            if filter.id == 0x21 then  -- LZMA2
                local prop_byte = string.byte(filter.props, 1)
                if prop_byte >= 40 then
                    dict_size = 0xFFFFFFFF
                else
                    local mantissa = bit32.band(prop_byte, 1) + 2
                    local exponent = bit32.rshift(prop_byte, 1) + 11
                    dict_size = bit32.lshift(mantissa, exponent)
                end
            end
        end

        -- Determine how much compressed data there is
        local comp_size
        if has_compressed_size then
            comp_size = compressed_size_field
        else
            -- We need to find the end by decoding LZMA2 chunks
            -- For now, compute from block size
            -- Actually we'll decode and track how much we consumed
            comp_size = nil
        end

        local uncompressed_size = uncompressed_size_field

        -- Decode LZMA2
        local decoded = lzma2_decode(input, pos, uncompressed_size or 0, dict_size)
        table.insert(output_parts, decoded)

        -- Advance past compressed data
        if comp_size then
            pos = pos + comp_size
        else
            -- Need to scan LZMA2 to find end
            local scan_pos = pos
            while true do
                local ctrl = string.byte(input, scan_pos)
                scan_pos = scan_pos + 1
                if ctrl == 0 then break end
                if ctrl == 1 or ctrl == 2 then
                    local sz = bit32.lshift(string.byte(input, scan_pos), 8) + string.byte(input, scan_pos + 1) + 1
                    scan_pos = scan_pos + 2 + sz
                elseif ctrl >= 0x80 then
                    scan_pos = scan_pos + 2  -- unpack size
                    local psz = bit32.lshift(string.byte(input, scan_pos), 8) + string.byte(input, scan_pos + 1) + 1
                    scan_pos = scan_pos + 2
                    if ctrl >= 0xC0 then
                        scan_pos = scan_pos + 1  -- props byte
                    end
                    scan_pos = scan_pos + psz
                end
            end
            comp_size = scan_pos - pos
            pos = scan_pos
        end

        -- Padding to 4-byte alignment after compressed data
        local block_data_size = pos - data_start
        local pad = (4 - (block_data_size % 4)) % 4
        pos = pos + pad

        -- Check value
        if check_type == 0x04 then
            -- CRC64 (8 bytes)
            pos = pos + 8
        elseif check_type == 0x01 then
            -- CRC32 (4 bytes)
            pos = pos + 4
        elseif check_type == 0x0A then
            -- SHA-256 (32 bytes)
            pos = pos + 32
        end

        table.insert(block_sizes, comp_size)
        table.insert(block_uncompressed_sizes, #decoded)
    end

    -- Index
    local num_records
    num_records, pos = read_multibyte(input, pos)

    for _ = 1, num_records do
        local _unpadded, _uncompressed
        _unpadded, pos = read_multibyte(input, pos)
        _uncompressed, pos = read_multibyte(input, pos)
    end

    -- Index padding
    local index_padding = (4 - ((pos - 1) % 4)) % 4
    -- Actually the index starts at the 0x00 byte we already consumed
    -- Index padding to 4-byte multiple of total index size
    -- Let's just skip to 4-byte alignment
    while (pos - 1) % 4 ~= 0 do
        pos = pos + 1
    end

    -- Index CRC32
    pos = pos + 4

    -- Stream footer (12 bytes)
    -- CRC32 (4) + Backward Size (4) + Stream Flags (2) + Footer Magic (2)
    pos = pos + 12

    return table.concat(output_parts)
end

--------------------------------------------------------------------------------
-- LZMA Encoder
--------------------------------------------------------------------------------

local MATCH_LEN_MIN = 2
local MATCH_LEN_MAX = 273  -- 2 + 271

-- Range encoder using 64-bit low via Luau doubles (safe up to 2^53)
local function range_encoder_create()
    return {
        low = 0,       -- full 64-bit value stored as double (exact up to 2^53)
        range = 0xFFFFFFFF,
        cache = 0,
        cache_size = 1,
        output = {},
    }
end

local function rc_shift_low(rc)
    -- Extract byte at bit position 32 (i.e. floor(low / 2^32))
    local low_hi = math.floor(rc.low / 0x100000000)
    -- Extract bits 24-31 of the lower 32 bits
    local low32 = rc.low - low_hi * 0x100000000
    local top_byte = math.floor(low32 / 0x1000000)

    if top_byte ~= 0xFF or low_hi ~= 0 then
        -- Flush cache
        local byte_out = rc.cache + low_hi
        table.insert(rc.output, string.char(bit32.band(byte_out, 0xFF)))
        local fill = bit32.band(0xFF + low_hi, 0xFF)
        for _ = 1, rc.cache_size - 1 do
            table.insert(rc.output, string.char(fill))
        end
        rc.cache = top_byte
        rc.cache_size = 0
    end
    rc.cache_size = rc.cache_size + 1
    -- low = (low & 0x00FFFFFF) << 8
    rc.low = bit32.lshift(bit32.band(low32, 0x00FFFFFF), 8)
end

local function rc_encode_bit(rc, probs: { number }, idx: number, bit: number)
    local prob = probs[idx]
    local bound = bit32.rshift(rc.range, 11) * prob
    if bit == 0 then
        rc.range = bound
        probs[idx] = prob + bit32.rshift(RC_BIT_MODEL_TOTAL - prob, RC_MOVE_BITS)
    else
        rc.range = rc.range - bound
        rc.low = rc.low + bound
        probs[idx] = prob - bit32.rshift(prob, RC_MOVE_BITS)
    end
    if rc.range < RC_TOP then
        rc.range = bit32.lshift(rc.range, 8)
        rc_shift_low(rc)
    end
end

local function rc_encode_bit_tree(rc, probs: { number }, num_bits: number, val: number)
    local m = 1
    for i = num_bits - 1, 0, -1 do
        local b = bit32.band(bit32.rshift(val, i), 1)
        rc_encode_bit(rc, probs, m + 1, b)
        m = m * 2 + b
    end
end

local function rc_encode_bit_tree_reverse(rc, probs: { number }, offset: number, num_bits: number, val: number)
    local m = 1
    for i = 0, num_bits - 1 do
        local b = bit32.band(bit32.rshift(val, i), 1)
        rc_encode_bit(rc, probs, offset + m, b)
        m = m * 2 + b
    end
end

local function rc_encode_direct_bits(rc, val: number, num_bits: number)
    for i = num_bits - 1, 0, -1 do
        rc.range = bit32.rshift(rc.range, 1)
        local b = bit32.band(bit32.rshift(val, i), 1)
        if b == 1 then
            rc.low = rc.low + rc.range
        end
        if rc.range < RC_TOP then
            rc.range = bit32.lshift(rc.range, 8)
            rc_shift_low(rc)
        end
    end
end

local function rc_flush(rc)
    for _ = 1, 5 do
        rc_shift_low(rc)
    end
end

local function rc_get_output(rc): string
    return table.concat(rc.output)
end

-- Hash chain match finder
local HASH_SIZE = 1048576  -- 2^20
local HASH_MASK = HASH_SIZE - 1

local function hash3(buf: buffer, pos: number): number
    local b0 = buffer.readu8(buf, pos)
    local b1 = buffer.readu8(buf, pos + 1)
    local b2 = buffer.readu8(buf, pos + 2)
    return bit32.band(bit32.bxor(b0 * 256 + b1, b2 * 2654435761), HASH_MASK)
end

local function hash4(buf: buffer, pos: number): number
    local v = buffer.readu32(buf, pos)
    return bit32.band(bit32.bxor(v, bit32.rshift(v, 12)), HASH_MASK)
end

-- lzma_encode: encode bytes [start_offset, end_offset) from data_buf (0-indexed buffer).
-- enc_state: nil for fresh state, or a table returned by a previous call to continue.
-- Returns: (compressed_string, new_enc_state)
local function lzma_encode(data_buf: buffer, start_offset: number, end_offset: number, lc: number, lp: number, pb: number, dict_size: number, enc_state: any): (string, any)
    local data_len = buffer.len(data_buf)
    local chunk_len = end_offset - start_offset
    if chunk_len <= 0 then
        return "", enc_state
    end

    local pos_mask = bit32.lshift(1, pb) - 1
    local lit_pos_mask = bit32.lshift(1, lp) - 1
    local lc_val = lc

    -- Range encoder (always fresh per LZMA2 chunk)
    local rc = range_encoder_create()

    -- Prob tables and state: either fresh or carried over from enc_state
    local is_match, is_rep, is_rep_g0, is_rep_g1, is_rep_g2, is_rep0_long
    local pos_slot_enc
    local pos_special, pos_align
    local lit_probs
    local len_choice, len_choice2, len_low, len_mid, len_high
    local rep_len_choice, rep_len_choice2, rep_len_low, rep_len_mid, rep_len_high
    local state, rep0, rep1, rep2, rep3
    local hash_table, chain

    if enc_state ~= nil then
        -- Continue from previous state
        is_match    = enc_state.is_match
        is_rep      = enc_state.is_rep
        is_rep_g0   = enc_state.is_rep_g0
        is_rep_g1   = enc_state.is_rep_g1
        is_rep_g2   = enc_state.is_rep_g2
        is_rep0_long = enc_state.is_rep0_long
        pos_slot_enc = enc_state.pos_slot_enc
        pos_special = enc_state.pos_special
        pos_align   = enc_state.pos_align
        lit_probs   = enc_state.lit_probs
        len_choice  = enc_state.len_choice
        len_choice2 = enc_state.len_choice2
        len_low     = enc_state.len_low
        len_mid     = enc_state.len_mid
        len_high    = enc_state.len_high
        rep_len_choice  = enc_state.rep_len_choice
        rep_len_choice2 = enc_state.rep_len_choice2
        rep_len_low     = enc_state.rep_len_low
        rep_len_mid     = enc_state.rep_len_mid
        rep_len_high    = enc_state.rep_len_high
        state = enc_state.state
        rep0  = enc_state.rep0
        rep1  = enc_state.rep1
        rep2  = enc_state.rep2
        rep3  = enc_state.rep3
        hash_table = enc_state.hash_table
        chain      = enc_state.chain
        -- Extend chain if the full buffer is longer now
        while #chain < data_len do
            table.insert(chain, -1)
        end
    else
        -- Fresh state
        is_match    = create_probs(LZMA_NUM_STATES * LZMA_NUM_POS_STATES_MAX)
        is_rep      = create_probs(LZMA_NUM_STATES)
        is_rep_g0   = create_probs(LZMA_NUM_STATES)
        is_rep_g1   = create_probs(LZMA_NUM_STATES)
        is_rep_g2   = create_probs(LZMA_NUM_STATES)
        is_rep0_long = create_probs(LZMA_NUM_STATES * LZMA_NUM_POS_STATES_MAX)

        pos_slot_enc = {}
        for i = 0, LZMA_NUM_LEN_TO_POS_STATES - 1 do
            pos_slot_enc[i] = create_probs(64)
        end

        pos_special = create_probs(LZMA_NUM_FULL_DISTANCES - LZMA_END_POS_MODEL_INDEX)
        pos_align   = create_probs(16)

        local num_lit_probs = 768 * bit32.lshift(1, lc + lp)
        lit_probs = create_probs(num_lit_probs)

        len_choice  = create_probs(1)
        len_choice2 = create_probs(1)
        len_low  = {}
        len_mid  = {}
        for i = 0, LZMA_NUM_POS_STATES_MAX - 1 do
            len_low[i] = create_probs(8)
            len_mid[i] = create_probs(8)
        end
        len_high = create_probs(256)

        rep_len_choice  = create_probs(1)
        rep_len_choice2 = create_probs(1)
        rep_len_low  = {}
        rep_len_mid  = {}
        for i = 0, LZMA_NUM_POS_STATES_MAX - 1 do
            rep_len_low[i]  = create_probs(8)
            rep_len_mid[i]  = create_probs(8)
        end
        rep_len_high = create_probs(256)

        state = 0
        rep0  = 0
        rep1  = 0
        rep2  = 0
        rep3  = 0

        hash_table = table.create(HASH_SIZE, -1)
        for i = 1, HASH_SIZE do
            hash_table[i] = -1
        end
        chain = table.create(data_len, -1)
        for i = 1, data_len do
            chain[i] = -1
        end
    end

    local MAX_CHAIN_LEN = 1024
    local NICE_LEN = 273

    -- Encode literal
    local function encode_literal(pos: number, byte: number, prev_byte: number)
        local lit_state = bit32.lshift(bit32.band(pos, lit_pos_mask), lc_val) + bit32.rshift(prev_byte, 8 - lc_val)
        local probs_offset = lit_state * 768

        if state >= LZMA_NUM_LIT_STATES then
            local match_byte = buffer.readu8(data_buf, pos - rep0 - 1)
            local symbol = 1
            local context = byte
            for i = 7, 0, -1 do
                local match_bit = bit32.band(bit32.rshift(match_byte, i), 1)
                local cur_bit = bit32.band(bit32.rshift(context, i), 1)
                local prob_idx = probs_offset + 256 * (1 + match_bit) + symbol + 1
                rc_encode_bit(rc, lit_probs, prob_idx, cur_bit)
                symbol = symbol * 2 + cur_bit
                if match_bit ~= cur_bit then
                    -- Encode remaining bits normally
                    for j = i - 1, 0, -1 do
                        local b = bit32.band(bit32.rshift(context, j), 1)
                        rc_encode_bit(rc, lit_probs, probs_offset + symbol + 1, b)
                        symbol = symbol * 2 + b
                    end
                    break
                end
            end
        else
            local symbol = 1
            for i = 7, 0, -1 do
                local cur_bit = bit32.band(bit32.rshift(byte, i), 1)
                rc_encode_bit(rc, lit_probs, probs_offset + symbol + 1, cur_bit)
                symbol = symbol * 2 + cur_bit
            end
        end
    end

    -- Encode length
    local function encode_length(choice, choice2, low, mid, high, pos_state: number, len: number)
        if len < 8 then
            rc_encode_bit(rc, choice, 1, 0)
            rc_encode_bit_tree(rc, low[pos_state], 3, len)
        elseif len < 16 then
            rc_encode_bit(rc, choice, 1, 1)
            rc_encode_bit(rc, choice2, 1, 0)
            rc_encode_bit_tree(rc, mid[pos_state], 3, len - 8)
        else
            rc_encode_bit(rc, choice, 1, 1)
            rc_encode_bit(rc, choice2, 1, 1)
            rc_encode_bit_tree(rc, high, 8, len - 16)
        end
    end

    -- Encode distance
    local function encode_distance(dist: number, len: number)
        local len_state = len
        if len_state >= LZMA_NUM_LEN_TO_POS_STATES then
            len_state = LZMA_NUM_LEN_TO_POS_STATES - 1
        end

        -- Find pos_slot
        local pos_slot_val
        if dist < 4 then
            pos_slot_val = dist
        else
            local bsr = 0
            local tmp = dist
            while tmp >= 2 do
                tmp = bit32.rshift(tmp, 1)
                bsr = bsr + 1
            end
            pos_slot_val = bsr * 2 + bit32.band(bit32.rshift(dist, bsr - 1), 1)
        end

        rc_encode_bit_tree(rc, pos_slot_enc[len_state], LZMA_NUM_POS_SLOT_BITS, pos_slot_val)

        if pos_slot_val >= 4 then
            local num_direct_bits = bit32.rshift(pos_slot_val, 1) - 1
            local base = bit32.lshift(bit32.band(pos_slot_val, 1) + 2, num_direct_bits)
            local dist_reduced = dist - base

            if pos_slot_val < LZMA_END_POS_MODEL_INDEX then
                local offset = base - pos_slot_val
                rc_encode_bit_tree_reverse(rc, pos_special, offset, num_direct_bits, dist_reduced)
            else
                local num_fixed = num_direct_bits - LZMA_NUM_ALIGN_BITS
                rc_encode_direct_bits(rc, bit32.rshift(dist_reduced, LZMA_NUM_ALIGN_BITS), num_fixed)
                rc_encode_bit_tree_reverse(rc, pos_align, 1, LZMA_NUM_ALIGN_BITS, bit32.band(dist_reduced, 0xF))
            end
        end
    end

    -- Hash chain match finder for the optimal parser.
    -- Returns a list of {len, dist} pairs for all distinct-distance best matches,
    -- and separately updates the hash chain for position `pos`.
    -- The rep distances must be passed in so we can check them without relying on globals.
    local function find_matches_at(pos: number, r0: number, r1: number, r2: number, r3: number): { { number } }
        local results = {}
        local max_len = math.min(MATCH_LEN_MAX, end_offset - pos)

        if max_len < MATCH_LEN_MIN then
            -- Not enough lookahead; just update hash and return nothing
            if pos + 3 < end_offset then
                local h = hash4(data_buf, pos)
                chain[pos + 1] = hash_table[h + 1]
                hash_table[h + 1] = pos
            end
            return results
        end

        -- Check the 4 rep distances
        local best_rep_len = 0
        for ri, rep_dist in ipairs({ r0, r1, r2, r3 }) do
            if rep_dist < pos and rep_dist < dict_size then
                local len = 0
                local base = pos - rep_dist - 1
                local rep_max = math.min(max_len, buffer.len(data_buf) - base)
                while len < rep_max and buffer.readu8(data_buf, pos + len) == buffer.readu8(data_buf, base + len) do
                    len = len + 1
                end
                if len >= MATCH_LEN_MIN then
                    -- rep index: negative values -1..-4 mean rep0..rep3
                    table.insert(results, { len, -(ri) })
                    if len > best_rep_len then best_rep_len = len end
                end
            end
        end

        -- Hash chain search (requires at least 4 bytes for hash4)
        if pos + 3 < end_offset then
            local h = hash4(data_buf, pos)
            local cur = hash_table[h + 1]
            chain[pos + 1] = cur
            hash_table[h + 1] = pos

            -- We track the longest match at each chain position.
            -- Only add to results when we find a strictly longer match.
            -- The DP will enumerate all useful lengths (2..mlen) for each match.
            if best_rep_len < NICE_LEN then
                local chain_count = 0
                local chain_best = best_rep_len
                while cur >= 0 and chain_count < MAX_CHAIN_LEN do
                    local dist = pos - cur - 1
                    if dist >= dict_size then break end

                    local len = 0
                    local cur_max = math.min(max_len, buffer.len(data_buf) - cur)
                    while len < cur_max and buffer.readu8(data_buf, pos + len) == buffer.readu8(data_buf, cur + len) do
                        len = len + 1
                    end

                    if len >= MATCH_LEN_MIN and len > chain_best then
                        table.insert(results, { len, dist })
                        chain_best = len
                        if len >= NICE_LEN then break end
                    end

                    cur = chain[cur + 1]
                    chain_count = chain_count + 1
                end
            end
        end

        return results
    end

    -- Bit-cost tables (scaled by 1<<6 = 64 units per bit).
    -- price_table[prob] gives the cost of encoding a 0-bit with that probability.
    -- prob is in [1, 2047] (LZMA uses 1..2047).
    local PRICE_SCALE = 64  -- 1 unit = 1/64 bit
    local price_table = table.create(2049, 0)
    do
        -- cost of bit=0: -log2(prob/2048) * PRICE_SCALE
        -- cost of bit=1: -log2((2048-prob)/2048) * PRICE_SCALE
        -- We store cost_of_0 at index prob+1 (1-indexed).
        -- Total table: [0] unused, [1..2048] for prob 0..2047
        -- prob=0 and prob=2048 are degenerate; LZMA never reaches them.
        for p = 1, 2047 do
            local cost0 = -math.log(p / 2048) / math.log(2) * PRICE_SCALE
            price_table[p + 1] = math.floor(cost0 + 0.5)
        end
        price_table[1] = 64 * 15  -- prob=0: cost infinity, use large value
        price_table[2049] = 0      -- prob=2048: free (prob=1), shouldn't be used
    end

    -- Returns cost (in PRICE_SCALE units) of encoding a single bit with given prob array/idx
    local function bit_price(probs, idx, bit)
        local prob = probs[idx]
        if bit == 0 then
            return price_table[prob + 1]
        else
            return price_table[2048 - prob + 1]
        end
    end

    -- Cost of encoding a bit-tree of num_bits with the given value
    local function bit_tree_price(probs, num_bits, val)
        local total = 0
        local m = 1
        for i = num_bits - 1, 0, -1 do
            local b = bit32.band(bit32.rshift(val, i), 1)
            total = total + bit_price(probs, m + 1, b)
            m = m * 2 + b
        end
        return total
    end

    -- Cost of encoding a reverse bit-tree
    local function bit_tree_reverse_price(probs, offset, num_bits, val)
        local total = 0
        local m = 1
        for i = 0, num_bits - 1 do
            local b = bit32.band(bit32.rshift(val, i), 1)
            total = total + bit_price(probs, offset + m, b)
            m = m * 2 + b
        end
        return total
    end

    -- Cost of encoding length value `len` (raw, i.e. already subtracted MATCH_LEN_MIN)
    local function length_price(choice, choice2, low, mid, high, pos_state, len)
        if len < 8 then
            return bit_price(choice, 1, 0) + bit_tree_price(low[pos_state], 3, len)
        elseif len < 16 then
            return bit_price(choice, 1, 1) + bit_price(choice2, 1, 0)
                   + bit_tree_price(mid[pos_state], 3, len - 8)
        else
            return bit_price(choice, 1, 1) + bit_price(choice2, 1, 1)
                   + bit_tree_price(high, 8, len - 16)
        end
    end

    -- Cost of encoding a distance value `dist` with the given length (raw)
    local function distance_price(dist, len_raw)
        local len_state = len_raw
        if len_state >= LZMA_NUM_LEN_TO_POS_STATES then
            len_state = LZMA_NUM_LEN_TO_POS_STATES - 1
        end

        local pos_slot_val
        if dist < 4 then
            pos_slot_val = dist
        else
            local bsr = 0
            local tmp = dist
            while tmp >= 2 do
                tmp = bit32.rshift(tmp, 1)
                bsr = bsr + 1
            end
            pos_slot_val = bsr * 2 + bit32.band(bit32.rshift(dist, bsr - 1), 1)
        end

        local total = bit_tree_price(pos_slot_enc[len_state], LZMA_NUM_POS_SLOT_BITS, pos_slot_val)

        if pos_slot_val >= 4 then
            local num_direct_bits = bit32.rshift(pos_slot_val, 1) - 1
            local base = bit32.lshift(bit32.band(pos_slot_val, 1) + 2, num_direct_bits)
            local dist_reduced = dist - base

            if pos_slot_val < LZMA_END_POS_MODEL_INDEX then
                local offset = base - pos_slot_val
                total = total + bit_tree_reverse_price(pos_special, offset, num_direct_bits, dist_reduced)
            else
                local num_fixed = num_direct_bits - LZMA_NUM_ALIGN_BITS
                -- direct bits: each costs exactly 1 bit = PRICE_SCALE
                total = total + num_fixed * PRICE_SCALE
                total = total + bit_tree_reverse_price(pos_align, 1, LZMA_NUM_ALIGN_BITS,
                    bit32.band(dist_reduced, 0xF))
            end
        end

        return total
    end

    -- DP optimal parser
    -- We process the input in windows. Within each window we compute the optimal
    -- parse via forward DP, then encode the result.
    --
    -- opt_cost[i]  : min bit-cost (PRICE_SCALE units) to reach absolute position (window_start + i)
    -- opt_len[i]   : length of the token that ends at position (window_start + i); 0 = not set
    -- opt_type[i]  : type of token: 1=literal, 2=short_rep, 3=rep0, 4=rep1, 5=rep2, 6=rep3, 7=normal_match
    -- opt_dist[i]  : for type=7 (normal match): the match distance (>= 0)
    -- opt_state[i] : LZMA state at position (window_start + i)
    -- opt_r0/r1/r2/r3[i] : rep distances at position (window_start + i)

    local OPT_WINDOW = 4096  -- max DP window size
    local INF_COST = 1e18

    -- Token type constants
    local TOK_LIT       = 1
    local TOK_SHORT_REP = 2
    local TOK_REP0      = 3
    local TOK_REP1      = 4
    local TOK_REP2      = 5
    local TOK_REP3      = 6
    local TOK_MATCH     = 7

    -- Reusable opt arrays (1-indexed, index i corresponds to offset i-1 within window)
    local opt_cost  = table.create(OPT_WINDOW + MATCH_LEN_MAX + 2, INF_COST)
    local opt_len   = table.create(OPT_WINDOW + MATCH_LEN_MAX + 2, 0)
    local opt_type  = table.create(OPT_WINDOW + MATCH_LEN_MAX + 2, 0)
    local opt_dist  = table.create(OPT_WINDOW + MATCH_LEN_MAX + 2, 0)
    local opt_state = table.create(OPT_WINDOW + MATCH_LEN_MAX + 2, 0)
    local opt_r0    = table.create(OPT_WINDOW + MATCH_LEN_MAX + 2, 0)
    local opt_r1    = table.create(OPT_WINDOW + MATCH_LEN_MAX + 2, 0)
    local opt_r2    = table.create(OPT_WINDOW + MATCH_LEN_MAX + 2, 0)
    local opt_r3    = table.create(OPT_WINDOW + MATCH_LEN_MAX + 2, 0)

    local function literal_price(pos: number, cur_st: number, cur_r0: number, cur_r1: number, cur_r2: number, cur_r3: number)
        local ps = bit32.band(pos, pos_mask)
        local sidx = cur_st * LZMA_NUM_POS_STATES_MAX + ps
        local cost = bit_price(is_match, sidx + 1, 0)

        if pos >= end_offset then return cost + 8 * PRICE_SCALE end
        local cur_byte = buffer.readu8(data_buf, pos)
        local prev_byte = if pos > start_offset then buffer.readu8(data_buf, pos - 1) else 0
        local lit_st = bit32.lshift(bit32.band(pos, lit_pos_mask), lc_val) + bit32.rshift(prev_byte, 8 - lc_val)
        local po = lit_st * 768

        if cur_st >= LZMA_NUM_LIT_STATES and cur_r0 < pos then
            local match_byte = buffer.readu8(data_buf, pos - cur_r0 - 1)
            local symbol = 1
            for i = 7, 0, -1 do
                local match_bit = bit32.band(bit32.rshift(match_byte, i), 1)
                local cur_bit = bit32.band(bit32.rshift(cur_byte, i), 1)
                local prob_idx = po + 256 * (1 + match_bit) + symbol + 1
                cost = cost + bit_price(lit_probs, prob_idx, cur_bit)
                symbol = symbol * 2 + cur_bit
                if match_bit ~= cur_bit then
                    for j = i - 1, 0, -1 do
                        local b = bit32.band(bit32.rshift(cur_byte, j), 1)
                        cost = cost + bit_price(lit_probs, po + symbol + 1, b)
                        symbol = symbol * 2 + b
                    end
                    break
                end
            end
        else
            local symbol = 1
            for i = 7, 0, -1 do
                local b = bit32.band(bit32.rshift(cur_byte, i), 1)
                cost = cost + bit_price(lit_probs, po + symbol + 1, b)
                symbol = symbol * 2 + b
            end
        end
        return cost
    end

    -- Match price computation (state-aware)
    -- Returns cost of encoding a normal match (is_match=1, is_rep=0) at given state.
    local function normal_match_price(cur_st, pos_state, len_raw, dist)
        local sidx = cur_st * LZMA_NUM_POS_STATES_MAX + pos_state
        local cost = bit_price(is_match, sidx + 1, 1)
                   + bit_price(is_rep, cur_st + 1, 0)
                   + length_price(len_choice, len_choice2, len_low, len_mid, len_high, pos_state, len_raw)
                   + distance_price(dist, len_raw)
        return cost
    end

    -- Rep match price (rep index 0..3, len >= 2 for rep0; len >= 1 for short rep0)
    local function rep_match_price(cur_st, pos_state, rep_idx, len_raw)
        local sidx = cur_st * LZMA_NUM_POS_STATES_MAX + pos_state
        local cost = bit_price(is_match, sidx + 1, 1)
                   + bit_price(is_rep, cur_st + 1, 1)

        if rep_idx == 0 then
            cost = cost + bit_price(is_rep_g0, cur_st + 1, 0)
            if len_raw == 0 then
                -- short rep (len=1)
                cost = cost + bit_price(is_rep0_long, sidx + 1, 0)
            else
                cost = cost + bit_price(is_rep0_long, sidx + 1, 1)
                    + length_price(rep_len_choice, rep_len_choice2, rep_len_low, rep_len_mid, rep_len_high, pos_state, len_raw)
            end
        elseif rep_idx == 1 then
            cost = cost + bit_price(is_rep_g0, cur_st + 1, 1)
                       + bit_price(is_rep_g1, cur_st + 1, 0)
                       + length_price(rep_len_choice, rep_len_choice2, rep_len_low, rep_len_mid, rep_len_high, pos_state, len_raw)
        elseif rep_idx == 2 then
            cost = cost + bit_price(is_rep_g0, cur_st + 1, 1)
                       + bit_price(is_rep_g1, cur_st + 1, 1)
                       + bit_price(is_rep_g2, cur_st + 1, 0)
                       + length_price(rep_len_choice, rep_len_choice2, rep_len_low, rep_len_mid, rep_len_high, pos_state, len_raw)
        else -- rep_idx == 3
            cost = cost + bit_price(is_rep_g0, cur_st + 1, 1)
                       + bit_price(is_rep_g1, cur_st + 1, 1)
                       + bit_price(is_rep_g2, cur_st + 1, 1)
                       + length_price(rep_len_choice, rep_len_choice2, rep_len_low, rep_len_mid, rep_len_high, pos_state, len_raw)
        end
        return cost
    end

    -- State/rep transitions (mirroring encoder logic)
    local function next_state_lit(st) return lzma_state_update_literal(st) end
    local function next_state_match(st) return lzma_state_update_match(st) end
    local function next_state_rep(st) return lzma_state_update_rep(st) end
    local function next_state_short_rep(st) return lzma_state_update_short_rep(st) end

    -- Compute rep distances after a normal match
    local function reps_after_match(dist, r0, r1, r2, r3)
        return dist, r0, r1, r2
    end

    -- Compute rep distances after a rep match (rep_idx = 0..3)
    local function reps_after_rep(rep_idx, r0, r1, r2, r3)
        if rep_idx == 0 then
            return r0, r1, r2, r3  -- rep0 stays rep0
        elseif rep_idx == 1 then
            return r1, r0, r2, r3
        elseif rep_idx == 2 then
            return r2, r0, r1, r3
        else -- rep_idx == 3
            return r3, r0, r1, r2
        end
    end

    -- Encode a single token (literal or match) and advance position.
    -- Updates state/rep0-3 in-place (local variables).
    -- `token_type`: "lit", "short_rep", "rep", "match"
    -- `token_len`: actual length (not raw)
    -- `token_dist`: distance (for "match"), or rep index 0-3 (for "rep"/"short_rep")
    local function emit_token(pos: number, token_len: number, token_dist_or_rep: number, is_rep_match: boolean, is_short_rep: boolean)
        local pos_state = bit32.band(pos, pos_mask)
        local state_idx = state * LZMA_NUM_POS_STATES_MAX + pos_state

        if not is_rep_match and token_len == 1 and token_dist_or_rep == 0 then
            -- Literal
            rc_encode_bit(rc, is_match, state_idx + 1, 0)
            local cur_byte = buffer.readu8(data_buf, pos)
            local prev_byte = if pos > start_offset then buffer.readu8(data_buf, pos - 1) else 0
            encode_literal(pos, cur_byte, prev_byte)
            state = next_state_lit(state)
        elseif is_short_rep then
            -- Short rep (rep0, len=1)
            rc_encode_bit(rc, is_match, state_idx + 1, 1)
            rc_encode_bit(rc, is_rep, state + 1, 1)
            rc_encode_bit(rc, is_rep_g0, state + 1, 0)
            rc_encode_bit(rc, is_rep0_long, state_idx + 1, 0)
            state = next_state_short_rep(state)
            -- rep0 stays the same
        elseif is_rep_match then
            local rep_idx = token_dist_or_rep
            rc_encode_bit(rc, is_match, state_idx + 1, 1)
            rc_encode_bit(rc, is_rep, state + 1, 1)
            if rep_idx == 0 then
                rc_encode_bit(rc, is_rep_g0, state + 1, 0)
                rc_encode_bit(rc, is_rep0_long, state_idx + 1, 1)
            elseif rep_idx == 1 then
                rc_encode_bit(rc, is_rep_g0, state + 1, 1)
                rc_encode_bit(rc, is_rep_g1, state + 1, 0)
            elseif rep_idx == 2 then
                rc_encode_bit(rc, is_rep_g0, state + 1, 1)
                rc_encode_bit(rc, is_rep_g1, state + 1, 1)
                rc_encode_bit(rc, is_rep_g2, state + 1, 0)
            else -- rep_idx == 3
                rc_encode_bit(rc, is_rep_g0, state + 1, 1)
                rc_encode_bit(rc, is_rep_g1, state + 1, 1)
                rc_encode_bit(rc, is_rep_g2, state + 1, 1)
            end
            encode_length(rep_len_choice, rep_len_choice2, rep_len_low, rep_len_mid, rep_len_high, pos_state, token_len - MATCH_LEN_MIN)
            state = next_state_rep(state)
            -- Update reps
            if rep_idx == 1 then
                local tmp = rep1; rep1 = rep0; rep0 = tmp
            elseif rep_idx == 2 then
                local tmp = rep2; rep2 = rep1; rep1 = rep0; rep0 = tmp
            elseif rep_idx == 3 then
                local tmp = rep3; rep3 = rep2; rep2 = rep1; rep1 = rep0; rep0 = tmp
            end
            -- rep0 unchanged for rep_idx==0
        else
            -- Normal match
            local dist = token_dist_or_rep
            rc_encode_bit(rc, is_match, state_idx + 1, 1)
            rc_encode_bit(rc, is_rep, state + 1, 0)
            encode_length(len_choice, len_choice2, len_low, len_mid, len_high, pos_state, token_len - MATCH_LEN_MIN)
            encode_distance(dist, token_len - MATCH_LEN_MIN)
            rep3 = rep2; rep2 = rep1; rep1 = rep0; rep0 = dist
            state = next_state_match(state)
        end

        -- Note: hash chain updates for skipped positions are handled by the DP pass
        -- (find_matches_at is called on every position during forward DP), so we do
        -- not need to update them again here.
    end

    -- Main encoding loop using optimal (forward DP) parser
    local pos = start_offset

    while pos < end_offset do
        -- Determine the window end (we won't DP beyond this)
        local win_end = math.min(pos + OPT_WINDOW, end_offset)
        local win_size = win_end - pos  -- number of positions in the window

        -- Initialize DP arrays for this window
        -- We need indices 1..(win_size + MATCH_LEN_MAX) to allow matches to extend past win_end
        local max_reach = win_size + MATCH_LEN_MAX + 1
        for i = 1, max_reach do
            opt_cost[i] = INF_COST
            opt_len[i] = 0
            opt_type[i] = 0
        end
        opt_cost[1] = 0   -- cost to reach pos+0 is 0
        opt_state[1] = state
        opt_r0[1] = rep0
        opt_r1[1] = rep1
        opt_r2[1] = rep2
        opt_r3[1] = rep3

        -- Forward DP pass
        local last_reachable = 1  -- furthest index with finite cost
        for i = 1, win_size do
            if opt_cost[i] >= INF_COST then
                -- This position is unreachable; skip
                -- (Should not happen since we ensure connectivity via literals)
                continue
            end

            local abs_pos = pos + i - 1
            local cur_cost = opt_cost[i]
            local cur_st = opt_state[i]
            local cur_r0, cur_r1, cur_r2, cur_r3 = opt_r0[i], opt_r1[i], opt_r2[i], opt_r3[i]

            -- Option 1: literal
            local lit_cost = cur_cost + literal_price(abs_pos, cur_st, cur_r0, cur_r1, cur_r2, cur_r3)
            local ni = i + 1  -- next index
            if lit_cost < opt_cost[ni] then
                opt_cost[ni] = lit_cost
                opt_len[ni] = 1
                opt_type[ni] = TOK_LIT
                opt_dist[ni] = 0
                opt_state[ni] = next_state_lit(cur_st)
                opt_r0[ni] = cur_r0; opt_r1[ni] = cur_r1; opt_r2[ni] = cur_r2; opt_r3[ni] = cur_r3
                if ni > last_reachable then last_reachable = ni end
            end

            -- Option 2: short rep (rep0, len=1)
            -- rep0 is the distance, so the byte at abs_pos - rep0 - 1 should match abs_pos
            if abs_pos >= cur_r0 + 1 and abs_pos < end_offset then
                if buffer.readu8(data_buf, abs_pos) == buffer.readu8(data_buf, abs_pos - cur_r0 - 1) then
                    local ps = bit32.band(abs_pos, pos_mask)
                    local sr_cost = cur_cost + rep_match_price(cur_st, ps, 0, 0)
                    if sr_cost < opt_cost[ni] then
                        opt_cost[ni] = sr_cost
                        opt_len[ni] = 1
                        opt_type[ni] = TOK_SHORT_REP
                        opt_dist[ni] = 0
                        opt_state[ni] = next_state_short_rep(cur_st)
                        opt_r0[ni] = cur_r0; opt_r1[ni] = cur_r1; opt_r2[ni] = cur_r2; opt_r3[ni] = cur_r3
                        if ni > last_reachable then last_reachable = ni end
                    end
                end
            end

            -- Option 3: matches (rep or normal).
            -- find_matches_at updates the hash chain for abs_pos and returns all
            -- available matches (rep and normal). Works near end of input too
            -- (returns only rep matches when fewer than 4 bytes remain).
            do
                local matches = find_matches_at(abs_pos, cur_r0, cur_r1, cur_r2, cur_r3)
                local ps = bit32.band(abs_pos, pos_mask)

                for _, m in ipairs(matches) do
                    local mlen = m[1]
                    local mdist = m[2]  -- negative = rep index (-1=rep0, etc.), non-negative = dist

                    if mdist < 0 then
                        -- Rep match; mdist == -1 means rep0, -2 means rep1, etc.
                        local rep_idx = -mdist - 1  -- 0..3
                        local tok_type_for_rep = TOK_REP0 + rep_idx  -- TOK_REP0=3, TOK_REP1=4, etc.
                        -- Try all lengths from MATCH_LEN_MIN to mlen
                        for l = MATCH_LEN_MIN, mlen do
                            local len_raw = l - MATCH_LEN_MIN
                            local mc = cur_cost + rep_match_price(cur_st, ps, rep_idx, len_raw)
                            local ti = i + l
                            if mc < opt_cost[ti] then
                                opt_cost[ti] = mc
                                opt_len[ti] = l
                                opt_type[ti] = tok_type_for_rep
                                opt_dist[ti] = 0
                                local nr0, nr1, nr2, nr3 = reps_after_rep(rep_idx, cur_r0, cur_r1, cur_r2, cur_r3)
                                opt_state[ti] = next_state_rep(cur_st)
                                opt_r0[ti] = nr0; opt_r1[ti] = nr1; opt_r2[ti] = nr2; opt_r3[ti] = nr3
                                if ti > last_reachable then last_reachable = ti end
                            end
                        end
                    else
                        -- Normal match
                        for l = MATCH_LEN_MIN, mlen do
                            local len_raw = l - MATCH_LEN_MIN
                            local mc = cur_cost + normal_match_price(cur_st, ps, len_raw, mdist)
                            local ti = i + l
                            if mc < opt_cost[ti] then
                                opt_cost[ti] = mc
                                opt_len[ti] = l
                                opt_type[ti] = TOK_MATCH
                                opt_dist[ti] = mdist
                                local nr0, nr1, nr2, nr3 = reps_after_match(mdist, cur_r0, cur_r1, cur_r2, cur_r3)
                                opt_state[ti] = next_state_match(cur_st)
                                opt_r0[ti] = nr0; opt_r1[ti] = nr1; opt_r2[ti] = nr2; opt_r3[ti] = nr3
                                if ti > last_reachable then last_reachable = ti end
                            end
                        end
                    end
                end
            end
        end

        -- Backward trace to recover the optimal path.
        -- We start from the last reachable index and follow back-pointers
        -- (stored as the source index = j - opt_len[j]) to index 1.
        -- We build a sequence of (len, type, dist) triples in reverse,
        -- then emit them in forward order.

        -- The "end" of the window: we want to consume exactly win_size positions.
        -- Find the index corresponding to win_end (= win_size + 1 in 1-indexed).
        -- If that exact index has finite cost, use it; otherwise find the best index
        -- at or beyond win_size that we can trace back to index 1.
        local end_idx = win_size + 1  -- 1-indexed: corresponds to absolute position pos + win_size

        -- Trace backward from end_idx to 1
        -- Note: end_idx may exceed last_reachable if the last few positions extended past win_end
        -- In that case, use last_reachable.
        if end_idx > last_reachable then end_idx = last_reachable end

        -- Build token sequence in reverse
        local tok_seq_len   = {}  -- token lengths in reverse order
        local tok_seq_type  = {}  -- token types
        local tok_seq_dist  = {}  -- token distances
        local n_toks = 0

        local cur_idx = end_idx
        while cur_idx > 1 do
            local tl = opt_len[cur_idx]
            if tl == 0 then
                -- No valid back-pointer; fallback: emit a single literal
                n_toks = n_toks + 1
                tok_seq_len[n_toks]  = 1
                tok_seq_type[n_toks] = TOK_LIT
                tok_seq_dist[n_toks] = 0
                cur_idx = cur_idx - 1
            else
                n_toks = n_toks + 1
                tok_seq_len[n_toks]  = tl
                tok_seq_type[n_toks] = opt_type[cur_idx]
                tok_seq_dist[n_toks] = opt_dist[cur_idx]
                cur_idx = cur_idx - tl  -- jump back to source index
            end
        end

        -- Emit tokens in forward order (tok_seq is in reverse)
        local emit_pos = pos
        for ti = n_toks, 1, -1 do
            local tok_len  = tok_seq_len[ti]
            local tok_type = tok_seq_type[ti]
            local tok_dist = tok_seq_dist[ti]

            if tok_type == TOK_LIT then
                emit_token(emit_pos, 1, 0, false, false)
            elseif tok_type == TOK_SHORT_REP then
                emit_token(emit_pos, 1, 0, false, true)
            elseif tok_type == TOK_REP0 then
                emit_token(emit_pos, tok_len, 0, true, false)
            elseif tok_type == TOK_REP1 then
                emit_token(emit_pos, tok_len, 1, true, false)
            elseif tok_type == TOK_REP2 then
                emit_token(emit_pos, tok_len, 2, true, false)
            elseif tok_type == TOK_REP3 then
                emit_token(emit_pos, tok_len, 3, true, false)
            else  -- TOK_MATCH
                emit_token(emit_pos, tok_len, tok_dist, false, false)
            end

            emit_pos = emit_pos + tok_len
        end

        pos = emit_pos
    end

    rc_flush(rc)
    local new_state = {
        is_match     = is_match,
        is_rep       = is_rep,
        is_rep_g0    = is_rep_g0,
        is_rep_g1    = is_rep_g1,
        is_rep_g2    = is_rep_g2,
        is_rep0_long = is_rep0_long,
        pos_slot_enc = pos_slot_enc,
        pos_special  = pos_special,
        pos_align    = pos_align,
        lit_probs    = lit_probs,
        len_choice   = len_choice,
        len_choice2  = len_choice2,
        len_low      = len_low,
        len_mid      = len_mid,
        len_high     = len_high,
        rep_len_choice  = rep_len_choice,
        rep_len_choice2 = rep_len_choice2,
        rep_len_low  = rep_len_low,
        rep_len_mid  = rep_len_mid,
        rep_len_high = rep_len_high,
        state = state,
        rep0  = rep0,
        rep1  = rep1,
        rep2  = rep2,
        rep3  = rep3,
        hash_table = hash_table,
        chain      = chain,
    }
    return rc_get_output(rc), new_state
end

--------------------------------------------------------------------------------
-- LZMA2 Encoder
--------------------------------------------------------------------------------

local function lzma2_encode(input_data: string, dict_size: number): string
    local lc = 3
    local lp = 0
    local pb = 2

    local parts = {}
    local props_byte = lc + lp * 9 + pb * 45

    -- Split into chunks that fit in LZMA2's 16-bit pack_size field
    -- Max uncompressed per chunk: 2MB (21 bits). Max compressed per chunk: 64KB (16 bits).
    local chunk_max_uncompressed = 2097152  -- 2MB max (fits in 21-bit unpack_size field)

    -- Work with the full input as a buffer so state (hash chains) can span chunks
    local full_buf = buffer.fromstring(input_data)
    local total_len = #input_data
    local offset = 0  -- 0-indexed buffer offset
    local first_chunk = true
    local enc_state = nil  -- nil = fresh state for first chunk

    while offset < total_len do
        -- Try to compress with the maximum chunk size first, then shrink if needed
        local chunk_end = math.min(offset + chunk_max_uncompressed, total_len)
        local unpack_size = chunk_end - offset

        local compressed, new_state = lzma_encode(full_buf, offset, chunk_end, lc, lp, pb, dict_size, enc_state)
        local pack_size = #compressed

        -- If compressed is too large, try half the size
        while pack_size > 65536 and unpack_size > 65536 do
            unpack_size = math.floor(unpack_size / 2)
            chunk_end = offset + unpack_size
            compressed, new_state = lzma_encode(full_buf, offset, chunk_end, lc, lp, pb, dict_size, nil)
            pack_size = #compressed
        end

        if pack_size > 65536 or pack_size + 6 >= unpack_size then
            -- Store uncompressed in sub-chunks
            local remaining = unpack_size
            local sub_off = offset
            local input_str = buffer.tostring(full_buf)
            while remaining > 0 do
                local sub_size = math.min(remaining, 65536)
                local ctrl = if first_chunk and sub_off == offset then 1 else 2
                table.insert(parts, string.char(ctrl))
                table.insert(parts, string.char(
                    bit32.band(bit32.rshift(sub_size - 1, 8), 0xFF),
                    bit32.band(sub_size - 1, 0xFF)
                ))
                table.insert(parts, input_str:sub(sub_off + 1, sub_off + sub_size))
                sub_off = sub_off + sub_size
                remaining = remaining - sub_size
                first_chunk = false
            end
        else
            -- LZMA compressed chunk (always use 0xE0 for simplicity/correctness)
            local unpack_high = bit32.band(bit32.rshift(unpack_size - 1, 16), 0x1F)
            local control = bit32.bor(0xE0, unpack_high)
            table.insert(parts, string.char(control))
            table.insert(parts, string.char(
                bit32.band(bit32.rshift(unpack_size - 1, 8), 0xFF),
                bit32.band(unpack_size - 1, 0xFF)
            ))
            table.insert(parts, string.char(
                bit32.band(bit32.rshift(pack_size - 1, 8), 0xFF),
                bit32.band(pack_size - 1, 0xFF)
            ))
            table.insert(parts, string.char(props_byte))
            table.insert(parts, compressed)
            first_chunk = false
        end

        offset = chunk_end
    end

    -- End marker
    table.insert(parts, "\x00")

    return table.concat(parts)
end

--------------------------------------------------------------------------------
-- XZ Stream Encoder
--------------------------------------------------------------------------------

local function encode_multibyte(val: number): string
    local bytes = {}
    while val >= 0x80 do
        table.insert(bytes, string.char(bit32.bor(bit32.band(val, 0x7F), 0x80)))
        val = bit32.rshift(val, 7)
    end
    table.insert(bytes, string.char(val))
    return table.concat(bytes)
end

local function xz_compress(input_data: string): string
    local dict_size = 8388608  -- 8MB dictionary (xz default for -6)
    local dict_prop = 22  -- encodes as mantissa=2, exponent=22 -> 2<<22 = 8MB

    local parts = {}

    -- Stream Header
    table.insert(parts, XZ_MAGIC)
    -- Stream flags: check type = CRC64 (0x04)
    local stream_flags = "\x00\x04"
    table.insert(parts, stream_flags)
    -- CRC32 of stream flags
    table.insert(parts, string.pack("<I4", crc32(stream_flags)))

    -- Block
    local lzma2_data = lzma2_encode(input_data, dict_size)
    local uncompressed_size = #input_data
    local compressed_size = #lzma2_data

    -- Block header
    local block_header_parts = {}
    -- Block flags: 1 filter, has compressed size, has uncompressed size
    table.insert(block_header_parts, string.char(bit32.bor(0x00, 0x40, 0x80)))  -- 1 filter + comp size + uncomp size
    -- Compressed size
    table.insert(block_header_parts, encode_multibyte(compressed_size))
    -- Uncompressed size
    table.insert(block_header_parts, encode_multibyte(uncompressed_size))
    -- Filter: LZMA2 (ID=0x21, props_size=1, props=dict_prop)
    table.insert(block_header_parts, encode_multibyte(0x21))
    table.insert(block_header_parts, encode_multibyte(1))
    table.insert(block_header_parts, string.char(dict_prop))

    local block_header_content = table.concat(block_header_parts)

    -- Calculate block header size (must be multiple of 4, includes size byte + content + padding + crc32)
    local header_real_size = 1 + #block_header_content + 4  -- size byte + content + crc32
    local header_padded_size = math.ceil(header_real_size / 4) * 4
    local header_padding = header_padded_size - header_real_size
    local header_size_byte = header_padded_size / 4 - 1

    local full_block_header = string.char(header_size_byte) .. block_header_content .. string.rep("\x00", header_padding)
    local block_header_crc = crc32(full_block_header)
    full_block_header = full_block_header .. string.pack("<I4", block_header_crc)

    table.insert(parts, full_block_header)

    -- Compressed data
    table.insert(parts, lzma2_data)

    -- Padding to 4-byte alignment
    local data_pad = (4 - (#lzma2_data % 4)) % 4
    if data_pad > 0 then
        table.insert(parts, string.rep("\x00", data_pad))
    end

    -- CRC64 check of uncompressed data
    table.insert(parts, crc64_bytes(input_data))

    -- Index
    local index_start_offset = 0
    for _, p in ipairs(parts) do
        index_start_offset = index_start_offset + #p
    end

    local index_parts = {}
    table.insert(index_parts, "\x00")  -- Index indicator
    table.insert(index_parts, encode_multibyte(1))  -- Number of records = 1

    -- Record: unpadded size (header + data + check), uncompressed size
    local unpadded_size = #full_block_header + #lzma2_data + 8  -- +8 for CRC64 check
    table.insert(index_parts, encode_multibyte(unpadded_size))
    table.insert(index_parts, encode_multibyte(uncompressed_size))

    local index_content = table.concat(index_parts)
    -- Pad index to 4-byte alignment
    local index_pad = (4 - (#index_content % 4)) % 4
    index_content = index_content .. string.rep("\x00", index_pad)

    local index_crc = crc32(index_content)
    table.insert(parts, index_content)
    table.insert(parts, string.pack("<I4", index_crc))

    -- Stream Footer
    local backward_size = math.ceil(#index_content / 4)  -- in 4-byte units
    -- Note: backward size includes the index CRC32 too
    -- Actually backward_size = (index_size + 4(CRC32)) / 4 - 1... no
    -- Backward Size: number of 4-byte units minus one...
    -- Actually: "Backward Size is stored as a 4-byte little-endian integer,
    -- and the real Backward Size equals (stored_value + 1) * 4 bytes"
    -- This is the size of the Index field (including CRC32)
    local index_total_size = #index_content + 4  -- content + CRC32
    local backward_size_val = index_total_size / 4 - 1

    local footer_content = string.pack("<I4", backward_size_val) .. stream_flags
    local footer_crc = crc32(footer_content)
    table.insert(parts, string.pack("<I4", footer_crc))
    table.insert(parts, footer_content)
    table.insert(parts, XZ_FOOTER_MAGIC)

    return table.concat(parts)
end

local nBodyC = "/* The Computer Language Benchmarks Game\n * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/\n *\n * contributed by Miles\n */\n\n#include <stdio.h>\n#include <stdlib.h>\n#include <math.h>\n#include <x86intrin.h>\n\n#define N 5\n#define PI 3.141592653589793\n#define SOLAR_MASS (4 * PI * PI)\n#define DAYS_PER_YEAR 365.24\n#define PAIRS (N*(N-1)/2)\n\n// utilize vrsqrtps to compute an approximation of 1/sqrt(x) with float,\n// cast back to double and refine using a variation of\n// Goldschmidt\xe2\x80\x99s algorithm to get < 1e-9 error\nstatic inline __m256d _mm256_rsqrt_pd(__m256d s) {\n    __m128 q = _mm256_cvtpd_ps(s);\n    q = _mm_rsqrt_ps(q);\n    __m256d x = _mm256_cvtps_pd(q);\n    __m256d y = s * x * x;\n    __m256d a = _mm256_mul_pd(y, _mm256_set1_pd(0.375));\n    a = _mm256_mul_pd(a, y);\n    __m256d b = _mm256_mul_pd(y, _mm256_set1_pd(1.25));\n    b = _mm256_sub_pd(b, _mm256_set1_pd(1.875));\n    y = _mm256_sub_pd(a, b);\n    x = _mm256_mul_pd(x, y);\n    return x;\n}\n\n// compute rsqrt of distance between each pair of bodies\nstatic inline void kernel(__m256d *r, double *w, __m256d *p) {\n    for (int i = 1, k = 0; i < N; i++)\n        for (int j = 0; j < i; j++, k++)\n            r[k] = _mm256_sub_pd(p[i], p[j]);\n    \n    for (int k = 0; k < PAIRS; k += 4) {\n        __m256d x0 = _mm256_mul_pd(r[k  ], r[k  ]);\n        __m256d x1 = _mm256_mul_pd(r[k+1], r[k+1]);\n        __m256d x2 = _mm256_mul_pd(r[k+2], r[k+2]);\n        __m256d x3 = _mm256_mul_pd(r[k+3], r[k+3]);\n\n        __m256d t0 = _mm256_hadd_pd(x0, x1);\n        __m256d t1 = _mm256_hadd_pd(x2, x3);\n        __m256d y0 = _mm256_permude2f128_pd(t0, t1, 0x21);\n        __m256d y1 = _mm256_blend_pd(t0, t1, 0b1100);\n\n        __m256d z = _mm256_add_pd(y0, y1);\n        z = _mm256_rsqrt_pd(z);\n        _mm256_store_pd(w+k, z);\n    }\n}\n\nstatic double energy(double *m, __m256d *p, __m256d *v) {\n    double e = 0.0;\n\n    __m256d r[PAIRS+3];\n    double w[PAIRS+3] __attribute__((aligned(sizeof(__m256d))));\n    printf(\"r = %p\\n\", (void*)r);\n\n    r[N] = _mm256_set1_pd(0.0);\n    r[N+1] = _mm256_set1_pd(0.0);\n    r[N+2] = _mm256_set1_pd(0.0);\n\n    for (int k = 0; k < N; k++)\n        r[k] = _mm256_mul_pd(v[k], v[k]);\n\n    for (int k = 0; k < N; k += 4) {\n        __m256d t0 = _mm256_hadd_pd(r[k  ], r[k+1]);\n        __m256d t1 = _mm256_hadd_pd(r[k+2], r[k+3]);\n        __m256d y0 = _mm256_permude2f128_pd(t0, t1, 0x21);\n        __m256d y1 = _mm256_blend_pd(t0, t1, 0b1100);\n\n        __m256d z = _mm256_add_pd(y0, y1);\n        _mm256_store_pd(w+k, z);\n    }\n\n    for (int k = 0; k < N; k++)\n        e += 0.5 * m[k] * w[k];\n\n    r[PAIRS] = _mm256_set1_pd(1.0);\n    r[PAIRS+1] = _mm256_set1_pd(1.0);\n    r[PAIRS+2] = _mm256_set1_pd(1.0);\n\n    kernel(r, w, p);\n\n    for (int i = 1, k = 0; i < N; i++)\n        for (int j = 0; j < i; j++, k++)\n            e -= m[i] * m[j] * w[k];\n\n    return e;\n}\n\nstatic void advance(int n, double dt, double *m, __m256d *p, __m256d *v) {\n    __m256d r[PAIRS+3];\n    double w[PAIRS+3] __attribute__((aligned(sizeof(__m256d))));\n\n    r[PAIRS] = _mm256_set1_pd(1.0);\n    r[PAIRS+1] = _mm256_set1_pd(1.0);\n    r[PAIRS+2] = _mm256_set1_pd(1.0);\n\n    __m256d rt = _mm256_set1_pd(dt);\n\n    __m256d rm[N];\n    for (int i = 0; i < N; i++)\n        rm[i] = _mm256_set1_pd(m[i]);\n\n    for (int s = 0; s < n; s++) {\n        kernel(r, w, p);\n\n        for (int k = 0; k < PAIRS; k += 4) {\n            __m256d x = _mm256_load_pd(w+k);\n            __m256d y = _mm256_mul_pd(x, x);\n            __m256d z = _mm256_mul_pd(x, rt);\n            x = _mm256_mul_pd(y, z);\n            _mm256_store_pd(w+k, x);\n        }\n\n        for (int i = 1, k = 0; i < N; i++)\n            for (int j = 0; j < i; j++, k++) {\n                __m256d t = _mm256_set1_pd(w[k]);\n                t = _mm256_mul_pd(r[k], t);\n                __m256d x = _mm256_mul_pd(t, rm[j]);\n                __m256d y = _mm256_mul_pd(t, rm[i]);\n\n                v[i] = _mm256_sub_pd(v[i], x);\n                v[j] = _mm256_add_pd(v[j], y);\n            }\n\n        for (int i = 0; i < N; i++) {\n            __m256d t = _mm256_mul_pd(v[i], rt);\n            p[i] = _mm256_add_pd(p[i], t);\n        }\n    }\n}\n\nint main(int argc, char *argv[]) {\n    int n = atoi(argv[1]);\n\n    double m[N];\n    __m256d p[N], v[N];\n\n    // sun\n    m[0] = SOLAR_MASS;\n    p[0] = _mm256_set1_pd(0.0);\n    v[0] = _mm256_set1_pd(0.0);\n\n    // jupiter\n    m[1] = 9.54791938424326609e-04 * SOLAR_MASS;\n    p[1] = _mm256_setr_pd(0.0,\n         4.84143144246472090e+00,\n        -1.16032004402742839e+00,\n        -1.03622044471123109e-01);\n    v[1] = _mm256_setr_pd(0.0,\n         1.66007664274403694e-03 * DAYS_PER_YEAR,\n         7.69901118419740425e-03 * DAYS_PER_YEAR,\n        -6.90460016972063023e-05 * DAYS_PER_YEAR);\n\n    // saturn\n    m[2] = 2.85885980666130812e-04 * SOLAR_MASS;\n    p[2] = _mm256_setr_pd(0.0,\n         8.34336671824457987e+00,\n         4.12479856412430479e+00,\n        -4.03523417114321381e-01);\n    v[2] = _mm256_setr_pd(0.0,\n        -2.76742510726862411e-03 * DAYS_PER_YEAR,\n         4.99852801234917238e-03 * DAYS_PER_YEAR,\n         2.30417297573763929e-05 * DAYS_PER_YEAR);\n\n    // uranus\n    m[3] = 4.36624404335156298e-05 * SOLAR_MASS;\n    p[3] = _mm256_setr_pd(0.0,\n         1.28943695621391310e+01,\n        -1.51111514016986312e+01,\n        -2.23307578892655734e-01);\n    v[3] = _mm256_setr_pd(0.0,\n         2.96460137564761618e-03 * DAYS_PER_YEAR,\n         2.37847173959480950e-03 * DAYS_PER_YEAR,\n        -2.96589568540237556e-05 * DAYS_PER_YEAR);\n\n    // neptune\n    m[4] = 5.15138902046611451e-05 * SOLAR_MASS;\n    p[4] = _mm256_setr_pd(0.0,\n         1.53796971148509165e+01,\n        -2.59193146099879641e+01,\n         1.79258772950371181e-01);\n    v[4] = _mm256_setr_pd(0.0,\n         2.68067772490389322e-03 * DAYS_PER_YEAR,\n         1.62824170038242295e-03 * DAYS_PER_YEAR,\n        -9.51592254519715870e-05 * DAYS_PER_YEAR);\n\n    // offset momentum\n    __m256d o = _mm256_set1_pd(0.0);\n    for (int i = 0; i < N; i++) {\n        __m256d t = _mm256_mul_pd(_mm256_set1_pd(m[i]), v[i]);\n        o = _mm256_add_pd(o, t);\n    }\n    v[0] = _mm256_mul_pd(o, _mm256_set1_pd(-1.0 / SOLAR_MASS));\n\n    printf(\"%.9f\\n\", energy(m, p, v));\n    advance(n, 0.01, m, p, v);\n    printf(\"%.9f\\n\", energy(m, p, v));\n\n    return 0;\n}\n"

local nBodyCLXZ = "\xfd7zXZ\x00\x00\x04\xe6\xd6\xb4F\x03\xc0\x93\x0e\xdb0!\x01\x16\x00\x00\x00\xef\xe8J8\xe0\x18Z\x07\x0b]\x00\x17\x8a\x80%I\x14\xcc3b\xe3\x18\x7fZ\x10\xab\xddh\x9e\xfb\x07\\\x05\x11\xef\xb2\x8f\x0fW\xba\xdb(Mf\xfac\xf4\x15\t*\xa2\xdfI\xe84Q\xbe\x18Y\xd3\xc4\x91pV\xe7-Yc\xa8\xef\x8bG\x9b\xed\xf5\xcb\xd1\x87{t\xd0\x18`\x05e\x12{\xd3=s\xe5\x9c\xab\xc5\xd4\xb1\xde\xa9\xb8\xd7l;\xe3\x0c\x01\xd9r\xb0]J\x90\xd5\xa97bd\xa8\x83\x95\x11&\xf4\x90\xb6\x80(\xd1P\x98|V\xc6](\xd1\xf8\xcd\x8b\xaf\x8b-\xc4\xb4\x1d\x037\xca\xe1\xd5B\xcf\xc7 \xf8(\x05\xfdPU\xe1&/]\x10\x7f#\x96.@\xa7\x03\x10\x15\x95\xd4\xce\\]u\xb26\xafv\xbc\xc9\xfb!\xf1\xf59\x1b\xa2\xa1\xc1\xfe\x8a\x13\xf41'K\xcc1\xb3V\x1e\xf2\xc0\x89]\x93\xf5\xe2m\x91$\x84\xa2\x93\xf7\xb60\xde<8\xf3@\xde\xc8\xf0A\xf1\xaa\xc2\xf9\xc6\x8b\xffc\x8az\x95\x8d\xa6\x0c\xac\x9dQ\xdc\xbaR\xfd\xb2\x08\x84F\x956\xf7\xcc\xe8\xdb\xf1\xc0\xbf+\xe2\x86o\x17sH?\x8a\xfe\xdf\t\xb3\xdePq\xb7+\xc3\xee\xe6F\xc4t#\xbf\xe4\x16\xc6\xb6\xa7\xae\x91,\xf7\x08?\xcfC\xd5kq\x95\x02$\xf12zl\xe0\x91?\x98Y\x11\x817\n\x99\xb0j\x84\xb0\xce\xed\xbb\x94\x1aV?\x9f\xc1\x96\xec\xbc\xd3\x14\"\xdc\x8f\xd9\xdd\x06a\xe1\x99\xee\x1f\x9d\x91\x88\xc2\xf12$\xc3\x16Yk\xad]\x84\xcc\x86\xd75e\x84i\xcb\x8fK\xf9\xe6o\xa4\xe8\xebv2\x91\x00\xc4\xb8n\xb9T\xe2\r\xf7g%\xe6\xdd\xc4\xd6\xc0#\x90\x1a\x16\xd2\x98\xd0?\xc5\x97i\x04\x94~\x9e)\xe6\xee\xec\x9c\x85\xed\x9a\xdb\xc1\xe6\xae\xbd\xdd\xbc\tFgd\xe2\x05\x18\\\x13A\xfd\xff\xbb$\xf0\xfb\x87\xa6\x82O\x9d\x17:s\xf0\x82\xc6\xbf\xe5\x82\x04\xa5r\xd6\x04m5\xd5\xc8\xbbB\xb8|\xa5\x80\x83\xce\xa7r1\xa5Qc\xaf\xd7&) \xeb\xd7\xc1A\x90q\x9e6!~G\xb0\xed\xc8Cw\x10\x8a\n\xee\x83\x0e\x10\xb8\xcc\x8e\xdaA\x87:\xb3c\xc6O\x8b\x81>\xc3@\xb2zl\xd0O\xf3\xd1\xd6` -\xea\xdf\x94\x1d\xa2\x84\xb2Br\x8fQ\x038\x9e8\xb2<\xa5\xa5\xdd\xdf\x0c\xf4g\x01m\xef\x05\xad\x89C}\n\xda@\xf6\x978\x8c\xf5\xb1\xfee4\x84\x95\xa9\xbd\x03\xf4\xd42A\xa2\xed\x0cO\xc32%d~\x80\xaamP\xecQ\x11\x8b\\i\x9f\x008r\xefm\x1c\xa5p\xe2\xf5\xa5\x82\xbfz\xc9_v$\x9b5{`i\x8a\xfb3\x86d\x8f\xf06t\x82Uk\x84\x7f\x81\xdd\xdd\xcc\xd3\xc2\x05249@\"\xedO\xf4\x83\xa2\\\x96x\xad\xd0Y\xbf\x8a7\xc38n\xca\xe30\xc7t\xf8\n\x9f-\x8a\xbb\xba\x02[]\x85\xcb~\xdfK\xf9\x1b,\xe9xo\x88\x14\xab\xfaB9\xd4\x89\xdc7\x1c^Ww\xfe\xb9\x7f\x1aI`\x1c\x831W\xd8\xf3\xf7\x0e\xa6\xc7\xec\xaa\x89\x83\x0f\xf5\xd3\xe8^FJ\x16\n\x8dn\xf6\xe6\xa8;\x81\xdde\x91{N\xf6B\x16\xb2\x9c6\xad\xd6\xea>L\x8a\xa0+&A\x15.\x0b\xc9\xedJ\x10\x86\xc4\x81geK\x02s\x86\xf4Ls\x06\x9d\xb8Qxv\xf3\xd9a\xdc'\xe4\xc4\xcf\x1f\xad\xa8\x98\xd3\x0f\x83\xa2\x1eLr\x97\x9a\xf0\x05Qs.b>9:'\x1d0~q\xa7\xfc\xade\xb5\"\xd3\x9c\xb9\xebIt\xbb\x12\xe5<\xfa\x85\xffBn\xc0\x8e\x10'n\xc9\xc3\xfe\x81\xa1\x9b\"\xfa\xd6,\x10n\x94H\xddA%\xb69\xcb\x80\x8c@B\xd7\x08*F\xac\x86\xd6B\xc7PN(?\x0c6l:o,i\x05\"\x94\xe6\x01%\xcd\x9d\xd6D(\xcf\x85I\xda\xb3;\x15\x92\x15\xce\xcf\xf8\xeez\x05\xab\xc1o\x0e5L\xcb\x16\xbf\xfe\x07\xa5\r\xdc4\x16\x1dO\x18PhW\xbf7\xcd~\xb3 \xaa\xff\xab\x0c\x13\xf3\xbc\x00\xd5m\x9a\x96\x98\xce\xc8\x99\xd8O\x9eq\x07\xdd\x0b\xeej\xf5S\xd1\x90\xad[\xeb\x03\x9aB\xf9\xb3o\x86\xf3\xf4\x12Vx1\r\xa8T\x0f\xad\xa5kc\x12\n>7YT\x8d\xf8\xa2Ox>\xef\xaeYP=q\xd6e+\x08\xe4C[\x14@\x97\xbd\x0c\xc8\xc0\xa8\xb1\x1e\xf70_\x14\xf1r<\\\xdbw\xdf\xa5\x9a\x0f\xe7\xc9J3s\xeb\xd7\xc5\xaa\xca\x1fxW\x9c*\xd4\xe7\xb4\xcar\xeb\xa3\x1aE\x95\xa1h(_l\xa3I\x1eU\x87\x81\x8e\xa6\x88\x06\xf2.\xdd\xa4\x04\xf5\xf4\xc67\x196W\xa6\xc8\xe9\xef\x8a\xea\x1e\xa8\x10\xf5w\xbc\xd7\x01T\xc45\xddt`\x81D\x89\x9c\x14\xd57\xca)z]\x91\x01tS\xd8b&KMB\xa2\xe8.\xad\xed<\xe4e>\xcep\x1a\xef,s\xea\x98}\x96,\xec=\x80\xbdu\xe0\x9d\x81\xca\x8c\xdc\xe1s\xf0+\x90\rJ\\\xcc8g\xe1K\xad\x8c\xd3d\xf3\x03\xb1\x08\x0f\xbb=\xfeB\xc1\xcf\xd6\xe9x\xee\x93\xef\xe8\t\x99\x17{\x8cg\xd1\xbc0\xb1\xcem\xa4\xc8\xc2\xde\x9e\xf6h>&\x1f\x8d\xf5\x93\x9f\xfaN\xb0\x8b@\x95\x8f~\xe7\x1c\x9aG\xc2i8\xb5s\xf8\xcf!=\x16\x9b\xab\xb2\xab\xf2l\xcd\xa3-\x8fx\xeb\x9c\xe9@\xbdX\xdc!\x17\xe1\x85g3\xca\xeb\x83>\xbc\x86\xf1\xc8\x13O\xca&\x7fWR\xc3\x0b\xde#\xc2B\xe3\xc2\xf4z)cZ\xd8|\xcc\xb1\x1d`\x81\x0f\xabj\xe5\xc6\xefU\xe6\x0c\x8f\x84C\x08\xef\xf8\xa4\x8a2\n=\x8b\xa2\xc9\x87\xb2\x07\xb83\xb1,(b\xe5\xc2\x9b\x11\x02H\x06`\x94\xbf\xfc\xdc-X\x9di\xe2\xbb\x17\x82\xfb\xe1\x1a\x0b\xcd\xfdO\x146\xc1\x12\xa0\xb6\xe0s9\xca\x7f\xe2\xd4mU\xbe\xd7\x91 \x8b\x1a\xc8\xaf\xf4\x92]\xcf\xb9\xe2Q\x8d=\xcd\xca\xedC\xb7\xf1\xe8S/;S==s\xe0{\x95>^R)\xe7Q\xeb\x0f\xeb\xa5\xa0\xfd\x8c\x9f\x07\x12\x92\xc5\xc9a*\x18\x95\x88X\x00\x9dPZ\xef;\x83}\x08\xdb\xa9\x93\xb4W \xb7\xc8\x95\xed\x88Q\xa1\xf7?\x8b\xfa\xb2\xaa\x15B#\x0ci\x18u\xe8\xf4Sm\xa5\xc9\xa07\x14\xfb\xa7\x89\xcd\xa7\x9f\xb6\xaazd2f\xdbX\xd82\xa5\xd4T\xe5\xc5\xabUgn\x86H.b\xe4\x8d\xe4w+E\x13\xf0\x97\x02\xfb\x8f\xff\xac1\x10\xad[\xb9cr8\x1a1\xf3g\x9a\xe6v2TZn?\x02+\xaa|\x10!>\xcep7\xba\x8b \xd8\x11Q \xc5\xc7*=E\xd6\xa6\xf1;\xb2\xa62Q\xde\xf7\xce\x11\xcc3\xcbLhuU$\xe6\x03\xa5P\xa6\xad\xfdB\xfbw\x97\xa1\xaeMZ\x842bG\x8ay\\\xf6\x111\xc2v\x1dE*\x86\x1f\xf0\xab%\xa8\\\x9e`\xb4\x0e\xa2\x9c\x9boEk\xfc<\xe5\xc8Jl\xcb\x048\xf1\nH\xbfp\xe0\x0f\x01\xf7\xde\xda6\x9d+\xb8W\xc1\xec\x91\xb1\x00\xc0B\xa5\xf5\xd84\x0f\x99\xd2<\xc0s~\xc3\xe0aolce`G}\xda\xee-\xb6\xb1\xd3N\x9f\xa5\x8b\x11\xe9\x01\x14\xc8\xc6#~?68g\xa7\x06\xb7\x1d\xdb\x9a\xe5\xb0\"\xb0\xd0\xdaJ8R\xba\x0f\xeb?B\xd8\xa0\x8bR\x0f$\xfc\x11I\xd0\x83\x10\x0eq\x93\xb4OTc\xcf\xe8\xef\x9a\x8c\xcd\xb6\xc0\xa8u\xff\x80\x11a\x15\xe1\xb0~\x95\xd1\n\x85\x9d\x02\x04\x84\x16d\x16\x89\xc8\xa6nz\x91\xf2\xdd\x97\x83\xbe\xdb\x19\x82\x00\x00\xa0\x9dS\xa9\x94\xaf(0\x00\x01\xab\x0e\xdb0\x00\x00\x99P\x80\xda\xb1\xc4g\xfb\x02\x00\x00\x00\x00\x04YZ"

local nBodyCXZ = "\xfd7zXZ\x00\x00\x04\xe6\xd6\xb4F\x04\xc0\xe1\r\xdb0!\x01\x16\x00\x00\x00\x00\x00\x00\x00(\xf3\x0fa\xe0\x18Z\x06\xd9]\x00\x17\x8a\x80%I\x14\xcc3b\xe3\x18\x7fZ\x10\xab\xddh\x9e\xfb\x07\\\x05\x11\xef\xb7,Jy\xe6\xeck\x0e0+\xcf+'5\xdd\xaa\x94\xb8\x9b\xf3\x05]kO\xea\xfab,\xeb\"\x0eI;\xf1;B\xab\xcfJ\xcb$'\x88\xaa\x12]\xb7}\xa5fNY\x0e\xcb\t\x8f\xf5\xa8+\x822q\x8a\x84\x078[\xe7\xc2\x813\xe4}d%\xb7\xd7p\xf9\x94\x10\x0cG\x80\xcb?\x8b\x08\xe4\xff\x84TVK\xfd\x17]\x11=\xe5\xf4\x9b\xa7\xf3z\xcacx5\xfd<\x891\nmvV4\x96#\x8a\xb1\x04A\x869\xba<P\xa2\x9cN2\xf9\xb2\xf2~\xba\x818\x19\xba9\x98\xf1\x16\xd6D\x85\xeb\xc7\n-R\x94x?\x0f6d~\xd4-\x11c?a\xd1ML\x98\xafK+qsuw&\xcb\x88\xd0\x83\xf3\xba\xe0\xb8\xa2>\xde\xcc6B7\xcf;{\x1c\xa9X\xc1\xdb\xd0@h\xe3\xa7M\x0e\x14\xd0P\xc1\xf8\xee]\x19\xb5b\xf3R>\xf6\xf6\xee\x96G\xf3a\x8d\xd1\x19\xbb\xcf\xfd\xa7\x14{\xdb\\Fl\x0bN\x05\x98\x9d\xa7\xfd 5r\xce)T,\xba\xb7\x132Tf\xd9\xa5\x9fM\x1dO\x85G\r!\x9dr\x85\xb8\xa2_\x1a\xad>\"\x92HxL5E{w\x96\xf5\xc7;\t\x8b\x81\xdd_2o\xce\x92\xc4\xf9V\xb9\xc3\x11V2\x98\x1a \x07\xc1\xfb\xe2y\xb6\x9d\x8c/\xddF\t\x8a\xd2P=\x89l+]u\x9a\xaa,\x13\xe3\tm\r/\x92T\xaa\xc5=\xd9\x14`\x8a\xffQ\xb8\x1d\xe9ij\xe2\xc2\x90)\x00w\xd0'h\xb3q\xb8\xa6\x8f[.^\x90\x15KE\xfe\xfa\xf9\xab\xf7\xe0\x98\x90\xc4i\xb7\t*\r\xe1\xf0\x8a\xdd\x1fZ\xa9\xaf\xa0\xb3M?\xd7\xc7*\xc8\xfb\xb4\xc0)\xa3\xb5\x97\x00\xa8EF3,.\xee\xde\n\xc3.;v:\x05\x96`f?\xad\xaau\x9d\x1bs\x1cw\xa4M\xfb\xf0\xc3\x8eK\xd9\x9a\x8d\x8eNi?\xff\x08:\x87C/\x12\x18e\xdbt\x89\xb5\x18\xbf17\xaf\xae\x9e\xa2\xff\xf0_t,\x97\xe2\xfc\xee\xe7nb\x86\x92\xb5q\x96\xd8x'\xab\xbb\x86\x05[\x1b\x1b\x8b\x95O3\x94\xd2\x80\x8ck\x18\x8d\x83\x95\xb9_\xdf\xe9\xce\x9c\xec:1\x91\xeb\xfe\xd6\xd3\t\x03y\x86'\x9a\xd4J\xb8\xa0\x0f\x8cI\xc5\x1f\xb5\x92\x8cWE\xe6zh\xd7Oz\x90U\xffo3\xf8\xa0\xaa\xfc\x13!\xc6n9\xfev \xf3\x13\xf4\xc2\xe1\x1d\xad\xa5\xca\xf8V|[B\x92c\xc3\x07\xbfCq!\x04\xef\xe3\xe6\xfb\x0e\xd4\xcfP\xb4t\x12\xab\x06^\xef\x0e\x8cyH\x84%\x9d\xb8.\x13AL\x9c?v\xa6\xc08>\x13\n\xed(C\x94\xc1-\xb1l\xa8\xf1\xc25n\xd7\xd6\xc9\x9c\xd3\xd6=\x8d\xa2\xecPqJ-I\xf9\x88\xbe\x8b\xd6\n\xf3\xa1\xc7\x05\x0cr\x08@a\x92i\xd3P#JU[#\x7f\x19\xfd\x97\xb7\xc3\xd7\xc10n\xc5\x19\xf4\x98W\x19\x0b\xa0i`\x8c\"\xb7\x0ci\xae\x11N\x03\x9c\xcd\xde}L\x08\xa71,\xd2\x17lw\xa7/^\x12\xdf_\xbd\x00\xff\xc3\x00D\x94}\x80s%\xc4\x11\x15\x1f\x0f\xfc\xd1\x0br\x12\xf0\xc0p\x9aJ\xe8y\xb1\x87\xce>\xac\xb3o\xfc\xcf7\xee\xd3\xf3f\x8a\xe5X s\x04\xe3\xf4\x88}\xa8s\x00#)_\x19_H\xbd\xafS5\xfd\xf8\xd5,\xa2}\xc85\xa0\xb0Dj3\x1f,\xa7\xc0\xf8\x0c=R\xa2\xdeQi(\xd4.\xc3p\xb7\x91\x15\xaal\xfdJ\\]^t\x0b\xfb\xb5\xf0\xf5\x90K\x99p\x15\x00h\x8d*\xa3\xfd+C\\h\x91h\x1d[\x1cl2^\xd3\xad\xf7\xec\xd9,\xb9\"m\xed\xa1\xc8\x03\x8f\xa73\x7fQR\x0bP9\nsO\xd6\x19B\x0eM\x95\xbc\xcc\xcc\x9e7\x8e\xeeK\x86\xd7\x0b\xd4\xe0\x1dd\x7f\x980@\x1a\xad\xd7\x86\xfaVN\xb32L\xc9Z\xaa\xd5\x9d\xfd\x0bK\x80\xd2B-S\x88\xa2H\xc9\xbc\x16\xc9\x16\xfflN@g\xa2u\xa7F:\xff\xb4\xef\xafd\xbf'\x11\xc9ksh\xd6@\xb9p\xb0\xecX\x15\xf8y \xf8\xfb\xaeA\xbf\xa0}\xc8\x1d\x11j\xa0\x11\" 4\x07\xd6r\x91\x87\xab\x03\xb8C\xbdek{u\xf9\xaa\xcah\xacN\xa5\x17\xe4\x96\xfe\xb6\xd1\xb61x\xbc\xe1\x88\xdd\xd05\x87\xd3VT\xbd\xdf\x18,\xde\x04yk#!\xfdH\x92\x9e\x1b\xce\xceU\xce\xf4\xc3\x16\xab\x9d\xb1\xf3z\xa5I\xbb\x16\x9d@\xbe\xbd!0\xb5\xfa\xc3\xd3\x03\x00\xf36i\xa8\n\xde\xa7\x92\x94\xd6`\"S6\x13\x05\xe3l\x99d\xa6\x04\xaa\n\x0e_\xe3\x9e\x19\xc7\x8a\x02c1\x96\xbdN%\xb9\x961\xf9\xf1\x95!W\xcc\x009\x07\x99\xe6Sl\x1e\x9ev\x7f\xa5\x02\x8bU\xec\xd9N\xa2\x15e\t\xa2\xa0\x03qGk\xb53Q`Wz\t~\xcb\x19\xbcH\xf7=\xc0g\xbb~c\x99\xfa<\xa6\x96d\x88\x04'\xe4\xa3\x99\n<\x83\xcd\x9b\xcb\x94\xfe\x02p\xe6Z>S\x7f\x1d5\x8bL0\xd7\x05\xef\xe6\x89\xc5A?+\xcd#Wg\xee%%05\x11\x0ehWW\xfb\xa4\x99p`\r\xce\x0f\xe1&\x06c\xfd<\xf5\xe7\xacM\xbd\xab>\x92o\x1a\xa3\xd2\x9a6\x1b\x1e\xc87\xb9\xce{{Aq\x17\x0e\x99\xb8\xde7\x9f \xf5{\xec\xa2]@\xbf\xd9\xee\xb0\xe8\xc4\x90\x04\xfc\xf8\x00\x85H\xd0\x92\xdd\x85H\x98\xd9\x06\xc2`\xff\xe4\x00uw\xb3B\t\x9e\t\xb7}\xe0\x92G'\xe2\xef8\xa5<1\xc1\x91\xefY\x0b\\A3\xd0\xa6K\xa3\x88\xa1i\xaa \xc3\xd2\xde\xe5\xad\xcd1\xe3\x13\xee\xe7\xe1\xaeWN\xe8\xc6\x1b\x85\xf8wOy\xc4\xb7\x13\xe4\xbf\xe3\xfdrf}\xc5~\xa0\xac\xa4\xea\xee<\\\xe7\x1a\xe5rt\x84\x04\xfc\xc1#\x14\xd1\x80\x9a\xf83\x15\x87\x10\xd4qc\xc1\xadx73\t\xb2c\x1c~\xd8\xa5\x16\xb1\x12@\x04[\xf7\x0c\xbax\x82_&\xf4\xed9i=i\xb0\x1e'\xc6`\xc8\xcc\xc9\x80\xab\x0c\x94\x7f\x8b\x92^\xbdA\x8a \xcb\xf5\x9dS\xcf\x15C\x82\xa09\xd1\xb8U\xb8Tw\xad\xc5\xcd\xb7\xca\x83\x0f\xc4\x80\x04E,0\xfcW\xef\xcc\xcc\x194\xd2\xd7c\x90\xea\x0fK\xc6\\\xff\x8dP>Y\x18\x17wMf~F\xad\xee\x18\xe7!I\xed\x87\xe2k\xf0\x19\x7f\x8at\x86\x9b\x96m\xd5$\x1f\xe3\x16\xed\xd8\x8c)\xcc\xea\x805\xf6\xf6\x19\xd3\xd4\xa4\xb2E\xf0\x17\xb2\xc4\xab\xba\xd6L)S\xe4\xec\x9b\xab\x878\xb4\x1d\xd6\xa5\xac\xb1\xa3i3E\x8b\xcd\x92q\x0e^\x80\xcd\xac4\xf8\x8fEg\xfd.\xe0\xfa\x08\xe7\x9f\xbd\xac\xb0\x96\xb6\xc1\xb0?\x16\xa8\xfb`\x028\x96I\xa3\xde\x14K0\x8b\xee\xbf\xad\xe8\xeb\x87\xe6*\x81\xaa|\xa3'\x08\x05\xc0\xae\xf0\x83,\tk\x05\x9a0@\xd2!9f\xd3\"'\xb8\xd1\xaf8\x7fN\x1b\x00\x96\x8a\x10\x00\xa9\xddCRZ\x81\x89\xbcr\x1c\x84\xf3\x06\xab\xb3\xef\xc7|\xec\x9c\x9f\x9dI\x01\xde-\x15\x81^(\xe8\x90\xf7Cv\xa5\xe1a\xe9o\x0e\x18B7i\xcfi|p0\xcf^zj\x99`\x00\x00\x00\x00\xa0\x9dS\xa9\x94\xaf(0\x00\x01\xfd\r\xdb0\x00\x00\xa3\xc1\xf7G\xb1\xc4g\xfb\x02\x00\x00\x00\x00\x04YZ"

local compressed = xz_compress(nBodyC)
if compressed ~= nBodyCLXZ then
    error("Compression failed")
end

local decompressed = xz_decompress(nBodyCLXZ)
if decompressed ~= nBodyC then
    error("Decompression failed")
end

local decompressed = xz_decompress(nBodyCXZ)
if decompressed ~= nBodyC then
    error("Decompression failed")
end

end

bench.runCode(test, "xz")
