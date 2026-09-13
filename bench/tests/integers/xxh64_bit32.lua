local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

	-- 64-bit wrapping add: (ah:al) + (bh:bl) -> (rh, rl)
	local function add64(ah, al, bh, bl)
		local lo = al + bl
		local hi = ah + bh + lo // 0x100000000
		return bit32.bor(hi, 0), bit32.bor(lo, 0)
	end

	-- 64-bit wrapping subtract: (ah:al) - (bh:bl) -> (rh, rl)
	local function sub64(ah, al, bh, bl)
		local lo = al - bl
		local borrow = 0
		if lo < 0 then
			lo += 0x100000000
			borrow = 1
		end
		local hi = ah - bh - borrow
		if hi < 0 then hi += 0x100000000 end
		return bit32.bor(hi, 0), bit32.bor(lo, 0)
	end

	-- 64-bit wrapping multiply via 16-bit limb schoolbook with carry propagation.
	-- All intermediates stay within double precision (< 2^53).
	local function mul64(ah, al, bh, bl)
		local a0 = bit32.band(al, 0xFFFF)
		local a1 = bit32.rshift(al, 16)
		local a2 = bit32.band(ah, 0xFFFF)
		local a3 = bit32.rshift(ah, 16)
		local b0 = bit32.band(bl, 0xFFFF)
		local b1 = bit32.rshift(bl, 16)
		local b2 = bit32.band(bh, 0xFFFF)
		local b3 = bit32.rshift(bh, 16)

		-- Column sums at each 16-bit position (positions 4+ are discarded)
		local c0 = a0 * b0
		local c1 = a1 * b0 + a0 * b1
		local c2 = a2 * b0 + a1 * b1 + a0 * b2
		local c3 = a3 * b0 + a2 * b1 + a1 * b2 + a0 * b3

		-- Propagate carries through 16-bit columns using arithmetic (not bit32)
		-- to avoid truncation on values > 2^32
		local r0 = c0 % 0x10000
		c1 = c1 + (c0 - r0) / 0x10000
		local r1 = c1 % 0x10000
		c2 = c2 + (c1 - r1) / 0x10000
		local r2 = c2 % 0x10000
		c3 = c3 + (c2 - r2) / 0x10000
		local r3 = c3 % 0x10000

		return r2 + r3 * 0x10000, r0 + r1 * 0x10000
	end

	-- 64-bit left rotate by n (0 < n < 32)
	local function lrotate64(ah, al, n)
		return bit32.bor(bit32.lshift(ah, n), bit32.rshift(al, 32 - n)),
		       bit32.bor(bit32.lshift(al, n), bit32.rshift(ah, 32 - n))
	end

	-- 64-bit xor
	local function xor64(ah, al, bh, bl)
		return bit32.bxor(ah, bh), bit32.bxor(al, bl)
	end

	-- Constants split into (hi, lo)
	local P1h, P1l = 0x9E3779B1, 0x85EBCA87
	local P2h, P2l = 0xC2B2AE3D, 0x27D4EB4F
	local P3h, P3l = 0x165667B1, 0x9E3779F9
	local P4h, P4l = 0x85EBCA77, 0xC2B2AE63
	local P5h, P5l = 0x27D4EB2F, 0x165667C5

	-- XXH64 round: acc = mul(lrotate(add(acc, mul(lane, P2)), 31), P1)
	local function round(ach, acl, lanh, lanl)
		local th, tl = add64(ach, acl, mul64(lanh, lanl, P2h, P2l))
		th, tl = lrotate64(th, tl, 31)
		return mul64(th, tl, P1h, P1l)
	end

	local function mergeRound(hh, hl, ach, acl)
		local th, tl = mul64(ach, acl, P2h, P2l)
		th, tl = lrotate64(th, tl, 31)
		th, tl = mul64(th, tl, P1h, P1l)
		hh, hl = xor64(hh, hl, th, tl)
		hh, hl = mul64(hh, hl, P1h, P1l)
		return add64(hh, hl, P4h, P4l)
	end

	local function xxh64(buf, seedh, seedl)
		local len = buffer.len(buf)
		local offset = 0
		local hh, hl

		if len >= 32 then
			local a1h, a1l = add64(seedh, seedl, P1h, P1l)
			a1h, a1l = add64(a1h, a1l, P2h, P2l)
			local a2h, a2l = add64(seedh, seedl, P2h, P2l)
			local a3h, a3l = seedh, seedl
			local a4h, a4l = sub64(seedh, seedl, P1h, P1l)

			while offset <= len - 32 do
				-- read64: hi = readu32(off+4), lo = readu32(off) (both LE, matching readinteger)
				a1h, a1l = round(a1h, a1l, buffer.readu32(buf, offset + 4), buffer.readu32(buf, offset))
				a2h, a2l = round(a2h, a2l, buffer.readu32(buf, offset + 12), buffer.readu32(buf, offset + 8))
				a3h, a3l = round(a3h, a3l, buffer.readu32(buf, offset + 20), buffer.readu32(buf, offset + 16))
				a4h, a4l = round(a4h, a4l, buffer.readu32(buf, offset + 28), buffer.readu32(buf, offset + 24))
				offset += 32
			end

			local r1h, r1l = lrotate64(a1h, a1l, 1)
			local r2h, r2l = lrotate64(a2h, a2l, 7)
			local r3h, r3l = lrotate64(a3h, a3l, 12)
			local r4h, r4l = lrotate64(a4h, a4l, 18)
			hh, hl = add64(r1h, r1l, r2h, r2l)
			hh, hl = add64(hh, hl, r3h, r3l)
			hh, hl = add64(hh, hl, r4h, r4l)

			hh, hl = mergeRound(hh, hl, a1h, a1l)
			hh, hl = mergeRound(hh, hl, a2h, a2l)
			hh, hl = mergeRound(hh, hl, a3h, a3l)
			hh, hl = mergeRound(hh, hl, a4h, a4l)
		else
			hh, hl = add64(seedh, seedl, P5h, P5l)
		end

		hh, hl = add64(hh, hl, 0, len)

		-- 8-byte tail lanes
		while offset <= len - 8 do
			local lanh = buffer.readu32(buf, offset + 4)
			local lanl = buffer.readu32(buf, offset)
			local th, tl = mul64(lanh, lanl, P2h, P2l)
			th, tl = lrotate64(th, tl, 31)
			th, tl = mul64(th, tl, P1h, P1l)
			hh, hl = xor64(hh, hl, th, tl)
			hh, hl = lrotate64(hh, hl, 27)
			hh, hl = mul64(hh, hl, P1h, P1l)
			hh, hl = add64(hh, hl, P4h, P4l)
			offset += 8
		end

		-- 4-byte tail
		if offset <= len - 4 then
			local v = buffer.readu32(buf, offset)
			local ph, pl = mul64(0, v, P1h, P1l)
			hh, hl = xor64(hh, hl, ph, pl)
			hh, hl = lrotate64(hh, hl, 23)
			hh, hl = mul64(hh, hl, P2h, P2l)
			hh, hl = add64(hh, hl, P3h, P3l)
			offset += 4
		end

		-- 1-byte tail
		while offset < len do
			local b = buffer.readu8(buf, offset)
			local ph, pl = mul64(0, b, P5h, P5l)
			hh, hl = xor64(hh, hl, ph, pl)
			hh, hl = lrotate64(hh, hl, 11)
			hh, hl = mul64(hh, hl, P1h, P1l)
			offset += 1
		end

		-- avalanche: hash ^= hash >> 33; hash *= P2; hash ^= hash >> 29; hash *= P3; hash ^= hash >> 32
		hh, hl = xor64(hh, hl, 0, bit32.rshift(hh, 1))
		hh, hl = mul64(hh, hl, P2h, P2l)
		hh, hl = xor64(hh, hl, bit32.rshift(hh, 29), bit32.bor(bit32.rshift(hl, 29), bit32.lshift(hh, 3)))
		hh, hl = mul64(hh, hl, P3h, P3l)
		hh, hl = xor64(hh, hl, 0, hh)

		return string.format("%08x%08x", hh, hl)
	end

	local input = buffer.fromstring(string.rep(".", 1e3))

	local ts0 = os.clock()

	for i = 1, 2500 do
		local res = xxh64(input, 0, 0)
		assert(res == "1d27c09e95a70d9e")
	end

	local ts1 = os.clock()

	return ts1 - ts0
end

bench.runCode(test, "xxh64_bit32")
