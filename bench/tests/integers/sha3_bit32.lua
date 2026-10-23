local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

	local RC_HI = {
		0x00000000, 0x00000000, 0x80000000, 0x80000000,
		0x00000000, 0x00000000, 0x80000000, 0x80000000,
		0x00000000, 0x00000000, 0x00000000, 0x00000000,
		0x00000000, 0x80000000, 0x80000000, 0x80000000,
		0x80000000, 0x80000000, 0x00000000, 0x80000000,
		0x80000000, 0x80000000, 0x00000000, 0x80000000,
	}

	local RC_LO = {
		0x00000001, 0x00008082, 0x0000808a, 0x80008000,
		0x0000808b, 0x80000001, 0x80008081, 0x00008009,
		0x0000008a, 0x00000088, 0x80008009, 0x8000000a,
		0x8000808b, 0x0000008b, 0x00008089, 0x00008003,
		0x00008002, 0x00000080, 0x0000800a, 0x8000000a,
		0x80008081, 0x00008080, 0x80000001, 0x80008008,
	}

	-- 64-bit left rotate by n (0 < n < 64)
	local function lrotate64(h, l, n)
		if n < 32 then
			return bit32.bor(bit32.lshift(h, n), bit32.rshift(l, 32 - n)),
			       bit32.bor(bit32.lshift(l, n), bit32.rshift(h, 32 - n))
		elseif n == 32 then
			return l, h
		else
			local m = n - 32
			return bit32.bor(bit32.lshift(l, m), bit32.rshift(h, 32 - m)),
			       bit32.bor(bit32.lshift(h, m), bit32.rshift(l, 32 - m))
		end
	end

	local function sha3_256(msg)
		local msgLen = #msg
		local rateBytes = 136

		local pad = rateBytes - (msgLen % rateBytes)
		local paddedLen = msgLen + pad

		local buf = buffer.create(paddedLen)
		buffer.writestring(buf, 0, msg)
		if pad == 1 then
			buffer.writeu8(buf, msgLen, 0x86)
		else
			buffer.writeu8(buf, msgLen, 0x06)
			buffer.writeu8(buf, paddedLen - 1, 0x80)
		end

		-- 25-lane state as (hi, lo) pairs
		local S00h, S00l, S10h, S10l, S20h, S20l, S30h, S30l, S40h, S40l = 0,0,0,0,0,0,0,0,0,0
		local S01h, S01l, S11h, S11l, S21h, S21l, S31h, S31l, S41h, S41l = 0,0,0,0,0,0,0,0,0,0
		local S02h, S02l, S12h, S12l, S22h, S22l, S32h, S32l, S42h, S42l = 0,0,0,0,0,0,0,0,0,0
		local S03h, S03l, S13h, S13l, S23h, S23l, S33h, S33l, S43h, S43l = 0,0,0,0,0,0,0,0,0,0
		local S04h, S04l, S14h, S14l, S24h, S24l, S34h, S34l, S44h, S44l = 0,0,0,0,0,0,0,0,0,0

		for blockOffset = 0, paddedLen - 1, rateBytes do
			-- absorb 17 lanes (136 bytes) — LE 64-bit read = (readu32 hi at +4, readu32 lo at +0)
			S00l = bit32.bxor(S00l, buffer.readu32(buf, blockOffset)); S00h = bit32.bxor(S00h, buffer.readu32(buf, blockOffset + 4))
			S10l = bit32.bxor(S10l, buffer.readu32(buf, blockOffset + 8)); S10h = bit32.bxor(S10h, buffer.readu32(buf, blockOffset + 12))
			S20l = bit32.bxor(S20l, buffer.readu32(buf, blockOffset + 16)); S20h = bit32.bxor(S20h, buffer.readu32(buf, blockOffset + 20))
			S30l = bit32.bxor(S30l, buffer.readu32(buf, blockOffset + 24)); S30h = bit32.bxor(S30h, buffer.readu32(buf, blockOffset + 28))
			S40l = bit32.bxor(S40l, buffer.readu32(buf, blockOffset + 32)); S40h = bit32.bxor(S40h, buffer.readu32(buf, blockOffset + 36))
			S01l = bit32.bxor(S01l, buffer.readu32(buf, blockOffset + 40)); S01h = bit32.bxor(S01h, buffer.readu32(buf, blockOffset + 44))
			S11l = bit32.bxor(S11l, buffer.readu32(buf, blockOffset + 48)); S11h = bit32.bxor(S11h, buffer.readu32(buf, blockOffset + 52))
			S21l = bit32.bxor(S21l, buffer.readu32(buf, blockOffset + 56)); S21h = bit32.bxor(S21h, buffer.readu32(buf, blockOffset + 60))
			S31l = bit32.bxor(S31l, buffer.readu32(buf, blockOffset + 64)); S31h = bit32.bxor(S31h, buffer.readu32(buf, blockOffset + 68))
			S41l = bit32.bxor(S41l, buffer.readu32(buf, blockOffset + 72)); S41h = bit32.bxor(S41h, buffer.readu32(buf, blockOffset + 76))
			S02l = bit32.bxor(S02l, buffer.readu32(buf, blockOffset + 80)); S02h = bit32.bxor(S02h, buffer.readu32(buf, blockOffset + 84))
			S12l = bit32.bxor(S12l, buffer.readu32(buf, blockOffset + 88)); S12h = bit32.bxor(S12h, buffer.readu32(buf, blockOffset + 92))
			S22l = bit32.bxor(S22l, buffer.readu32(buf, blockOffset + 96)); S22h = bit32.bxor(S22h, buffer.readu32(buf, blockOffset + 100))
			S32l = bit32.bxor(S32l, buffer.readu32(buf, blockOffset + 104)); S32h = bit32.bxor(S32h, buffer.readu32(buf, blockOffset + 108))
			S42l = bit32.bxor(S42l, buffer.readu32(buf, blockOffset + 112)); S42h = bit32.bxor(S42h, buffer.readu32(buf, blockOffset + 116))
			S03l = bit32.bxor(S03l, buffer.readu32(buf, blockOffset + 120)); S03h = bit32.bxor(S03h, buffer.readu32(buf, blockOffset + 124))
			S13l = bit32.bxor(S13l, buffer.readu32(buf, blockOffset + 128)); S13h = bit32.bxor(S13h, buffer.readu32(buf, blockOffset + 132))

			for round = 1, 24 do
				-- THETA
				local C0h = bit32.bxor(S00h, S01h, S02h, S03h, S04h); local C0l = bit32.bxor(S00l, S01l, S02l, S03l, S04l)
				local C1h = bit32.bxor(S10h, S11h, S12h, S13h, S14h); local C1l = bit32.bxor(S10l, S11l, S12l, S13l, S14l)
				local C2h = bit32.bxor(S20h, S21h, S22h, S23h, S24h); local C2l = bit32.bxor(S20l, S21l, S22l, S23l, S24l)
				local C3h = bit32.bxor(S30h, S31h, S32h, S33h, S34h); local C3l = bit32.bxor(S30l, S31l, S32l, S33l, S34l)
				local C4h = bit32.bxor(S40h, S41h, S42h, S43h, S44h); local C4l = bit32.bxor(S40l, S41l, S42l, S43l, S44l)

				-- D[i] = C[i-1] XOR lrotate(C[i+1], 1) — rotation by 1 (< 32)
				local D0h = bit32.bxor(C4h, bit32.bor(bit32.lshift(C1h, 1), bit32.rshift(C1l, 31)))
				local D0l = bit32.bxor(C4l, bit32.bor(bit32.lshift(C1l, 1), bit32.rshift(C1h, 31)))
				local D1h = bit32.bxor(C0h, bit32.bor(bit32.lshift(C2h, 1), bit32.rshift(C2l, 31)))
				local D1l = bit32.bxor(C0l, bit32.bor(bit32.lshift(C2l, 1), bit32.rshift(C2h, 31)))
				local D2h = bit32.bxor(C1h, bit32.bor(bit32.lshift(C3h, 1), bit32.rshift(C3l, 31)))
				local D2l = bit32.bxor(C1l, bit32.bor(bit32.lshift(C3l, 1), bit32.rshift(C3h, 31)))
				local D3h = bit32.bxor(C2h, bit32.bor(bit32.lshift(C4h, 1), bit32.rshift(C4l, 31)))
				local D3l = bit32.bxor(C2l, bit32.bor(bit32.lshift(C4l, 1), bit32.rshift(C4h, 31)))
				local D4h = bit32.bxor(C3h, bit32.bor(bit32.lshift(C0h, 1), bit32.rshift(C0l, 31)))
				local D4l = bit32.bxor(C3l, bit32.bor(bit32.lshift(C0l, 1), bit32.rshift(C0h, 31)))

				-- RHO + PI
				local B00h, B00l = bit32.bxor(S00h, D0h), bit32.bxor(S00l, D0l) -- rot 0
				local B10h, B10l = lrotate64(bit32.bxor(S11h, D1h), bit32.bxor(S11l, D1l), 44)
				local B20h, B20l = lrotate64(bit32.bxor(S22h, D2h), bit32.bxor(S22l, D2l), 43)
				local B30h, B30l = lrotate64(bit32.bxor(S33h, D3h), bit32.bxor(S33l, D3l), 21)
				local B40h, B40l = lrotate64(bit32.bxor(S44h, D4h), bit32.bxor(S44l, D4l), 14)

				local B01h, B01l = lrotate64(bit32.bxor(S30h, D3h), bit32.bxor(S30l, D3l), 28)
				local B11h, B11l = lrotate64(bit32.bxor(S41h, D4h), bit32.bxor(S41l, D4l), 20)
				local B21h, B21l = lrotate64(bit32.bxor(S02h, D0h), bit32.bxor(S02l, D0l), 3)
				local B31h, B31l = lrotate64(bit32.bxor(S13h, D1h), bit32.bxor(S13l, D1l), 45)
				local B41h, B41l = lrotate64(bit32.bxor(S24h, D2h), bit32.bxor(S24l, D2l), 61)

				local B02h, B02l = lrotate64(bit32.bxor(S10h, D1h), bit32.bxor(S10l, D1l), 1)
				local B12h, B12l = lrotate64(bit32.bxor(S21h, D2h), bit32.bxor(S21l, D2l), 6)
				local B22h, B22l = lrotate64(bit32.bxor(S32h, D3h), bit32.bxor(S32l, D3l), 25)
				local B32h, B32l = lrotate64(bit32.bxor(S43h, D4h), bit32.bxor(S43l, D4l), 8)
				local B42h, B42l = lrotate64(bit32.bxor(S04h, D0h), bit32.bxor(S04l, D0l), 18)

				local B03h, B03l = lrotate64(bit32.bxor(S40h, D4h), bit32.bxor(S40l, D4l), 27)
				local B13h, B13l = lrotate64(bit32.bxor(S01h, D0h), bit32.bxor(S01l, D0l), 36)
				local B23h, B23l = lrotate64(bit32.bxor(S12h, D1h), bit32.bxor(S12l, D1l), 10)
				local B33h, B33l = lrotate64(bit32.bxor(S23h, D2h), bit32.bxor(S23l, D2l), 15)
				local B43h, B43l = lrotate64(bit32.bxor(S34h, D3h), bit32.bxor(S34l, D3l), 56)

				local B04h, B04l = lrotate64(bit32.bxor(S20h, D2h), bit32.bxor(S20l, D2l), 62)
				local B14h, B14l = lrotate64(bit32.bxor(S31h, D3h), bit32.bxor(S31l, D3l), 55)
				local B24h, B24l = lrotate64(bit32.bxor(S42h, D4h), bit32.bxor(S42l, D4l), 39)
				local B34h, B34l = lrotate64(bit32.bxor(S03h, D0h), bit32.bxor(S03l, D0l), 41)
				local B44h, B44l = lrotate64(bit32.bxor(S14h, D1h), bit32.bxor(S14l, D1l), 2)

				-- CHI
				S00h = bit32.bxor(B00h, bit32.band(bit32.bnot(B10h), B20h)); S00l = bit32.bxor(B00l, bit32.band(bit32.bnot(B10l), B20l))
				S10h = bit32.bxor(B10h, bit32.band(bit32.bnot(B20h), B30h)); S10l = bit32.bxor(B10l, bit32.band(bit32.bnot(B20l), B30l))
				S20h = bit32.bxor(B20h, bit32.band(bit32.bnot(B30h), B40h)); S20l = bit32.bxor(B20l, bit32.band(bit32.bnot(B30l), B40l))
				S30h = bit32.bxor(B30h, bit32.band(bit32.bnot(B40h), B00h)); S30l = bit32.bxor(B30l, bit32.band(bit32.bnot(B40l), B00l))
				S40h = bit32.bxor(B40h, bit32.band(bit32.bnot(B00h), B10h)); S40l = bit32.bxor(B40l, bit32.band(bit32.bnot(B00l), B10l))

				S01h = bit32.bxor(B01h, bit32.band(bit32.bnot(B11h), B21h)); S01l = bit32.bxor(B01l, bit32.band(bit32.bnot(B11l), B21l))
				S11h = bit32.bxor(B11h, bit32.band(bit32.bnot(B21h), B31h)); S11l = bit32.bxor(B11l, bit32.band(bit32.bnot(B21l), B31l))
				S21h = bit32.bxor(B21h, bit32.band(bit32.bnot(B31h), B41h)); S21l = bit32.bxor(B21l, bit32.band(bit32.bnot(B31l), B41l))
				S31h = bit32.bxor(B31h, bit32.band(bit32.bnot(B41h), B01h)); S31l = bit32.bxor(B31l, bit32.band(bit32.bnot(B41l), B01l))
				S41h = bit32.bxor(B41h, bit32.band(bit32.bnot(B01h), B11h)); S41l = bit32.bxor(B41l, bit32.band(bit32.bnot(B01l), B11l))

				S02h = bit32.bxor(B02h, bit32.band(bit32.bnot(B12h), B22h)); S02l = bit32.bxor(B02l, bit32.band(bit32.bnot(B12l), B22l))
				S12h = bit32.bxor(B12h, bit32.band(bit32.bnot(B22h), B32h)); S12l = bit32.bxor(B12l, bit32.band(bit32.bnot(B22l), B32l))
				S22h = bit32.bxor(B22h, bit32.band(bit32.bnot(B32h), B42h)); S22l = bit32.bxor(B22l, bit32.band(bit32.bnot(B32l), B42l))
				S32h = bit32.bxor(B32h, bit32.band(bit32.bnot(B42h), B02h)); S32l = bit32.bxor(B32l, bit32.band(bit32.bnot(B42l), B02l))
				S42h = bit32.bxor(B42h, bit32.band(bit32.bnot(B02h), B12h)); S42l = bit32.bxor(B42l, bit32.band(bit32.bnot(B02l), B12l))

				S03h = bit32.bxor(B03h, bit32.band(bit32.bnot(B13h), B23h)); S03l = bit32.bxor(B03l, bit32.band(bit32.bnot(B13l), B23l))
				S13h = bit32.bxor(B13h, bit32.band(bit32.bnot(B23h), B33h)); S13l = bit32.bxor(B13l, bit32.band(bit32.bnot(B23l), B33l))
				S23h = bit32.bxor(B23h, bit32.band(bit32.bnot(B33h), B43h)); S23l = bit32.bxor(B23l, bit32.band(bit32.bnot(B33l), B43l))
				S33h = bit32.bxor(B33h, bit32.band(bit32.bnot(B43h), B03h)); S33l = bit32.bxor(B33l, bit32.band(bit32.bnot(B43l), B03l))
				S43h = bit32.bxor(B43h, bit32.band(bit32.bnot(B03h), B13h)); S43l = bit32.bxor(B43l, bit32.band(bit32.bnot(B03l), B13l))

				S04h = bit32.bxor(B04h, bit32.band(bit32.bnot(B14h), B24h)); S04l = bit32.bxor(B04l, bit32.band(bit32.bnot(B14l), B24l))
				S14h = bit32.bxor(B14h, bit32.band(bit32.bnot(B24h), B34h)); S14l = bit32.bxor(B14l, bit32.band(bit32.bnot(B24l), B34l))
				S24h = bit32.bxor(B24h, bit32.band(bit32.bnot(B34h), B44h)); S24l = bit32.bxor(B24l, bit32.band(bit32.bnot(B34l), B44l))
				S34h = bit32.bxor(B34h, bit32.band(bit32.bnot(B44h), B04h)); S34l = bit32.bxor(B34l, bit32.band(bit32.bnot(B44l), B04l))
				S44h = bit32.bxor(B44h, bit32.band(bit32.bnot(B04h), B14h)); S44l = bit32.bxor(B44l, bit32.band(bit32.bnot(B04l), B14l))

				-- IOTA
				S00h = bit32.bxor(S00h, RC_HI[round])
				S00l = bit32.bxor(S00l, RC_LO[round])
			end
		end

		-- squeeze 32 bytes: bswap each lane for big-endian hex
		return string.format(
			"%08x%08x%08x%08x%08x%08x%08x%08x",
			bit32.byteswap(S00l), bit32.byteswap(S00h), bit32.byteswap(S10l), bit32.byteswap(S10h),
			bit32.byteswap(S20l), bit32.byteswap(S20h), bit32.byteswap(S30l), bit32.byteswap(S30h)
		)
	end

	local input = string.rep(".", 1e3)

	local ts0 = os.clock()

	for i = 1, 100 do
		local res = sha3_256(input)
		assert(res == "778f41ec28c470b1947cf8785207ca0e5829b3b04966283c93cd2f2cd37c831c")
	end

	local ts1 = os.clock()

	return ts1 - ts0
end

bench.runCode(test, "sha3_256_bit32")
