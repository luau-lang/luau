local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

	local RC =
	{
		0x0000000000000001i, 0x0000000000008082i, 0x800000000000808ai, 0x8000000080008000i,
		0x000000000000808bi, 0x0000000080000001i, 0x8000000080008081i, 0x8000000000008009i,
		0x000000000000008ai, 0x0000000000000088i, 0x0000000080008009i, 0x000000008000000ai,
		0x000000008000808bi, 0x800000000000008bi, 0x8000000000008089i, 0x8000000000008003i,
		0x8000000000008002i, 0x8000000000000080i, 0x000000000000800ai, 0x800000008000000ai,
		0x8000000080008081i, 0x8000000000008080i, 0x0000000080000001i, 0x8000000080008008i,
	}

	local function sha3_256(msg)
		-- pad10*1 with SHA-3 domain separator 0x06
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

		-- 25-lane state (S<col><row>), little-endian interpretation
		local S00, S10, S20, S30, S40 = 0i, 0i, 0i, 0i, 0i
		local S01, S11, S21, S31, S41 = 0i, 0i, 0i, 0i, 0i
		local S02, S12, S22, S32, S42 = 0i, 0i, 0i, 0i, 0i
		local S03, S13, S23, S33, S43 = 0i, 0i, 0i, 0i, 0i
		local S04, S14, S24, S34, S44 = 0i, 0i, 0i, 0i, 0i

		for blockOffset = 0, paddedLen - 1, rateBytes do
			-- absorb 17 lanes (136 bytes) of message
			S00 = integer.bxor(S00, buffer.readinteger(buf, blockOffset))
			S10 = integer.bxor(S10, buffer.readinteger(buf, blockOffset + 8))
			S20 = integer.bxor(S20, buffer.readinteger(buf, blockOffset + 16))
			S30 = integer.bxor(S30, buffer.readinteger(buf, blockOffset + 24))
			S40 = integer.bxor(S40, buffer.readinteger(buf, blockOffset + 32))
			S01 = integer.bxor(S01, buffer.readinteger(buf, blockOffset + 40))
			S11 = integer.bxor(S11, buffer.readinteger(buf, blockOffset + 48))
			S21 = integer.bxor(S21, buffer.readinteger(buf, blockOffset + 56))
			S31 = integer.bxor(S31, buffer.readinteger(buf, blockOffset + 64))
			S41 = integer.bxor(S41, buffer.readinteger(buf, blockOffset + 72))
			S02 = integer.bxor(S02, buffer.readinteger(buf, blockOffset + 80))
			S12 = integer.bxor(S12, buffer.readinteger(buf, blockOffset + 88))
			S22 = integer.bxor(S22, buffer.readinteger(buf, blockOffset + 96))
			S32 = integer.bxor(S32, buffer.readinteger(buf, blockOffset + 104))
			S42 = integer.bxor(S42, buffer.readinteger(buf, blockOffset + 112))
			S03 = integer.bxor(S03, buffer.readinteger(buf, blockOffset + 120))
			S13 = integer.bxor(S13, buffer.readinteger(buf, blockOffset + 128))

			for round = 1, 24 do
				-- THETA
				local C0 = integer.bxor(S00, S01, S02, S03, S04)
				local C1 = integer.bxor(S10, S11, S12, S13, S14)
				local C2 = integer.bxor(S20, S21, S22, S23, S24)
				local C3 = integer.bxor(S30, S31, S32, S33, S34)
				local C4 = integer.bxor(S40, S41, S42, S43, S44)

				local D0 = integer.bxor(C4, integer.lrotate(C1, 1i))
				local D1 = integer.bxor(C0, integer.lrotate(C2, 1i))
				local D2 = integer.bxor(C1, integer.lrotate(C3, 1i))
				local D3 = integer.bxor(C2, integer.lrotate(C4, 1i))
				local D4 = integer.bxor(C3, integer.lrotate(C0, 1i))

				-- RHO + PI: B[X,Y] = ROT(S[(3Y+X) mod 5, X] XOR D[(3Y+X) mod 5], r[..])
				local B00 = integer.bxor(S00, D0)
				local B10 = integer.lrotate(integer.bxor(S11, D1), 44i)
				local B20 = integer.lrotate(integer.bxor(S22, D2), 43i)
				local B30 = integer.lrotate(integer.bxor(S33, D3), 21i)
				local B40 = integer.lrotate(integer.bxor(S44, D4), 14i)

				local B01 = integer.lrotate(integer.bxor(S30, D3), 28i)
				local B11 = integer.lrotate(integer.bxor(S41, D4), 20i)
				local B21 = integer.lrotate(integer.bxor(S02, D0), 3i)
				local B31 = integer.lrotate(integer.bxor(S13, D1), 45i)
				local B41 = integer.lrotate(integer.bxor(S24, D2), 61i)

				local B02 = integer.lrotate(integer.bxor(S10, D1), 1i)
				local B12 = integer.lrotate(integer.bxor(S21, D2), 6i)
				local B22 = integer.lrotate(integer.bxor(S32, D3), 25i)
				local B32 = integer.lrotate(integer.bxor(S43, D4), 8i)
				local B42 = integer.lrotate(integer.bxor(S04, D0), 18i)

				local B03 = integer.lrotate(integer.bxor(S40, D4), 27i)
				local B13 = integer.lrotate(integer.bxor(S01, D0), 36i)
				local B23 = integer.lrotate(integer.bxor(S12, D1), 10i)
				local B33 = integer.lrotate(integer.bxor(S23, D2), 15i)
				local B43 = integer.lrotate(integer.bxor(S34, D3), 56i)

				local B04 = integer.lrotate(integer.bxor(S20, D2), 62i)
				local B14 = integer.lrotate(integer.bxor(S31, D3), 55i)
				local B24 = integer.lrotate(integer.bxor(S42, D4), 39i)
				local B34 = integer.lrotate(integer.bxor(S03, D0), 41i)
				local B44 = integer.lrotate(integer.bxor(S14, D1), 2i)

				-- CHI
				S00 = integer.bxor(B00, integer.band(integer.bnot(B10), B20))
				S10 = integer.bxor(B10, integer.band(integer.bnot(B20), B30))
				S20 = integer.bxor(B20, integer.band(integer.bnot(B30), B40))
				S30 = integer.bxor(B30, integer.band(integer.bnot(B40), B00))
				S40 = integer.bxor(B40, integer.band(integer.bnot(B00), B10))

				S01 = integer.bxor(B01, integer.band(integer.bnot(B11), B21))
				S11 = integer.bxor(B11, integer.band(integer.bnot(B21), B31))
				S21 = integer.bxor(B21, integer.band(integer.bnot(B31), B41))
				S31 = integer.bxor(B31, integer.band(integer.bnot(B41), B01))
				S41 = integer.bxor(B41, integer.band(integer.bnot(B01), B11))

				S02 = integer.bxor(B02, integer.band(integer.bnot(B12), B22))
				S12 = integer.bxor(B12, integer.band(integer.bnot(B22), B32))
				S22 = integer.bxor(B22, integer.band(integer.bnot(B32), B42))
				S32 = integer.bxor(B32, integer.band(integer.bnot(B42), B02))
				S42 = integer.bxor(B42, integer.band(integer.bnot(B02), B12))

				S03 = integer.bxor(B03, integer.band(integer.bnot(B13), B23))
				S13 = integer.bxor(B13, integer.band(integer.bnot(B23), B33))
				S23 = integer.bxor(B23, integer.band(integer.bnot(B33), B43))
				S33 = integer.bxor(B33, integer.band(integer.bnot(B43), B03))
				S43 = integer.bxor(B43, integer.band(integer.bnot(B03), B13))

				S04 = integer.bxor(B04, integer.band(integer.bnot(B14), B24))
				S14 = integer.bxor(B14, integer.band(integer.bnot(B24), B34))
				S24 = integer.bxor(B24, integer.band(integer.bnot(B34), B44))
				S34 = integer.bxor(B34, integer.band(integer.bnot(B44), B04))
				S44 = integer.bxor(B44, integer.band(integer.bnot(B04), B14))

				-- IOTA
				S00 = integer.bxor(S00, RC[round])
			end
		end

		-- squeeze 32 bytes (first 4 lanes, little-endian bytes -> hex via bswap)
		return string.format(
			"%016x%016x%016x%016x",
			integer.bswap(S00), integer.bswap(S10), integer.bswap(S20), integer.bswap(S30)
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

bench.runCode(test, "sha3_256_int64")
