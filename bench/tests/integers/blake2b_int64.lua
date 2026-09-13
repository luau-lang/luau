local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

	local IV1 = 0x6a09e667f3bcc908i
	local IV2 = 0xbb67ae8584caa73bi
	local IV3 = 0x3c6ef372fe94f82bi
	local IV4 = 0xa54ff53a5f1d36f1i
	local IV5 = 0x510e527fade682d1i
	local IV6 = 0x9b05688c2b3e6c1fi
	local IV7 = 0x1f83d9abfb41bd6bi
	local IV8 = 0x5be0cd19137e2179i

	local INV_MASK = 0xffffffffffffffffi

	local SIGMA =
	{
		{  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16 },
		{ 15, 11,  5,  9, 10, 16, 14,  7,  2, 13,  1,  3, 12,  8,  6,  4 },
		{ 12,  9, 13,  1,  6,  3, 16, 14, 11, 15,  4,  7,  8,  2, 10,  5 },
		{  8, 10,  4,  2, 14, 13, 12, 15,  3,  7,  6, 11,  5,  1, 16,  9 },
		{ 10,  1,  6,  8,  3,  5, 11, 16, 15,  2, 12, 13,  7,  9,  4, 14 },
		{  3, 13,  7, 11,  1, 12,  9,  4,  5, 14,  8,  6, 16, 15,  2, 10 },
		{ 13,  6,  2, 16, 15, 14,  5, 11,  1,  8,  7,  4, 10,  3,  9, 12 },
		{ 14, 12,  8, 15, 13,  2,  4, 10,  6,  1, 16,  5,  9,  7,  3, 11 },
		{  7, 16, 15, 10, 12,  4,  1,  9, 13,  3, 14,  8,  2,  5, 11,  6 },
		{ 11,  3,  9,  5,  8,  7,  2,  6, 16, 12, 10, 15,  4, 13, 14,  1 },
		{  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16 },
		{ 15, 11,  5,  9, 10, 16, 14,  7,  2, 13,  1,  3, 12,  8,  6,  4 },
	}

	local function compress(h, M, t, isLast)
		local V0, V1, V2, V3 = h[1], h[2], h[3], h[4]
		local V4, V5, V6, V7 = h[5], h[6], h[7], h[8]
		local V8, V9, V10, V11 = IV1, IV2, IV3, IV4
		local V12 = integer.bxor(IV5, t)
		local V13 = IV6
		local V14 = if isLast then integer.bxor(IV7, INV_MASK) else IV7
		local V15 = IV8

		for r = 1, 12 do
			local s = SIGMA[r]

			-- column step
			V0 = integer.add(integer.add(V0, V4), M[s[1]])
			V12 = integer.rrotate(integer.bxor(V12, V0), 32i)
			V8 = integer.add(V8, V12)
			V4 = integer.rrotate(integer.bxor(V4, V8), 24i)
			V0 = integer.add(integer.add(V0, V4), M[s[2]])
			V12 = integer.rrotate(integer.bxor(V12, V0), 16i)
			V8 = integer.add(V8, V12)
			V4 = integer.rrotate(integer.bxor(V4, V8), 63i)

			V1 = integer.add(integer.add(V1, V5), M[s[3]])
			V13 = integer.rrotate(integer.bxor(V13, V1), 32i)
			V9 = integer.add(V9, V13)
			V5 = integer.rrotate(integer.bxor(V5, V9), 24i)
			V1 = integer.add(integer.add(V1, V5), M[s[4]])
			V13 = integer.rrotate(integer.bxor(V13, V1), 16i)
			V9 = integer.add(V9, V13)
			V5 = integer.rrotate(integer.bxor(V5, V9), 63i)

			V2 = integer.add(integer.add(V2, V6), M[s[5]])
			V14 = integer.rrotate(integer.bxor(V14, V2), 32i)
			V10 = integer.add(V10, V14)
			V6 = integer.rrotate(integer.bxor(V6, V10), 24i)
			V2 = integer.add(integer.add(V2, V6), M[s[6]])
			V14 = integer.rrotate(integer.bxor(V14, V2), 16i)
			V10 = integer.add(V10, V14)
			V6 = integer.rrotate(integer.bxor(V6, V10), 63i)

			V3 = integer.add(integer.add(V3, V7), M[s[7]])
			V15 = integer.rrotate(integer.bxor(V15, V3), 32i)
			V11 = integer.add(V11, V15)
			V7 = integer.rrotate(integer.bxor(V7, V11), 24i)
			V3 = integer.add(integer.add(V3, V7), M[s[8]])
			V15 = integer.rrotate(integer.bxor(V15, V3), 16i)
			V11 = integer.add(V11, V15)
			V7 = integer.rrotate(integer.bxor(V7, V11), 63i)

			-- diagonal step
			V0 = integer.add(integer.add(V0, V5), M[s[9]])
			V15 = integer.rrotate(integer.bxor(V15, V0), 32i)
			V10 = integer.add(V10, V15)
			V5 = integer.rrotate(integer.bxor(V5, V10), 24i)
			V0 = integer.add(integer.add(V0, V5), M[s[10]])
			V15 = integer.rrotate(integer.bxor(V15, V0), 16i)
			V10 = integer.add(V10, V15)
			V5 = integer.rrotate(integer.bxor(V5, V10), 63i)

			V1 = integer.add(integer.add(V1, V6), M[s[11]])
			V12 = integer.rrotate(integer.bxor(V12, V1), 32i)
			V11 = integer.add(V11, V12)
			V6 = integer.rrotate(integer.bxor(V6, V11), 24i)
			V1 = integer.add(integer.add(V1, V6), M[s[12]])
			V12 = integer.rrotate(integer.bxor(V12, V1), 16i)
			V11 = integer.add(V11, V12)
			V6 = integer.rrotate(integer.bxor(V6, V11), 63i)

			V2 = integer.add(integer.add(V2, V7), M[s[13]])
			V13 = integer.rrotate(integer.bxor(V13, V2), 32i)
			V8 = integer.add(V8, V13)
			V7 = integer.rrotate(integer.bxor(V7, V8), 24i)
			V2 = integer.add(integer.add(V2, V7), M[s[14]])
			V13 = integer.rrotate(integer.bxor(V13, V2), 16i)
			V8 = integer.add(V8, V13)
			V7 = integer.rrotate(integer.bxor(V7, V8), 63i)

			V3 = integer.add(integer.add(V3, V4), M[s[15]])
			V14 = integer.rrotate(integer.bxor(V14, V3), 32i)
			V9 = integer.add(V9, V14)
			V4 = integer.rrotate(integer.bxor(V4, V9), 24i)
			V3 = integer.add(integer.add(V3, V4), M[s[16]])
			V14 = integer.rrotate(integer.bxor(V14, V3), 16i)
			V9 = integer.add(V9, V14)
			V4 = integer.rrotate(integer.bxor(V4, V9), 63i)
		end

		h[1] = integer.bxor(integer.bxor(h[1], V0), V8)
		h[2] = integer.bxor(integer.bxor(h[2], V1), V9)
		h[3] = integer.bxor(integer.bxor(h[3], V2), V10)
		h[4] = integer.bxor(integer.bxor(h[4], V3), V11)
		h[5] = integer.bxor(integer.bxor(h[5], V4), V12)
		h[6] = integer.bxor(integer.bxor(h[6], V5), V13)
		h[7] = integer.bxor(integer.bxor(h[7], V6), V14)
		h[8] = integer.bxor(integer.bxor(h[8], V7), V15)
	end

	local function blake2b(buf)
		local len = buffer.len(buf)

		local h =
		{
			integer.bxor(IV1, 0x01010040i),
			IV2, IV3, IV4, IV5, IV6, IV7, IV8,
		}

		local M = table.create(16, 0i)

		-- process all full 128-byte blocks except the last
		local fullBlocks = (len - 1) // 128
		for blockIdx = 0, fullBlocks - 1 do
			local off = blockIdx * 128
			for j = 1, 16 do
				M[j] = buffer.readinteger(buf, off + (j - 1) * 8)
			end
			compress(h, M, integer.create((blockIdx + 1) * 128), false)
		end

		-- final block: copy remaining + pad with zeros to 128 bytes
		local lastBlockStart = fullBlocks * 128
		local lastBlockLen = len - lastBlockStart
		local lastBuf = buffer.create(128)
		if lastBlockLen > 0 then
			buffer.copy(lastBuf, 0, buf, lastBlockStart, lastBlockLen)
		end
		for j = 1, 16 do
			M[j] = buffer.readinteger(lastBuf, (j - 1) * 8)
		end
		compress(h, M, integer.create(len), true)

		return string.format(
			"%016x%016x%016x%016x%016x%016x%016x%016x",
			integer.bswap(h[1]), integer.bswap(h[2]), integer.bswap(h[3]), integer.bswap(h[4]),
			integer.bswap(h[5]), integer.bswap(h[6]), integer.bswap(h[7]), integer.bswap(h[8])
		)
	end

	local input = buffer.fromstring(string.rep(".", 1e3))

	local ts0 = os.clock()

	for i = 1, 100 do
		local res = blake2b(input)
		assert(res == "b9ac083b18cd6518abaed42698bbb673e9786c877a6ee89efa1c3a6508a54a20ef76f53cbcc19a18a670ea3ac668836300337c51abafca35efaac59f4396c833")
	end

	local ts1 = os.clock()

	return ts1 - ts0
end

bench.runCode(test, "blake2b_int64")
