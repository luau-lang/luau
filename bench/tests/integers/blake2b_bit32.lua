local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

	-- 64-bit wrapping add: (ah:al) + (bh:bl) -> (rh, rl)
	local function add64(ah, al, bh, bl)
		local lo = al + bl
		local hi = ah + bh + lo // 0x100000000
		return bit32.bor(hi, 0), bit32.bor(lo, 0)
	end

	-- IV constants (hi, lo)
	local IV1h, IV1l = 0x6a09e667, 0xf3bcc908
	local IV2h, IV2l = 0xbb67ae85, 0x84caa73b
	local IV3h, IV3l = 0x3c6ef372, 0xfe94f82b
	local IV4h, IV4l = 0xa54ff53a, 0x5f1d36f1
	local IV5h, IV5l = 0x510e527f, 0xade682d1
	local IV6h, IV6l = 0x9b05688c, 0x2b3e6c1f
	local IV7h, IV7l = 0x1f83d9ab, 0xfb41bd6b
	local IV8h, IV8l = 0x5be0cd19, 0x137e2179

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

	local function compress(hh, hl, Mh, Ml, th, tl, isLast)
		local V0h, V0l = hh[1], hl[1]
		local V1h, V1l = hh[2], hl[2]
		local V2h, V2l = hh[3], hl[3]
		local V3h, V3l = hh[4], hl[4]
		local V4h, V4l = hh[5], hl[5]
		local V5h, V5l = hh[6], hl[6]
		local V6h, V6l = hh[7], hl[7]
		local V7h, V7l = hh[8], hl[8]
		local V8h, V8l = IV1h, IV1l
		local V9h, V9l = IV2h, IV2l
		local V10h, V10l = IV3h, IV3l
		local V11h, V11l = IV4h, IV4l
		local V12h, V12l = bit32.bxor(IV5h, th), bit32.bxor(IV5l, tl)
		local V13h, V13l = IV6h, IV6l
		local V14h, V14l, V15h, V15l
		if isLast then
			V14h, V14l = bit32.bxor(IV7h, 0xffffffff), bit32.bxor(IV7l, 0xffffffff)
		else
			V14h, V14l = IV7h, IV7l
		end
		V15h, V15l = IV8h, IV8l

		for r = 1, 12 do
			local s = SIGMA[r]

			-- Rotation helpers inlined for the 4 specific amounts:
			-- rrotate by 32 = swap hi/lo
			-- rrotate by 24 = bit32.rshift(h,24)|bit32.lshift(l,8), bit32.rshift(l,24)|bit32.lshift(h,8)
			-- rrotate by 16 = bit32.rshift(h,16)|bit32.lshift(l,16), bit32.rshift(l,16)|bit32.lshift(h,16)
			-- rrotate by 63 = lrotate by 1 = bit32.lshift(h,1)|bit32.rshift(l,31), bit32.lshift(l,1)|bit32.rshift(h,31)

			-- G(V0,V4,V8,V12) with m[s[1]], m[s[2]]
			V0h, V0l = add64(V0h, V0l, V4h, V4l); V0h, V0l = add64(V0h, V0l, Mh[s[1]], Ml[s[1]])
			V12h, V12l = bit32.bxor(V12l, V0l), bit32.bxor(V12h, V0h) -- rrotate 32
			V8h, V8l = add64(V8h, V8l, V12h, V12l)
			local xh, xl = bit32.bxor(V4h, V8h), bit32.bxor(V4l, V8l); V4h = bit32.bor(bit32.rshift(xh, 24), bit32.lshift(xl, 8)); V4l = bit32.bor(bit32.rshift(xl, 24), bit32.lshift(xh, 8)) -- rrotate 24
			V0h, V0l = add64(V0h, V0l, V4h, V4l); V0h, V0l = add64(V0h, V0l, Mh[s[2]], Ml[s[2]])
			xh, xl = bit32.bxor(V12h, V0h), bit32.bxor(V12l, V0l); V12h = bit32.bor(bit32.rshift(xh, 16), bit32.lshift(xl, 16)); V12l = bit32.bor(bit32.rshift(xl, 16), bit32.lshift(xh, 16)) -- rrotate 16
			V8h, V8l = add64(V8h, V8l, V12h, V12l)
			xh, xl = bit32.bxor(V4h, V8h), bit32.bxor(V4l, V8l); V4h = bit32.bor(bit32.lshift(xh, 1), bit32.rshift(xl, 31)); V4l = bit32.bor(bit32.lshift(xl, 1), bit32.rshift(xh, 31)) -- rrotate 63 = lrotate 1

			-- G(V1,V5,V9,V13) with m[s[3]], m[s[4]]
			V1h, V1l = add64(V1h, V1l, V5h, V5l); V1h, V1l = add64(V1h, V1l, Mh[s[3]], Ml[s[3]])
			V13h, V13l = bit32.bxor(V13l, V1l), bit32.bxor(V13h, V1h)
			V9h, V9l = add64(V9h, V9l, V13h, V13l)
			xh, xl = bit32.bxor(V5h, V9h), bit32.bxor(V5l, V9l); V5h = bit32.bor(bit32.rshift(xh, 24), bit32.lshift(xl, 8)); V5l = bit32.bor(bit32.rshift(xl, 24), bit32.lshift(xh, 8))
			V1h, V1l = add64(V1h, V1l, V5h, V5l); V1h, V1l = add64(V1h, V1l, Mh[s[4]], Ml[s[4]])
			xh, xl = bit32.bxor(V13h, V1h), bit32.bxor(V13l, V1l); V13h = bit32.bor(bit32.rshift(xh, 16), bit32.lshift(xl, 16)); V13l = bit32.bor(bit32.rshift(xl, 16), bit32.lshift(xh, 16))
			V9h, V9l = add64(V9h, V9l, V13h, V13l)
			xh, xl = bit32.bxor(V5h, V9h), bit32.bxor(V5l, V9l); V5h = bit32.bor(bit32.lshift(xh, 1), bit32.rshift(xl, 31)); V5l = bit32.bor(bit32.lshift(xl, 1), bit32.rshift(xh, 31))

			-- G(V2,V6,V10,V14) with m[s[5]], m[s[6]]
			V2h, V2l = add64(V2h, V2l, V6h, V6l); V2h, V2l = add64(V2h, V2l, Mh[s[5]], Ml[s[5]])
			V14h, V14l = bit32.bxor(V14l, V2l), bit32.bxor(V14h, V2h)
			V10h, V10l = add64(V10h, V10l, V14h, V14l)
			xh, xl = bit32.bxor(V6h, V10h), bit32.bxor(V6l, V10l); V6h = bit32.bor(bit32.rshift(xh, 24), bit32.lshift(xl, 8)); V6l = bit32.bor(bit32.rshift(xl, 24), bit32.lshift(xh, 8))
			V2h, V2l = add64(V2h, V2l, V6h, V6l); V2h, V2l = add64(V2h, V2l, Mh[s[6]], Ml[s[6]])
			xh, xl = bit32.bxor(V14h, V2h), bit32.bxor(V14l, V2l); V14h = bit32.bor(bit32.rshift(xh, 16), bit32.lshift(xl, 16)); V14l = bit32.bor(bit32.rshift(xl, 16), bit32.lshift(xh, 16))
			V10h, V10l = add64(V10h, V10l, V14h, V14l)
			xh, xl = bit32.bxor(V6h, V10h), bit32.bxor(V6l, V10l); V6h = bit32.bor(bit32.lshift(xh, 1), bit32.rshift(xl, 31)); V6l = bit32.bor(bit32.lshift(xl, 1), bit32.rshift(xh, 31))

			-- G(V3,V7,V11,V15) with m[s[7]], m[s[8]]
			V3h, V3l = add64(V3h, V3l, V7h, V7l); V3h, V3l = add64(V3h, V3l, Mh[s[7]], Ml[s[7]])
			V15h, V15l = bit32.bxor(V15l, V3l), bit32.bxor(V15h, V3h)
			V11h, V11l = add64(V11h, V11l, V15h, V15l)
			xh, xl = bit32.bxor(V7h, V11h), bit32.bxor(V7l, V11l); V7h = bit32.bor(bit32.rshift(xh, 24), bit32.lshift(xl, 8)); V7l = bit32.bor(bit32.rshift(xl, 24), bit32.lshift(xh, 8))
			V3h, V3l = add64(V3h, V3l, V7h, V7l); V3h, V3l = add64(V3h, V3l, Mh[s[8]], Ml[s[8]])
			xh, xl = bit32.bxor(V15h, V3h), bit32.bxor(V15l, V3l); V15h = bit32.bor(bit32.rshift(xh, 16), bit32.lshift(xl, 16)); V15l = bit32.bor(bit32.rshift(xl, 16), bit32.lshift(xh, 16))
			V11h, V11l = add64(V11h, V11l, V15h, V15l)
			xh, xl = bit32.bxor(V7h, V11h), bit32.bxor(V7l, V11l); V7h = bit32.bor(bit32.lshift(xh, 1), bit32.rshift(xl, 31)); V7l = bit32.bor(bit32.lshift(xl, 1), bit32.rshift(xh, 31))

			-- diagonal step: G(V0,V5,V10,V15) with m[s[9]], m[s[10]]
			V0h, V0l = add64(V0h, V0l, V5h, V5l); V0h, V0l = add64(V0h, V0l, Mh[s[9]], Ml[s[9]])
			V15h, V15l = bit32.bxor(V15l, V0l), bit32.bxor(V15h, V0h)
			V10h, V10l = add64(V10h, V10l, V15h, V15l)
			xh, xl = bit32.bxor(V5h, V10h), bit32.bxor(V5l, V10l); V5h = bit32.bor(bit32.rshift(xh, 24), bit32.lshift(xl, 8)); V5l = bit32.bor(bit32.rshift(xl, 24), bit32.lshift(xh, 8))
			V0h, V0l = add64(V0h, V0l, V5h, V5l); V0h, V0l = add64(V0h, V0l, Mh[s[10]], Ml[s[10]])
			xh, xl = bit32.bxor(V15h, V0h), bit32.bxor(V15l, V0l); V15h = bit32.bor(bit32.rshift(xh, 16), bit32.lshift(xl, 16)); V15l = bit32.bor(bit32.rshift(xl, 16), bit32.lshift(xh, 16))
			V10h, V10l = add64(V10h, V10l, V15h, V15l)
			xh, xl = bit32.bxor(V5h, V10h), bit32.bxor(V5l, V10l); V5h = bit32.bor(bit32.lshift(xh, 1), bit32.rshift(xl, 31)); V5l = bit32.bor(bit32.lshift(xl, 1), bit32.rshift(xh, 31))

			-- G(V1,V6,V11,V12) with m[s[11]], m[s[12]]
			V1h, V1l = add64(V1h, V1l, V6h, V6l); V1h, V1l = add64(V1h, V1l, Mh[s[11]], Ml[s[11]])
			V12h, V12l = bit32.bxor(V12l, V1l), bit32.bxor(V12h, V1h)
			V11h, V11l = add64(V11h, V11l, V12h, V12l)
			xh, xl = bit32.bxor(V6h, V11h), bit32.bxor(V6l, V11l); V6h = bit32.bor(bit32.rshift(xh, 24), bit32.lshift(xl, 8)); V6l = bit32.bor(bit32.rshift(xl, 24), bit32.lshift(xh, 8))
			V1h, V1l = add64(V1h, V1l, V6h, V6l); V1h, V1l = add64(V1h, V1l, Mh[s[12]], Ml[s[12]])
			xh, xl = bit32.bxor(V12h, V1h), bit32.bxor(V12l, V1l); V12h = bit32.bor(bit32.rshift(xh, 16), bit32.lshift(xl, 16)); V12l = bit32.bor(bit32.rshift(xl, 16), bit32.lshift(xh, 16))
			V11h, V11l = add64(V11h, V11l, V12h, V12l)
			xh, xl = bit32.bxor(V6h, V11h), bit32.bxor(V6l, V11l); V6h = bit32.bor(bit32.lshift(xh, 1), bit32.rshift(xl, 31)); V6l = bit32.bor(bit32.lshift(xl, 1), bit32.rshift(xh, 31))

			-- G(V2,V7,V8,V13) with m[s[13]], m[s[14]]
			V2h, V2l = add64(V2h, V2l, V7h, V7l); V2h, V2l = add64(V2h, V2l, Mh[s[13]], Ml[s[13]])
			V13h, V13l = bit32.bxor(V13l, V2l), bit32.bxor(V13h, V2h)
			V8h, V8l = add64(V8h, V8l, V13h, V13l)
			xh, xl = bit32.bxor(V7h, V8h), bit32.bxor(V7l, V8l); V7h = bit32.bor(bit32.rshift(xh, 24), bit32.lshift(xl, 8)); V7l = bit32.bor(bit32.rshift(xl, 24), bit32.lshift(xh, 8))
			V2h, V2l = add64(V2h, V2l, V7h, V7l); V2h, V2l = add64(V2h, V2l, Mh[s[14]], Ml[s[14]])
			xh, xl = bit32.bxor(V13h, V2h), bit32.bxor(V13l, V2l); V13h = bit32.bor(bit32.rshift(xh, 16), bit32.lshift(xl, 16)); V13l = bit32.bor(bit32.rshift(xl, 16), bit32.lshift(xh, 16))
			V8h, V8l = add64(V8h, V8l, V13h, V13l)
			xh, xl = bit32.bxor(V7h, V8h), bit32.bxor(V7l, V8l); V7h = bit32.bor(bit32.lshift(xh, 1), bit32.rshift(xl, 31)); V7l = bit32.bor(bit32.lshift(xl, 1), bit32.rshift(xh, 31))

			-- G(V3,V4,V9,V14) with m[s[15]], m[s[16]]
			V3h, V3l = add64(V3h, V3l, V4h, V4l); V3h, V3l = add64(V3h, V3l, Mh[s[15]], Ml[s[15]])
			V14h, V14l = bit32.bxor(V14l, V3l), bit32.bxor(V14h, V3h)
			V9h, V9l = add64(V9h, V9l, V14h, V14l)
			xh, xl = bit32.bxor(V4h, V9h), bit32.bxor(V4l, V9l); V4h = bit32.bor(bit32.rshift(xh, 24), bit32.lshift(xl, 8)); V4l = bit32.bor(bit32.rshift(xl, 24), bit32.lshift(xh, 8))
			V3h, V3l = add64(V3h, V3l, V4h, V4l); V3h, V3l = add64(V3h, V3l, Mh[s[16]], Ml[s[16]])
			xh, xl = bit32.bxor(V14h, V3h), bit32.bxor(V14l, V3l); V14h = bit32.bor(bit32.rshift(xh, 16), bit32.lshift(xl, 16)); V14l = bit32.bor(bit32.rshift(xl, 16), bit32.lshift(xh, 16))
			V9h, V9l = add64(V9h, V9l, V14h, V14l)
			xh, xl = bit32.bxor(V4h, V9h), bit32.bxor(V4l, V9l); V4h = bit32.bor(bit32.lshift(xh, 1), bit32.rshift(xl, 31)); V4l = bit32.bor(bit32.lshift(xl, 1), bit32.rshift(xh, 31))
		end

		hh[1] = bit32.bxor(bit32.bxor(hh[1], V0h), V8h);  hl[1] = bit32.bxor(bit32.bxor(hl[1], V0l), V8l)
		hh[2] = bit32.bxor(bit32.bxor(hh[2], V1h), V9h);  hl[2] = bit32.bxor(bit32.bxor(hl[2], V1l), V9l)
		hh[3] = bit32.bxor(bit32.bxor(hh[3], V2h), V10h); hl[3] = bit32.bxor(bit32.bxor(hl[3], V2l), V10l)
		hh[4] = bit32.bxor(bit32.bxor(hh[4], V3h), V11h); hl[4] = bit32.bxor(bit32.bxor(hl[4], V3l), V11l)
		hh[5] = bit32.bxor(bit32.bxor(hh[5], V4h), V12h); hl[5] = bit32.bxor(bit32.bxor(hl[5], V4l), V12l)
		hh[6] = bit32.bxor(bit32.bxor(hh[6], V5h), V13h); hl[6] = bit32.bxor(bit32.bxor(hl[6], V5l), V13l)
		hh[7] = bit32.bxor(bit32.bxor(hh[7], V6h), V14h); hl[7] = bit32.bxor(bit32.bxor(hl[7], V6l), V14l)
		hh[8] = bit32.bxor(bit32.bxor(hh[8], V7h), V15h); hl[8] = bit32.bxor(bit32.bxor(hl[8], V7l), V15l)
	end

	local function blake2b(buf)
		local len = buffer.len(buf)

		-- h[1] = IV1 XOR 0x01010040 (digest=64, key=0, fanout=1, depth=1)
		local hh = { IV1h, IV2h, IV3h, IV4h, IV5h, IV6h, IV7h, IV8h }
		local hl = { bit32.bxor(IV1l, 0x01010040), IV2l, IV3l, IV4l, IV5l, IV6l, IV7l, IV8l }

		local Mh = table.create(16, 0)
		local Ml = table.create(16, 0)

		local fullBlocks = (len - 1) // 128
		for blockIdx = 0, fullBlocks - 1 do
			local off = blockIdx * 128
			for j = 1, 16 do
				local bo = off + (j - 1) * 8
				Mh[j] = buffer.readu32(buf, bo + 4)
				Ml[j] = buffer.readu32(buf, bo)
			end
			local byteCount = (blockIdx + 1) * 128
			compress(hh, hl, Mh, Ml, 0, byteCount, false)
		end

		local lastBlockStart = fullBlocks * 128
		local lastBlockLen = len - lastBlockStart
		local lastBuf = buffer.create(128)
		if lastBlockLen > 0 then
			buffer.copy(lastBuf, 0, buf, lastBlockStart, lastBlockLen)
		end
		for j = 1, 16 do
			local bo = (j - 1) * 8
			Mh[j] = buffer.readu32(lastBuf, bo + 4)
			Ml[j] = buffer.readu32(lastBuf, bo)
		end
		compress(hh, hl, Mh, Ml, 0, len, true)

		return string.format(
			"%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x",
			bit32.byteswap(hl[1]), bit32.byteswap(hh[1]), bit32.byteswap(hl[2]), bit32.byteswap(hh[2]),
			bit32.byteswap(hl[3]), bit32.byteswap(hh[3]), bit32.byteswap(hl[4]), bit32.byteswap(hh[4]),
			bit32.byteswap(hl[5]), bit32.byteswap(hh[5]), bit32.byteswap(hl[6]), bit32.byteswap(hh[6]),
			bit32.byteswap(hl[7]), bit32.byteswap(hh[7]), bit32.byteswap(hl[8]), bit32.byteswap(hh[8]))
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

bench.runCode(test, "blake2b_bit32")
