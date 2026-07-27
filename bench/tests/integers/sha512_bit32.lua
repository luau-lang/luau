local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

	local K_HI = {
		0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
		0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
		0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
		0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
		0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
		0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
		0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
		0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
		0xca273ece, 0xd186b8c7, 0xeada7dd6, 0xf57d4f7f, 0x06f067aa, 0x0a637dc5, 0x113f9804, 0x1b710b35,
		0x28db77f5, 0x32caab7b, 0x3c9ebe0a, 0x431d67c4, 0x4cc5d4be, 0x597f299c, 0x5fcb6fab, 0x6c44198c,
	}

	local K_LO = {
		0xd728ae22, 0x23ef65cd, 0xec4d3b2f, 0x8189dbbc, 0xf348b538, 0xb605d019, 0xaf194f9b, 0xda6d8118,
		0xa3030242, 0x45706fbe, 0x4ee4b28c, 0xd5ffb4e2, 0xf27b896f, 0x3b1696b1, 0x25c71235, 0xcf692694,
		0x9ef14ad2, 0x384f25e3, 0x8b8cd5b5, 0x77ac9c65, 0x592b0275, 0x6ea6e483, 0xbd41fbd4, 0x831153b5,
		0xee66dfab, 0x2db43210, 0x98fb213f, 0xbeef0ee4, 0x3da88fc2, 0x930aa725, 0xe003826f, 0x0a0e6e70,
		0x46d22ffc, 0x5c26c926, 0x5ac42aed, 0x9d95b3df, 0x8baf63de, 0x3c77b2a8, 0x47edaee6, 0x1482353b,
		0x4cf10364, 0xbc423001, 0xd0f89791, 0x0654be30, 0xd6ef5218, 0x5565a910, 0x5771202a, 0x32bbd1b8,
		0xb8d2d0c8, 0x5141ab53, 0xdf8eeb99, 0xe19b48a8, 0xc5c95a63, 0xe3418acb, 0x7763e373, 0xd6b2b8a3,
		0x5defb2fc, 0x43172f60, 0xa1f0ab72, 0x1a6439ec, 0x23631e28, 0xde82bde9, 0xb2c67915, 0xe372532b,
		0xea26619c, 0x21c0c207, 0xcde0eb1e, 0xee6ed178, 0x72176fba, 0xa2c898a6, 0xbef90dae, 0x131c471b,
		0x23047d84, 0x40c72493, 0x15c9bebc, 0x9c100d4c, 0xcb3e42b6, 0xfc657e2a, 0x3ad6faec, 0x4a475817,
	}

	local function preprocess(msg)
		local msgLen = #msg
		local extra = 128 - ((msgLen + 17) % 128)

		local padded = msg .. '\128' .. string.rep('\0', extra + 8)
		local paddedLen = #padded + 8

		local buf = buffer.create(paddedLen)
		buffer.writestring(buf, 0, padded)

		local bitLen = msgLen * 8
		for i = 0, 7 do
			local rem = bitLen % 256
			buffer.writeu8(buf, paddedLen - 1 - i, rem)
			bitLen = (bitLen - rem) / 256
		end

		return buf, paddedLen
	end

	local function sha512(msg)
		local buf, paddedLen = preprocess(msg)

		local WH, WL = table.create(80, 0), table.create(80, 0)

		local H1h, H2h, H3h, H4h = 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a
		local H5h, H6h, H7h, H8h = 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
		local H1l, H2l, H3l, H4l = 0xf3bcc908, 0x84caa73b, 0xfe94f82b, 0x5f1d36f1
		local H5l, H6l, H7l, H8l = 0xade682d1, 0x2b3e6c1f, 0xfb41bd6b, 0x137e2179

		for offset = 0, paddedLen - 1, 128 do
			for t = 1, 16 do
				local bo = offset + (t - 1) * 8
				WH[t] = bit32.byteswap(buffer.readu32(buf, bo))
				WL[t] = bit32.byteswap(buffer.readu32(buf, bo + 4))
			end

			for t = 17, 80 do
				local p15h, p15l = WH[t - 15], WL[t - 15]
				local p2h, p2l = WH[t - 2], WL[t - 2]

				-- s0 = rrotate(w[t-15], 1) XOR rrotate(w[t-15], 8) XOR bit32.rshift(w[t-15], 7)
				-- Using + instead of bor because shifted halves never have overlapping bits
				local s0l = bit32.bxor(
					bit32.rshift(p15l, 1) + bit32.lshift(p15h, 31),
					bit32.rshift(p15l, 8) + bit32.lshift(p15h, 24),
					bit32.rshift(p15l, 7) + bit32.lshift(p15h, 25))
				local s0h = bit32.bxor(
					bit32.rshift(p15h, 1) + bit32.lshift(p15l, 31),
					bit32.rshift(p15h, 8) + bit32.lshift(p15l, 24),
					bit32.rshift(p15h, 7))

				-- s1 = rrotate(w[t-2], 19) XOR rrotate(w[t-2], 61) XOR bit32.rshift(w[t-2], 6)
				local s1l = bit32.bxor(
					bit32.rshift(p2l, 19) + bit32.lshift(p2h, 13),
					bit32.lshift(p2l, 3) + bit32.rshift(p2h, 29),
					bit32.rshift(p2l, 6) + bit32.lshift(p2h, 26))
				local s1h = bit32.bxor(
					bit32.rshift(p2h, 19) + bit32.lshift(p2l, 13),
					bit32.lshift(p2h, 3) + bit32.rshift(p2l, 29),
					bit32.rshift(p2h, 6))

				-- w[t] = w[t-16] + s0 + w[t-7] + s1  (64-bit wrapping add via carry)
				local tmplo = WL[t - 16] + s0l + WL[t - 7] + s1l
				WL[t] = bit32.bor(tmplo, 0)
				WH[t] = s0h + s1h + WH[t - 16] + WH[t - 7] + tmplo // 0x100000000
			end

			local ah, al = H1h, H1l
			local bh, bl = H2h, H2l
			local ch, cl = H3h, H3l
			local dh, dl = H4h, H4l
			local eh, el = H5h, H5l
			local fh, fl = H6h, H6l
			local gh, gl = H7h, H7l
			local hh, hl = H8h, H8l

			for t = 1, 80 do
				-- Sigma1 = rrotate(e, 14) XOR rrotate(e, 18) XOR rrotate(e, 41)
				local sig1l = bit32.bxor(
					bit32.rshift(el, 14) + bit32.lshift(eh, 18),
					bit32.rshift(el, 18) + bit32.lshift(eh, 14),
					bit32.lshift(el, 23) + bit32.rshift(eh, 9))
				local sig1h = bit32.bxor(
					bit32.rshift(eh, 14) + bit32.lshift(el, 18),
					bit32.rshift(eh, 18) + bit32.lshift(el, 14),
					bit32.lshift(eh, 23) + bit32.rshift(el, 9))

				-- Sigma0 = rrotate(a, 28) XOR rrotate(a, 34) XOR rrotate(a, 39)
				local sig0l = bit32.bxor(
					bit32.rshift(al, 28) + bit32.lshift(ah, 4),
					bit32.lshift(al, 30) + bit32.rshift(ah, 2),
					bit32.lshift(al, 25) + bit32.rshift(ah, 7))
				local sig0h = bit32.bxor(
					bit32.rshift(ah, 28) + bit32.lshift(al, 4),
					bit32.lshift(ah, 30) + bit32.rshift(al, 2),
					bit32.lshift(ah, 25) + bit32.rshift(al, 7))

				-- Ch = (e AND f) XOR (NOT(e) AND g)
				-- Using + because band results are complementary (no overlapping bits)
				local chl = bit32.band(el, fl) + bit32.band(-1 - el, gl)
				local chh = bit32.band(eh, fh) + bit32.band(-1 - eh, gh)

				-- Maj = (a AND b) XOR (a AND c) XOR (b AND c)
				-- Rewritten as: (b AND c) + (a AND (b XOR c))
				local majl = bit32.band(cl, bl) + bit32.band(al, bit32.bxor(cl, bl))
				local majh = bit32.band(ch, bh) + bit32.band(ah, bit32.bxor(ch, bh))

				-- T1 = h + Sigma1 + Ch + K[t] + W[t]
				local t1l = hl + sig1l + chl + K_LO[t] + WL[t]
				local t1h = hh + sig1h + chh + K_HI[t] + WH[t] + t1l // 0x100000000
				t1l = bit32.bor(t1l, 0)

				-- Shift state and compute new e and a
				hh, hl = gh, gl
				gh, gl = fh, fl
				fh, fl = eh, el

				local enl = dl + t1l
				eh = dh + t1h + enl // 0x100000000
				el = bit32.bor(enl, 0)

				dh, dl = ch, cl
				ch, cl = bh, bl
				bh, bl = ah, al

				local anl = t1l + sig0l + majl
				ah = t1h + sig0h + majh + anl // 0x100000000
				al = bit32.bor(anl, 0)
			end

			H1l = H1l + al; H1h = bit32.bor(H1h + ah + H1l // 0x100000000, 0); H1l = bit32.bor(H1l, 0)
			H2l = H2l + bl; H2h = bit32.bor(H2h + bh + H2l // 0x100000000, 0); H2l = bit32.bor(H2l, 0)
			H3l = H3l + cl; H3h = bit32.bor(H3h + ch + H3l // 0x100000000, 0); H3l = bit32.bor(H3l, 0)
			H4l = H4l + dl; H4h = bit32.bor(H4h + dh + H4l // 0x100000000, 0); H4l = bit32.bor(H4l, 0)
			H5l = H5l + el; H5h = bit32.bor(H5h + eh + H5l // 0x100000000, 0); H5l = bit32.bor(H5l, 0)
			H6l = H6l + fl; H6h = bit32.bor(H6h + fh + H6l // 0x100000000, 0); H6l = bit32.bor(H6l, 0)
			H7l = H7l + gl; H7h = bit32.bor(H7h + gh + H7l // 0x100000000, 0); H7l = bit32.bor(H7l, 0)
			H8l = H8l + hl; H8h = bit32.bor(H8h + hh + H8l // 0x100000000, 0); H8l = bit32.bor(H8l, 0)
		end

		return string.format(
			"%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x%08x",
			H1h, H1l, H2h, H2l, H3h, H3l, H4h, H4l,
			H5h, H5l, H6h, H6l, H7h, H7l, H8h, H8l)
	end

	local input = string.rep(".", 1e3)

	local ts0 = os.clock()

	for i = 1, 100 do
		local res = sha512(input)
		assert(res == "a17d627e7c3f79207e8ca630348c2e15b70206f88905167dbbc18fd8d2b2806f2ad757c781dfbdc6a0caf1c84a8615bfdbda58f0356543bd00e646a45ca83790")
	end

	local ts1 = os.clock()

	return ts1 - ts0
end

bench.runCode(test, "sha512_bit32")
