local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

	local primes =
	{
		0x428a2f98d728ae22i, 0x7137449123ef65cdi, 0xb5c0fbcfec4d3b2fi, 0xe9b5dba58189dbbci,
		0x3956c25bf348b538i, 0x59f111f1b605d019i, 0x923f82a4af194f9bi, 0xab1c5ed5da6d8118i,
		0xd807aa98a3030242i, 0x12835b0145706fbei, 0x243185be4ee4b28ci, 0x550c7dc3d5ffb4e2i,
		0x72be5d74f27b896fi, 0x80deb1fe3b1696b1i, 0x9bdc06a725c71235i, 0xc19bf174cf692694i,
		0xe49b69c19ef14ad2i, 0xefbe4786384f25e3i, 0x0fc19dc68b8cd5b5i, 0x240ca1cc77ac9c65i,
		0x2de92c6f592b0275i, 0x4a7484aa6ea6e483i, 0x5cb0a9dcbd41fbd4i, 0x76f988da831153b5i,
		0x983e5152ee66dfabi, 0xa831c66d2db43210i, 0xb00327c898fb213fi, 0xbf597fc7beef0ee4i,
		0xc6e00bf33da88fc2i, 0xd5a79147930aa725i, 0x06ca6351e003826fi, 0x142929670a0e6e70i,
		0x27b70a8546d22ffci, 0x2e1b21385c26c926i, 0x4d2c6dfc5ac42aedi, 0x53380d139d95b3dfi,
		0x650a73548baf63dei, 0x766a0abb3c77b2a8i, 0x81c2c92e47edaee6i, 0x92722c851482353bi,
		0xa2bfe8a14cf10364i, 0xa81a664bbc423001i, 0xc24b8b70d0f89791i, 0xc76c51a30654be30i,
		0xd192e819d6ef5218i, 0xd69906245565a910i, 0xf40e35855771202ai, 0x106aa07032bbd1b8i,
		0x19a4c116b8d2d0c8i, 0x1e376c085141ab53i, 0x2748774cdf8eeb99i, 0x34b0bcb5e19b48a8i,
		0x391c0cb3c5c95a63i, 0x4ed8aa4ae3418acbi, 0x5b9cca4f7763e373i, 0x682e6ff3d6b2b8a3i,
		0x748f82ee5defb2fci, 0x78a5636f43172f60i, 0x84c87814a1f0ab72i, 0x8cc702081a6439eci,
		0x90befffa23631e28i, 0xa4506cebde82bde9i, 0xbef9a3f7b2c67915i, 0xc67178f2e372532bi,
		0xca273eceea26619ci, 0xd186b8c721c0c207i, 0xeada7dd6cde0eb1ei, 0xf57d4f7fee6ed178i,
		0x06f067aa72176fbai, 0x0a637dc5a2c898a6i, 0x113f9804bef90daei, 0x1b710b35131c471bi,
		0x28db77f523047d84i, 0x32caab7b40c72493i, 0x3c9ebe0a15c9bebci, 0x431d67c49c100d4ci,
		0x4cc5d4becb3e42b6i, 0x597f299cfc657e2ai, 0x5fcb6fab3ad6faeci, 0x6c44198c4a475817i,
	}

	local function toHex(buf)
		return string.format(
			"%016x%016x%016x%016x%016x%016x%016x%016x",
			buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7], buf[8]
		)
	end

	local function preprocess(msg)
		local msgLen = #msg
		local extra = 128 - ((msgLen + 17) % 128)

		local padded = msg .. '\128' .. string.rep('\0', extra + 8)
		local paddedLen = #padded + 8

		local buf = buffer.create(paddedLen)
		buffer.writestring(buf, 0, padded)

		-- length goes in the low 8 bytes (high 8 bytes left as zero, fits since msgLen*8 < 2^64)
		local bitLen = msgLen * 8
		for i = 0, 7 do
			local rem = bitLen % 256
			buffer.writeu8(buf, paddedLen - 1 - i, rem)
			bitLen = (bitLen - rem) / 256
		end

		return buf, paddedLen
	end

	local function digestBlock(buf, i, hash, digest)
		for j = 1, 16 do
			local offset = i + (j - 1) * 8
			digest[j] = integer.bswap(buffer.readinteger(buf, offset))
		end

		for j = 17, 80 do
			local v = digest[j - 15]
			local s0 = integer.bxor(integer.rrotate(v, 1i), integer.rrotate(v, 8i), integer.rshift(v, 7i))

			v = digest[j - 2]
			local s1 = integer.bxor(integer.rrotate(v, 19i), integer.rrotate(v, 61i), integer.rshift(v, 6i))

			digest[j] = integer.add(integer.add(digest[j - 16], s0), integer.add(digest[j - 7], s1))
		end

		local a, b, c, d, e, f, g, h = table.unpack(hash)

		for r = 1, 80 do
			local s0 = integer.bxor(integer.rrotate(a, 28i), integer.rrotate(a, 34i), integer.rrotate(a, 39i))
			local maj = integer.bxor(integer.band(a, b), integer.band(a, c), integer.band(b, c))
			local t2 = integer.add(s0, maj)

			local s1 = integer.bxor(integer.rrotate(e, 14i), integer.rrotate(e, 18i), integer.rrotate(e, 41i))
			local ch = integer.bxor(integer.band(e, f), integer.band(integer.bnot(e), g))
			local t1 = integer.add(integer.add(integer.add(h, s1), ch), integer.add(primes[r], digest[r]))

			h, g, f, e, d, c, b, a = g, f, e, integer.add(d, t1), c, b, a, integer.add(t1, t2)
		end

		hash[1] = integer.add(hash[1], a)
		hash[2] = integer.add(hash[2], b)
		hash[3] = integer.add(hash[3], c)
		hash[4] = integer.add(hash[4], d)
		hash[5] = integer.add(hash[5], e)
		hash[6] = integer.add(hash[6], f)
		hash[7] = integer.add(hash[7], g)
		hash[8] = integer.add(hash[8], h)
	end

	local function sha512(msg)
		local buf, paddedLen = preprocess(msg)

		local hash =
		{
			0x6a09e667f3bcc908i,
			0xbb67ae8584caa73bi,
			0x3c6ef372fe94f82bi,
			0xa54ff53a5f1d36f1i,
			0x510e527fade682d1i,
			0x9b05688c2b3e6c1fi,
			0x1f83d9abfb41bd6bi,
			0x5be0cd19137e2179i,
		}

		local digest = {}

		for i = 0, paddedLen - 1, 128 do
			digestBlock(buf, i, hash, digest)
		end

		return toHex(hash)
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

bench.runCode(test, "sha512_int64")
