local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

	local C1 = 0x87c37b91114253d5i
	local C2 = 0x4cf5ad432745937fi
	local M5 = 5i
	local K1 = 0x52dce729i
	local K2 = 0x38495ab5i
	local FMIX1 = 0xff51afd7ed558ccdi
	local FMIX2 = 0xc4ceb9fe1a85ec53i

	local function fmix64(k)
		k = integer.mul(integer.bxor(k, integer.rshift(k, 33i)), FMIX1)
		k = integer.mul(integer.bxor(k, integer.rshift(k, 33i)), FMIX2)
		return integer.bxor(k, integer.rshift(k, 33i))
	end

	local function murmur3_x64_128(buf, seed)
		local len = buffer.len(buf)
		local nblocks = len // 16

		local h1 = seed
		local h2 = seed

		for i = 0, nblocks - 1 do
			local off = i * 16
			local k1 = buffer.readinteger(buf, off)
			local k2 = buffer.readinteger(buf, off + 8)

			k1 = integer.mul(integer.lrotate(integer.mul(k1, C1), 31i), C2)
			h1 = integer.bxor(h1, k1)
			h1 = integer.add(integer.mul(integer.add(integer.lrotate(h1, 27i), h2), M5), K1)

			k2 = integer.mul(integer.lrotate(integer.mul(k2, C2), 33i), C1)
			h2 = integer.bxor(h2, k2)
			h2 = integer.add(integer.mul(integer.add(integer.lrotate(h2, 31i), h1), M5), K2)
		end

		-- tail (0..15 bytes)
		local tailStart = nblocks * 16
		local rem = len - tailStart
		if rem > 0 then
			local k1 = 0i
			local k2 = 0i

			if rem >= 9 then
				k1 = buffer.readinteger(buf, tailStart)
				for j = 8, rem - 1 do
					k2 = integer.bxor(k2, integer.lshift(integer.create(buffer.readu8(buf, tailStart + j)), integer.create((j - 8) * 8)))
				end
				k2 = integer.mul(integer.lrotate(integer.mul(k2, C2), 33i), C1)
				h2 = integer.bxor(h2, k2)
			elseif rem == 8 then
				k1 = buffer.readinteger(buf, tailStart)
			else
				for j = 0, rem - 1 do
					k1 = integer.bxor(k1, integer.lshift(integer.create(buffer.readu8(buf, tailStart + j)), integer.create(j * 8)))
				end
			end

			k1 = integer.mul(integer.lrotate(integer.mul(k1, C1), 31i), C2)
			h1 = integer.bxor(h1, k1)
		end

		h1 = integer.bxor(h1, integer.create(len))
		h2 = integer.bxor(h2, integer.create(len))

		h1 = integer.add(h1, h2)
		h2 = integer.add(h2, h1)

		h1 = fmix64(h1)
		h2 = fmix64(h2)

		h1 = integer.add(h1, h2)
		h2 = integer.add(h2, h1)

		-- byte-stream hex (little-endian per 64-bit lane)
		return string.format("%016x%016x", integer.bswap(h1), integer.bswap(h2))
	end

	local input = buffer.fromstring(string.rep(".", 1e3))

	local ts0 = os.clock()

	for i = 1, 1500 do
		local res = murmur3_x64_128(input, 0i)
		assert(res == "d092966966d88531ef6a373f8fbb714f")
	end

	local ts1 = os.clock()

	return ts1 - ts0
end

bench.runCode(test, "murmur128_int64")
