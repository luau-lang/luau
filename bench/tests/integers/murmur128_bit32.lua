local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

	-- 64-bit wrapping add: (ah:al) + (bh:bl) -> (rh, rl)
	local function add64(ah, al, bh, bl)
		local lo = al + bl
		local hi = ah + bh + lo // 0x100000000
		return bit32.bor(hi, 0), bit32.bor(lo, 0)
	end

	-- 64-bit wrapping multiply via 16-bit limb schoolbook
	local function mul64(ah, al, bh, bl)
		local a0 = bit32.band(al, 0xFFFF)
		local a1 = bit32.rshift(al, 16)
		local a2 = bit32.band(ah, 0xFFFF)
		local a3 = bit32.rshift(ah, 16)
		local b0 = bit32.band(bl, 0xFFFF)
		local b1 = bit32.rshift(bl, 16)
		local b2 = bit32.band(bh, 0xFFFF)
		local b3 = bit32.rshift(bh, 16)

		local c0 = a0 * b0
		local c1 = a1 * b0 + a0 * b1
		local c2 = a2 * b0 + a1 * b1 + a0 * b2
		local c3 = a3 * b0 + a2 * b1 + a1 * b2 + a0 * b3

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

	-- Constants (hi, lo)
	local C1h, C1l = 0x87c37b91, 0x114253d5
	local C2h, C2l = 0x4cf5ad43, 0x2745937f
	local FMIX1h, FMIX1l = 0xff51afd7, 0xed558ccd
	local FMIX2h, FMIX2l = 0xc4ceb9fe, 0x1a85ec53

	local function fmix64(kh, kl)
		-- k ^= k >> 33 (shift > 32: result = (0, bit32.rshift(kh, 1)))
		kh, kl = xor64(kh, kl, 0, bit32.rshift(kh, 1))
		kh, kl = mul64(kh, kl, FMIX1h, FMIX1l)
		kh, kl = xor64(kh, kl, 0, bit32.rshift(kh, 1))
		kh, kl = mul64(kh, kl, FMIX2h, FMIX2l)
		kh, kl = xor64(kh, kl, 0, bit32.rshift(kh, 1))
		return kh, kl
	end

	local function murmur3_x64_128(buf, seedh, seedl)
		local len = buffer.len(buf)
		local nblocks = len // 16

		local h1h, h1l = seedh, seedl
		local h2h, h2l = seedh, seedl

		for i = 0, nblocks - 1 do
			local off = i * 16
			local k1h, k1l = buffer.readu32(buf, off + 4), buffer.readu32(buf, off)
			local k2h, k2l = buffer.readu32(buf, off + 12), buffer.readu32(buf, off + 8)

			k1h, k1l = mul64(k1h, k1l, C1h, C1l)
			k1h, k1l = lrotate64(k1h, k1l, 31)
			k1h, k1l = mul64(k1h, k1l, C2h, C2l)
			h1h, h1l = xor64(h1h, h1l, k1h, k1l)
			h1h, h1l = lrotate64(h1h, h1l, 27)
			h1h, h1l = add64(h1h, h1l, h2h, h2l)
			h1h, h1l = mul64(h1h, h1l, 0, 5)
			h1h, h1l = add64(h1h, h1l, 0, 0x52dce729)

			k2h, k2l = mul64(k2h, k2l, C2h, C2l)
			-- lrotate by 33: n > 32, m = 1 → swap then rotate left by 1
			k2h, k2l = bit32.bor(bit32.lshift(k2l, 1), bit32.rshift(k2h, 31)), bit32.bor(bit32.lshift(k2h, 1), bit32.rshift(k2l, 31))
			k2h, k2l = mul64(k2h, k2l, C1h, C1l)
			h2h, h2l = xor64(h2h, h2l, k2h, k2l)
			h2h, h2l = lrotate64(h2h, h2l, 31)
			h2h, h2l = add64(h2h, h2l, h1h, h1l)
			h2h, h2l = mul64(h2h, h2l, 0, 5)
			h2h, h2l = add64(h2h, h2l, 0, 0x38495ab5)
		end

		-- tail
		local tailStart = nblocks * 16
		local rem = len - tailStart
		if rem > 0 then
			local k1h, k1l = 0, 0
			local k2h, k2l = 0, 0

			if rem >= 9 then
				k1h, k1l = buffer.readu32(buf, tailStart + 4), buffer.readu32(buf, tailStart)
				for j = 8, rem - 1 do
					local shift = (j - 8) * 8
					local byte = buffer.readu8(buf, tailStart + j)
					if shift < 32 then
						k2l = bit32.bxor(k2l, bit32.lshift(byte, shift))
					else
						k2h = bit32.bxor(k2h, bit32.lshift(byte, shift - 32))
					end
				end
				k2h, k2l = mul64(k2h, k2l, C2h, C2l)
				k2h, k2l = bit32.bor(bit32.lshift(k2l, 1), bit32.rshift(k2h, 31)), bit32.bor(bit32.lshift(k2h, 1), bit32.rshift(k2l, 31))
				k2h, k2l = mul64(k2h, k2l, C1h, C1l)
				h2h, h2l = xor64(h2h, h2l, k2h, k2l)
			elseif rem == 8 then
				k1h, k1l = buffer.readu32(buf, tailStart + 4), buffer.readu32(buf, tailStart)
			else
				for j = 0, rem - 1 do
					local shift = j * 8
					local byte = buffer.readu8(buf, tailStart + j)
					if shift < 32 then
						k1l = bit32.bxor(k1l, bit32.lshift(byte, shift))
					else
						k1h = bit32.bxor(k1h, bit32.lshift(byte, shift - 32))
					end
				end
			end

			k1h, k1l = mul64(k1h, k1l, C1h, C1l)
			k1h, k1l = lrotate64(k1h, k1l, 31)
			k1h, k1l = mul64(k1h, k1l, C2h, C2l)
			h1h, h1l = xor64(h1h, h1l, k1h, k1l)
		end

		-- finalization
		h1h, h1l = xor64(h1h, h1l, 0, len)
		h2h, h2l = xor64(h2h, h2l, 0, len)

		h1h, h1l = add64(h1h, h1l, h2h, h2l)
		h2h, h2l = add64(h2h, h2l, h1h, h1l)

		h1h, h1l = fmix64(h1h, h1l)
		h2h, h2l = fmix64(h2h, h2l)

		h1h, h1l = add64(h1h, h1l, h2h, h2l)
		h2h, h2l = add64(h2h, h2l, h1h, h1l)

		return string.format("%08x%08x%08x%08x", bit32.byteswap(h1l), bit32.byteswap(h1h), bit32.byteswap(h2l), bit32.byteswap(h2h))
	end

	local input = buffer.fromstring(string.rep(".", 1e3))

	local ts0 = os.clock()

	for i = 1, 1500 do
		local res = murmur3_x64_128(input, 0, 0)
		assert(res == "d092966966d88531ef6a373f8fbb714f")
	end

	local ts1 = os.clock()

	return ts1 - ts0
end

bench.runCode(test, "murmur128_bit32")
