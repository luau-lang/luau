--!strict
-- SHA-256 (FIPS 180-4) plus HMAC-SHA256, used as the Noise hash function.
--
-- Words are 32-bit. Logical operations use `bit32`; additions accumulate a few
-- 32-bit terms (sum < 2^35, exact as a double) and are masked back to 32 bits.

local wordmath = require("./wordmath")

local sha256 = {}

local TWO32 = 0x100000000
local rotr = wordmath.rotr32

-- Round constants (first 32 bits of the fractional parts of the cube roots of
-- the first 64 primes).
local K = {
	0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
	0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
	0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
	0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
	0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
	0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
	0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
	0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
}

-- Initial hash value (first 32 bits of the fractional parts of the square roots
-- of the first 8 primes).
local H0 = { 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19 }

-- Read a big-endian 32-bit word from a buffer.
local function readBE32(buf: buffer, off: number): number
	return buffer.readu8(buf, off) * 0x1000000
		+ buffer.readu8(buf, off + 1) * 0x10000
		+ buffer.readu8(buf, off + 2) * 0x100
		+ buffer.readu8(buf, off + 3)
end

-- Write a big-endian 32-bit word into a buffer.
local function writeBE32(buf: buffer, off: number, value: number)
	buffer.writeu8(buf, off, bit32.band(bit32.rshift(value, 24), 0xff))
	buffer.writeu8(buf, off + 1, bit32.band(bit32.rshift(value, 16), 0xff))
	buffer.writeu8(buf, off + 2, bit32.band(bit32.rshift(value, 8), 0xff))
	buffer.writeu8(buf, off + 3, bit32.band(value, 0xff))
end

-- Pad a message per FIPS 180-4 and return a new buffer whose length is a
-- multiple of 64 bytes.
local function pad(msg: buffer): buffer
	local len = buffer.len(msg)
	local bitLen = len * 8
	-- 1 byte 0x80, then k zero bytes, then 8 bytes length: total multiple of 64.
	local padded = ((len + 1 + 8 + 63) // 64) * 64
	local out = buffer.create(padded)
	buffer.copy(out, 0, msg, 0, len)
	buffer.writeu8(out, len, 0x80)
	-- 64-bit big-endian bit length (split into high/low 32-bit words).
	writeBE32(out, padded - 8, math.floor(bitLen / TWO32))
	writeBE32(out, padded - 4, bitLen % TWO32)
	return out
end

sha256.DIGEST_SIZE = 32
sha256.BLOCK_SIZE = 64

-- Compute the SHA-256 digest of `msg`, returning a 32-byte buffer.
function sha256.hash(msg: buffer): buffer
	local data = pad(msg)
	local h = table.clone(H0)
	local w = table.create(64)

	for base = 0, buffer.len(data) - 1, 64 do
		-- Message schedule.
		for t = 0, 15 do
			w[t + 1] = readBE32(data, base + t * 4)
		end
		for t = 16, 63 do
			local w15 = w[t - 15 + 1]
			local w2 = w[t - 2 + 1]
			local s0 = bit32.bxor(rotr(w15, 7), rotr(w15, 18), bit32.rshift(w15, 3))
			local s1 = bit32.bxor(rotr(w2, 17), rotr(w2, 19), bit32.rshift(w2, 10))
			w[t + 1] = (w[t - 16 + 1] + s0 + w[t - 7 + 1] + s1) % TWO32
		end

		local a, b, c, d, e, f, g, hh = h[1], h[2], h[3], h[4], h[5], h[6], h[7], h[8]

		for t = 1, 64 do
			local S1 = bit32.bxor(rotr(e, 6), rotr(e, 11), rotr(e, 25))
			local ch = bit32.bxor(bit32.band(e, f), bit32.band(bit32.bnot(e), g))
			local temp1 = (hh + S1 + ch + K[t] + w[t]) % TWO32
			local S0 = bit32.bxor(rotr(a, 2), rotr(a, 13), rotr(a, 22))
			local maj = bit32.bxor(bit32.band(a, b), bit32.band(a, c), bit32.band(b, c))
			local temp2 = (S0 + maj) % TWO32

			hh = g
			g = f
			f = e
			e = (d + temp1) % TWO32
			d = c
			c = b
			b = a
			a = (temp1 + temp2) % TWO32
		end

		h[1] = (h[1] + a) % TWO32
		h[2] = (h[2] + b) % TWO32
		h[3] = (h[3] + c) % TWO32
		h[4] = (h[4] + d) % TWO32
		h[5] = (h[5] + e) % TWO32
		h[6] = (h[6] + f) % TWO32
		h[7] = (h[7] + g) % TWO32
		h[8] = (h[8] + hh) % TWO32
	end

	local out = buffer.create(32)
	for i = 1, 8 do
		writeBE32(out, (i - 1) * 4, h[i])
	end
	return out
end

-- HMAC-SHA256 (RFC 2104) over `msg` with `key`. Returns a 32-byte tag.
function sha256.hmac(key: buffer, msg: buffer): buffer
	local blockSize = 64
	-- Keys longer than the block size are hashed first.
	local k = buffer.create(blockSize)
	if buffer.len(key) > blockSize then
		local hk = sha256.hash(key)
		buffer.copy(k, 0, hk, 0, buffer.len(hk))
	else
		buffer.copy(k, 0, key, 0, buffer.len(key))
	end

	local ipad = buffer.create(blockSize + buffer.len(msg))
	local opad = buffer.create(blockSize + 32)
	for i = 0, blockSize - 1 do
		local kb = buffer.readu8(k, i)
		buffer.writeu8(ipad, i, bit32.bxor(kb, 0x36))
		buffer.writeu8(opad, i, bit32.bxor(kb, 0x5c))
	end
	buffer.copy(ipad, blockSize, msg, 0, buffer.len(msg))

	local inner = sha256.hash(ipad)
	buffer.copy(opad, blockSize, inner, 0, 32)
	return sha256.hash(opad)
end

return sha256
