--!strict
-- Poly1305 one-time message authenticator (RFC 8439).
--
-- Poly1305 evaluates a polynomial in the accumulator modulo the prime
-- p = 2^130 - 5, then adds a 128-bit value `s`. The hard part in Luau is the
-- 130-bit arithmetic, since doubles are only exact to 2^53.
--
-- Representation: numbers are held as ten limbs of 13 bits (little-endian),
-- so a full number is exactly 130 bits (13 * 10). Two properties make this
-- convenient:
--   * 2^130 = (2^13)^10, so the top of the number lines up on a limb boundary
--     and the reduction 2^130 ≡ 5 (mod p) is a clean per-limb fold.
--   * Every partial product limb_i * limb_j is < 2^26, and each output column
--     sums at most ten of them (< 2^30), staying far below 2^53.

local poly1305 = {}

local LIMBS = 10
local LIMB_BITS = 13
local LIMB_BASE = 8192 -- 2^13
local LIMB_MASK = 8191 -- 2^13 - 1

-- Convert the first `n` bytes of `buf` (a little-endian integer) into ten
-- base-2^13 limbs. Bits beyond 130 are dropped, but callers never supply that
-- many. A bit accumulator shifts whole bytes in and emits 13-bit limbs.
local function bytesToLimbs(buf: buffer, n: number): { number }
	local limbs = table.create(LIMBS)
	for i = 1, LIMBS do
		limbs[i] = 0
	end

	local acc = 0
	local accBits = 0
	local li = 1
	for i = 0, n - 1 do
		acc += buffer.readu8(buf, i) * (2 ^ accBits)
		accBits += 8
		while accBits >= LIMB_BITS and li <= LIMBS do
			limbs[li] = acc % LIMB_BASE
			acc = math.floor(acc / LIMB_BASE)
			accBits -= LIMB_BITS
			li += 1
		end
	end
	if li <= LIMBS then
		limbs[li] = acc % LIMB_BASE
	end
	return limbs
end

-- Convert ten base-2^13 limbs into `n` little-endian bytes written to `out`.
local function limbsToBytes(limbs: { number }, out: buffer, n: number)
	local acc = 0
	local accBits = 0
	local oi = 0
	for i = 1, LIMBS do
		acc += limbs[i] * (2 ^ accBits)
		accBits += LIMB_BITS
		while accBits >= 8 and oi < n do
			buffer.writeu8(out, oi, acc % 256)
			acc = math.floor(acc / 256)
			accBits -= 8
			oi += 1
		end
	end
	while oi < n do
		buffer.writeu8(out, oi, acc % 256)
		acc = math.floor(acc / 256)
		oi += 1
	end
end

-- Propagate carries through ten limbs, folding any overflow past bit 130 back
-- in as `carry * 5` (since 2^130 ≡ 5 mod p). Operates in place.
local function carryReduce(limbs: { number })
	local c = 0
	for i = 1, LIMBS do
		local v = limbs[i] + c
		limbs[i] = v % LIMB_BASE
		c = math.floor(v / LIMB_BASE)
	end
	-- Fold the >=2^130 part back in. Each pass shrinks the carry quickly.
	while c > 0 do
		c = c * 5
		for i = 1, LIMBS do
			local v = limbs[i] + c
			limbs[i] = v % LIMB_BASE
			c = math.floor(v / LIMB_BASE)
			if c == 0 then
				break
			end
		end
	end
end

-- Multiply two reduced numbers modulo p = 2^130 - 5. Schoolbook 10x10 with the
-- high limbs folded via 2^130 ≡ 5, then a carry pass.
local function mulmod(a: { number }, b: { number }): { number }
	-- Product limbs d[1..19] (indices 1..19 correspond to limb positions 0..18).
	local d = table.create(2 * LIMBS - 1)
	for k = 1, 2 * LIMBS - 1 do
		d[k] = 0
	end

	for i = 1, LIMBS do
		local ai = a[i]
		if ai ~= 0 then
			for j = 1, LIMBS do
				d[i + j - 1] += ai * b[j]
			end
		end
	end

	-- Fold limbs at positions >= 10 (bit >= 130) into low limbs times 5.
	for k = 2 * LIMBS - 1, LIMBS + 1, -1 do
		d[k - LIMBS] += 5 * d[k]
	end

	local out = table.create(LIMBS)
	for i = 1, LIMBS do
		out[i] = d[i]
	end
	carryReduce(out)
	return out
end

-- Add `b` into `a` limb-wise (no reduction; caller reduces via mulmod).
local function addInto(a: { number }, b: { number })
	for i = 1, LIMBS do
		a[i] += b[i]
	end
end

-- Read and clamp the 16-byte `r` half of the key per RFC 8439.
local function clampedR(key: buffer): { number }
	local r = buffer.create(16)
	buffer.copy(r, 0, key, 0, 16)
	-- Clear the top 4 bits of bytes 3, 7, 11, 15.
	buffer.writeu8(r, 3, bit32.band(buffer.readu8(r, 3), 15))
	buffer.writeu8(r, 7, bit32.band(buffer.readu8(r, 7), 15))
	buffer.writeu8(r, 11, bit32.band(buffer.readu8(r, 11), 15))
	buffer.writeu8(r, 15, bit32.band(buffer.readu8(r, 15), 15))
	-- Clear the bottom 2 bits of bytes 4, 8, 12.
	buffer.writeu8(r, 4, bit32.band(buffer.readu8(r, 4), 252))
	buffer.writeu8(r, 8, bit32.band(buffer.readu8(r, 8), 252))
	buffer.writeu8(r, 12, bit32.band(buffer.readu8(r, 12), 252))
	return bytesToLimbs(r, 16)
end

-- Conditionally subtract p so the accumulator lands in [0, p). Computes
-- h + 5; if that carries past bit 130 then h >= p, so the wrapped value
-- (h + 5 - 2^130) is the reduced result.
local function freeze(h: { number })
	local g = table.create(LIMBS)
	local c = 5
	for i = 1, LIMBS do
		local v = h[i] + c
		g[i] = v % LIMB_BASE
		c = math.floor(v / LIMB_BASE)
	end
	if c > 0 then
		for i = 1, LIMBS do
			h[i] = g[i]
		end
	end
end

poly1305.TAG_SIZE = 16
poly1305.KEY_SIZE = 32

-- Compute the 16-byte Poly1305 tag over `msg` using the 32-byte one-time key.
function poly1305.mac(key: buffer, msg: buffer): buffer
	assert(buffer.len(key) == 32, "Poly1305 key must be 32 bytes")

	local r = clampedR(key)
	local h = table.create(LIMBS)
	for i = 1, LIMBS do
		h[i] = 0
	end

	local n = buffer.len(msg)
	local off = 0
	while off < n do
		local len = if n - off < 16 then n - off else 16
		-- Build the block padded with the high "1" bit: for a full block that
		-- is 2^128; for a short final block it is 2^(8*len).
		local blk = buffer.create(len + 1)
		buffer.copy(blk, 0, msg, off, len)
		buffer.writeu8(blk, len, 1)

		local m = bytesToLimbs(blk, len + 1)
		addInto(h, m)
		h = mulmod(h, r)
		off += len
	end

	freeze(h)

	-- tag = (h + s) mod 2^128, where s is the second half of the key.
	local out = buffer.create(16)
	limbsToBytes(h, out, 16)
	local carry = 0
	for i = 0, 15 do
		local v = buffer.readu8(out, i) + buffer.readu8(key, 16 + i) + carry
		buffer.writeu8(out, i, v % 256)
		carry = math.floor(v / 256)
	end
	return out
end

-- Constant-time-ish tag comparison (length is public). Returns true on match.
function poly1305.verify(tagA: buffer, tagB: buffer): boolean
	if buffer.len(tagA) ~= 16 or buffer.len(tagB) ~= 16 then
		return false
	end
	local diff = 0
	for i = 0, 15 do
		diff = bit32.bor(diff, bit32.bxor(buffer.readu8(tagA, i), buffer.readu8(tagB, i)))
	end
	return diff == 0
end

return poly1305
