--!strict
-- X25519 Elliptic-Curve Diffie-Hellman (RFC 7748).
--
-- Field arithmetic is over GF(2^255 - 19). Field elements are represented as
-- fifteen uniform limbs of 17 bits (little-endian): 17 * 15 = 255 bits, so the
-- number lines up exactly on 2^255 and the reduction 2^255 ≡ 19 (mod p) is a
-- clean per-limb fold. Every limb*limb product is < 2^34 and each output column
-- sums at most ~15 of them (times the small 19 fold factor), staying well below
-- 2^53 -- the exactness limit for Luau doubles.

local x25519 = {}

local LIMBS = 15
local RADIX = 17
local BASE = 131072 -- 2^17
local MASK = 131071 -- 2^17 - 1

-- === field element construction ==========================================

local function feZero(): { number }
	local f = table.create(LIMBS)
	for i = 1, LIMBS do
		f[i] = 0
	end
	return f
end

local function feOne(): { number }
	local f = feZero()
	f[1] = 1
	return f
end

local function feCopy(a: { number }): { number }
	local f = table.create(LIMBS)
	for i = 1, LIMBS do
		f[i] = a[i]
	end
	return f
end

-- === carry / reduction ===================================================

-- Propagate carries so every limb is < 2^17, folding overflow past bit 255
-- back in as (carry * 19), since 2^255 ≡ 19 (mod p). A few passes converge
-- because each pass shrinks the residual carry sharply.
local function carry(f: { number })
	for _ = 1, 4 do
		local c = 0
		for i = 1, LIMBS do
			local v = f[i] + c
			f[i] = v % BASE
			c = math.floor(v / BASE)
		end
		if c == 0 then
			break
		end
		f[1] += c * 19
	end
end

-- === field arithmetic ====================================================

-- Multiply two field elements modulo p.
local function feMul(a: { number }, b: { number }): { number }
	local c = table.create(2 * LIMBS - 1)
	for k = 1, 2 * LIMBS - 1 do
		c[k] = 0
	end

	for i = 1, LIMBS do
		local ai = a[i]
		if ai ~= 0 then
			for j = 1, LIMBS do
				c[i + j - 1] += ai * b[j]
			end
		end
	end

	-- Fold product limbs at bit >= 255 (index >= LIMBS) into low limbs * 19.
	for k = 2 * LIMBS - 1, LIMBS + 1, -1 do
		c[k - LIMBS] += 19 * c[k]
	end

	local out = table.create(LIMBS)
	for i = 1, LIMBS do
		out[i] = c[i]
	end
	carry(out)
	return out
end

-- Square (just a multiply by itself; correctness over micro-optimization).
local function feSquare(a: { number }): { number }
	return feMul(a, a)
end

-- Add two reduced field elements.
local function feAdd(a: { number }, b: { number }): { number }
	local out = table.create(LIMBS)
	for i = 1, LIMBS do
		out[i] = a[i] + b[i]
	end
	carry(out)
	return out
end

-- Precomputed 2*p in this radix, used to keep subtraction non-negative.
-- p = 2^255 - 19, so p_0 = 2^17 - 19 and p_1..p_14 = 2^17 - 1.
local TWO_P = {}
do
	TWO_P[1] = 2 * (BASE - 19)
	for i = 2, LIMBS do
		TWO_P[i] = 2 * (BASE - 1)
	end
end

-- Subtract reduced `b` from reduced `a`. Adding 2*p (≡ 0 mod p) first keeps
-- every limb non-negative before the carry.
local function feSub(a: { number }, b: { number }): { number }
	local out = table.create(LIMBS)
	for i = 1, LIMBS do
		out[i] = a[i] + TWO_P[i] - b[i]
	end
	carry(out)
	return out
end

-- Multiply a field element by a small (< 2^17) constant.
local function feMulConst(a: { number }, k: number): { number }
	local out = table.create(LIMBS)
	for i = 1, LIMBS do
		out[i] = a[i] * k
	end
	carry(out)
	return out
end

-- Reduce to the canonical representative in [0, p): conditionally subtract p.
-- After carry(), f < 2^255, so at most one subtraction is needed. Adding 19 and
-- checking for a carry past bit 255 tells us whether f >= p.
local function feFreeze(f: { number })
	carry(f)
	local c = 19
	local g = table.create(LIMBS)
	for i = 1, LIMBS do
		local v = f[i] + c
		g[i] = v % BASE
		c = math.floor(v / BASE)
	end
	if c > 0 then
		-- f + 19 >= 2^255  =>  f >= p; g is f + 19 - 2^255 = f - p.
		for i = 1, LIMBS do
			f[i] = g[i]
		end
	end
end

-- Square `a` repeatedly, `k` times.
local function fePow2k(a: { number }, k: number): { number }
	local r = a
	for _ = 1, k do
		r = feSquare(r)
	end
	return r
end

-- Modular inverse via Fermat: a^(p-2). Uses the standard curve25519 addition
-- chain (djb ref10 fe_invert) to reach the exponent 2^255 - 21.
local function feInvert(z: { number }): { number }
	local t0 = feSquare(z) -- z^2
	local t1 = fePow2k(t0, 2) -- z^8
	t1 = feMul(z, t1) -- z^9
	t0 = feMul(t0, t1) -- z^11
	local t2 = feSquare(t0) -- z^22
	t1 = feMul(t1, t2) -- z^(2^5 - 1)
	t2 = fePow2k(t1, 5)
	t1 = feMul(t2, t1) -- 2^10 - 1
	t2 = fePow2k(t1, 10)
	t2 = feMul(t2, t1) -- 2^20 - 1
	local t3 = fePow2k(t2, 20)
	t2 = feMul(t3, t2) -- 2^40 - 1
	t2 = fePow2k(t2, 10)
	t1 = feMul(t2, t1) -- 2^50 - 1
	t2 = fePow2k(t1, 50)
	t2 = feMul(t2, t1) -- 2^100 - 1
	t3 = fePow2k(t2, 100)
	t2 = feMul(t3, t2) -- 2^200 - 1
	t2 = fePow2k(t2, 50)
	t1 = feMul(t2, t1) -- 2^250 - 1
	t1 = fePow2k(t1, 5)
	return feMul(t1, t0) -- 2^255 - 21
end

-- === encode / decode =====================================================

-- Decode 32 little-endian bytes into a field element (15 limbs of 17 bits).
local function feFromBytes(buf: buffer): { number }
	local limbs = feZero()
	local acc = 0
	local accBits = 0
	local li = 1
	for i = 0, 31 do
		acc += buffer.readu8(buf, i) * (2 ^ accBits)
		accBits += 8
		while accBits >= RADIX and li <= LIMBS do
			limbs[li] = acc % BASE
			acc = math.floor(acc / BASE)
			accBits -= RADIX
			li += 1
		end
	end
	return limbs
end

-- Encode a field element into 32 little-endian bytes (canonical form).
local function feToBytes(f: { number }): buffer
	feFreeze(f)
	local out = buffer.create(32)
	local acc = 0
	local accBits = 0
	local oi = 0
	for i = 1, LIMBS do
		acc += f[i] * (2 ^ accBits)
		accBits += RADIX
		while accBits >= 8 and oi < 32 do
			buffer.writeu8(out, oi, acc % 256)
			acc = math.floor(acc / 256)
			accBits -= 8
			oi += 1
		end
	end
	while oi < 32 do
		buffer.writeu8(out, oi, acc % 256)
		acc = math.floor(acc / 256)
		oi += 1
	end
	return out
end

-- === scalar / ladder =====================================================

-- Constant-time conditional swap of two field elements, in place.
-- With swapv in {0,1}: a'[i] = a[i] - swap*(a[i]-b[i]), which is a[i] when
-- swap=0 and b[i] when swap=1. Branch-free.
local function cswap(swapv: number, a: { number }, b: { number })
	for i = 1, LIMBS do
		local ai = a[i]
		local bi = b[i]
		a[i] = ai - swapv * (ai - bi)
		b[i] = bi + swapv * (ai - bi)
	end
end

-- Clamp a 32-byte scalar per RFC 7748 and return a copy.
local function clampScalar(scalar: buffer): buffer
	local s = buffer.create(32)
	buffer.copy(s, 0, scalar, 0, 32)
	buffer.writeu8(s, 0, bit32.band(buffer.readu8(s, 0), 248))
	buffer.writeu8(s, 31, bit32.band(buffer.readu8(s, 31), 127))
	buffer.writeu8(s, 31, bit32.bor(buffer.readu8(s, 31), 64))
	return s
end

-- Extract bit `t` (0-based) from a little-endian byte buffer.
local function scalarBit(s: buffer, t: number): number
	local byte = buffer.readu8(s, t // 8)
	return bit32.band(bit32.rshift(byte, t % 8), 1)
end

x25519.SCALAR_SIZE = 32
x25519.POINT_SIZE = 32

-- Compute the X25519 scalar multiplication of `scalar` (32 bytes) and the
-- u-coordinate `uBytes` (32 bytes). Returns the resulting 32-byte u-coordinate.
function x25519.scalarMult(scalar: buffer, uBytes: buffer): buffer
	assert(buffer.len(scalar) == 32, "scalar must be 32 bytes")
	assert(buffer.len(uBytes) == 32, "u-coordinate must be 32 bytes")

	local k = clampScalar(scalar)

	-- Decode u with the high bit of the last byte masked off (RFC 7748).
	local uMasked = buffer.create(32)
	buffer.copy(uMasked, 0, uBytes, 0, 32)
	buffer.writeu8(uMasked, 31, bit32.band(buffer.readu8(uMasked, 31), 127))
	local x1 = feFromBytes(uMasked)

	local x2 = feOne()
	local z2 = feZero()
	local x3 = feCopy(x1)
	local z3 = feOne()
	local swap = 0

	for t = 254, 0, -1 do
		local kt = scalarBit(k, t)
		swap = bit32.bxor(swap, kt)
		cswap(swap, x2, x3)
		cswap(swap, z2, z3)
		swap = kt

		local A = feAdd(x2, z2)
		local AA = feSquare(A)
		local B = feSub(x2, z2)
		local BB = feSquare(B)
		local E = feSub(AA, BB)
		local C = feAdd(x3, z3)
		local D = feSub(x3, z3)
		local DA = feMul(D, A)
		local CB = feMul(C, B)

		local daPlusCb = feAdd(DA, CB)
		x3 = feSquare(daPlusCb)
		local daMinusCb = feSub(DA, CB)
		z3 = feMul(x1, feSquare(daMinusCb))

		x2 = feMul(AA, BB)
		-- a24 = 121665
		z2 = feMul(E, feAdd(AA, feMulConst(E, 121665)))
	end

	cswap(swap, x2, x3)
	cswap(swap, z2, z3)

	local result = feMul(x2, feInvert(z2))
	return feToBytes(result)
end

-- The Curve25519 base point u = 9.
local BASEPOINT = buffer.create(32)
buffer.writeu8(BASEPOINT, 0, 9)
x25519.BASEPOINT = BASEPOINT

-- Derive the public key for a given 32-byte secret scalar.
function x25519.publicKey(secret: buffer): buffer
	return x25519.scalarMult(secret, BASEPOINT)
end

return x25519
