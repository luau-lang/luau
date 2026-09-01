--!strict
-- ML-KEM-768 (FIPS 203), the post-quantum key-encapsulation mechanism.
--
-- Polynomials live in R_q = Z_q[X]/(X^256 + 1) with q = 3329, represented as
-- 0-based coefficient tables (coeffs[0..255]). q^2 and even q^3 fit exactly in
-- a double, so plain `%` arithmetic is exact.
--
-- The structure mirrors FIPS 203: small poly/vector/matrix helpers, sampling
-- (SampleNTT and the centered binomial distribution), byte (de)serialization,
-- the K-PKE public-key scheme, and the ML-KEM Fujisaki-Okamoto wrapper.

local keccak = require("./keccak")

local mlkem = {}

local N = 256
local Q = 3329
local K = 3 -- module rank for ML-KEM-768
local ETA1 = 2
local ETA2 = 2
local DU = 10
local DV = 4
local NTT_F = 3303 -- 128^-1 mod q

mlkem.EK_SIZE = 384 * K + 32 -- 1184
mlkem.DK_SIZE = 768 * K + 96 -- 2400
mlkem.C_SIZE = 32 * (DU * K + DV) -- 1088
mlkem.SS_SIZE = 32
mlkem.D_SIZE = 32
mlkem.Z_SIZE = 32
mlkem.M_SIZE = 32

-- === zeta table ==========================================================

-- Bit-reversal of the low 7 bits of `i`.
local function bitRev7(i: number): number
	local r = 0
	for b = 0, 6 do
		if bit32.band(bit32.rshift(i, b), 1) == 1 then
			r += bit32.lshift(1, 6 - b)
		end
	end
	return r
end

-- ZETAS[i] = 17^bitRev7(i) mod q, for i in 0..127 (0-based table).
local ZETAS = {}
do
	local z = 1
	-- Build 17^e mod q on demand via fast exponentiation.
	local function powmod(base: number, exp: number): number
		local result = 1
		local b = base % Q
		while exp > 0 do
			if exp % 2 == 1 then
				result = (result * b) % Q
			end
			b = (b * b) % Q
			exp = math.floor(exp / 2)
		end
		return result
	end
	for i = 0, 127 do
		ZETAS[i] = powmod(17, bitRev7(i))
	end
	z = z -- silence unused
end

-- === polynomial arithmetic ===============================================

local function polyZero(): { number }
	local p = {}
	for i = 0, 255 do
		p[i] = 0
	end
	return p
end

local function polyCopy(a: { number }): { number }
	local p = {}
	for i = 0, 255 do
		p[i] = a[i]
	end
	return p
end

local function polyAdd(a: { number }, b: { number }): { number }
	local p = {}
	for i = 0, 255 do
		p[i] = (a[i] + b[i]) % Q
	end
	return p
end

local function polySub(a: { number }, b: { number }): { number }
	local p = {}
	for i = 0, 255 do
		p[i] = (a[i] - b[i]) % Q
	end
	return p
end

-- Forward NTT, in place (Cooley-Tukey). Standard order in, bit-reversed out.
local function ntt(f: { number }): { number }
	local k = 1
	local len = 128
	while len >= 2 do
		local start = 0
		while start < 256 do
			local zeta = ZETAS[k]
			k += 1
			for j = start, start + len - 1 do
				local t = (zeta * f[j + len]) % Q
				f[j + len] = (f[j] - t) % Q
				f[j] = (f[j] + t) % Q
			end
			start += 2 * len
		end
		len = math.floor(len / 2)
	end
	return f
end

-- Inverse NTT, in place (Gentleman-Sande), including the 128^-1 scaling.
local function invNtt(f: { number }): { number }
	local k = 127
	local len = 2
	while len <= 128 do
		local start = 0
		while start < 256 do
			local zeta = ZETAS[k]
			k -= 1
			for j = start, start + len - 1 do
				local t = f[j]
				f[j] = (t + f[j + len]) % Q
				f[j + len] = (zeta * ((f[j + len] - t) % Q)) % Q
			end
			start += 2 * len
		end
		len *= 2
	end
	for j = 0, 255 do
		f[j] = (f[j] * NTT_F) % Q
	end
	return f
end

-- Base-case multiply of two degree-1 polynomials mod (X^2 - zeta).
local function baseMul(a0: number, a1: number, b0: number, b1: number, zeta: number): (number, number)
	local r0 = (a0 * b0 + zeta * a1 * b1) % Q
	local r1 = (a1 * b0 + a0 * b1) % Q
	return r0, r1
end

-- Pointwise multiply two NTT-domain polynomials.
local function nttMul(f: { number }, g: { number }): { number }
	local h = {}
	for i = 0, 63 do
		local zeta = ZETAS[64 + i]
		local r0, r1 = baseMul(f[4 * i], f[4 * i + 1], g[4 * i], g[4 * i + 1], zeta)
		local r2, r3 = baseMul(f[4 * i + 2], f[4 * i + 3], g[4 * i + 2], g[4 * i + 3], (Q - zeta) % Q)
		h[4 * i] = r0
		h[4 * i + 1] = r1
		h[4 * i + 2] = r2
		h[4 * i + 3] = r3
	end
	return h
end

-- === vector / matrix (module) arithmetic =================================

type Vector = { { number } }
type Matrix = { { { number } } }

local function vecNtt(v: Vector): Vector
	for i = 1, K do
		ntt(v[i])
	end
	return v
end

local function vecInvNtt(v: Vector): Vector
	for i = 1, K do
		invNtt(v[i])
	end
	return v
end

local function vecAdd(a: Vector, b: Vector): Vector
	local r = {}
	for i = 1, K do
		r[i] = polyAdd(a[i], b[i])
	end
	return r
end

-- Matrix-vector product in the NTT domain. With `transpose`, uses A[j][i].
local function matVecMul(A: Matrix, v: Vector, transpose: boolean): Vector
	local w = {}
	for i = 1, K do
		local acc = polyZero()
		for j = 1, K do
			local a = if transpose then A[j][i] else A[i][j]
			acc = polyAdd(acc, nttMul(a, v[j]))
		end
		w[i] = acc
	end
	return w
end

-- Inner product of two NTT-domain vectors -> single NTT-domain polynomial.
local function dotNtt(u: Vector, v: Vector): { number }
	local acc = polyZero()
	for i = 1, K do
		acc = polyAdd(acc, nttMul(u[i], v[i]))
	end
	return acc
end

-- === sampling ============================================================

-- SampleNTT (FIPS 203 Algorithm 7): rejection-sample an NTT-domain polynomial
-- from a 34-byte seed via SHAKE128. The 12-bit values < q are kept.
local function sampleNtt(seed: buffer): { number }
	local poly = polyZero()
	local outLen = 864 -- multiple of 3; usually enough for 256 coefficients
	local xof = keccak.shake128(seed, outLen)
	local i = 0
	local j = 0
	while j < 256 do
		if i + 3 > outLen then
			-- Extremely rare: extend the (identical-prefix) SHAKE stream.
			outLen *= 2
			xof = keccak.shake128(seed, outLen)
		end
		local b0 = buffer.readu8(xof, i)
		local b1 = buffer.readu8(xof, i + 1)
		local b2 = buffer.readu8(xof, i + 2)
		local d1 = b0 + 256 * (b1 % 16)
		local d2 = math.floor(b1 / 16) + 16 * b2
		if d1 < Q then
			poly[j] = d1
			j += 1
		end
		if d2 < Q and j < 256 then
			poly[j] = d2
			j += 1
		end
		i += 3
	end
	return poly
end

-- SamplePolyCBD (FIPS 203 Algorithm 8): centered binomial distribution from a
-- PRF byte stream, reading 2*eta bits per coefficient (LSB first).
local function samplePolyCbd(buf: buffer, eta: number): { number }
	local poly = {}
	local acc = 0
	local accBits = 0
	local bi = 0
	local step = 2 * eta
	for idx = 0, 255 do
		while accBits < step do
			acc += buffer.readu8(buf, bi) * (2 ^ accBits)
			accBits += 8
			bi += 1
		end
		local a = 0
		for t = 0, eta - 1 do
			a += math.floor(acc / (2 ^ t)) % 2
		end
		local b = 0
		for t = 0, eta - 1 do
			b += math.floor(acc / (2 ^ (eta + t))) % 2
		end
		acc = math.floor(acc / (2 ^ step))
		accBits -= step
		poly[idx] = (a - b) % Q
	end
	return poly
end

-- === byte (de)serialization ==============================================

-- ByteEncode_d: pack 256 d-bit coefficients into 32*d bytes (LSB first).
local function byteEncode(poly: { number }, d: number): buffer
	local out = buffer.create(32 * d)
	local acc = 0
	local accBits = 0
	local oi = 0
	for i = 0, 255 do
		acc += poly[i] * (2 ^ accBits)
		accBits += d
		while accBits >= 8 do
			buffer.writeu8(out, oi, acc % 256)
			acc = math.floor(acc / 256)
			accBits -= 8
			oi += 1
		end
	end
	return out
end

-- ByteDecode_d: unpack 32*d bytes into 256 coefficients. For d == 12 the values
-- are reduced mod q (canonical decoding).
local function byteDecode(buf: buffer, off: number, d: number): { number }
	local poly = {}
	local acc = 0
	local accBits = 0
	local bi = off
	local twoD = 2 ^ d
	for i = 0, 255 do
		while accBits < d do
			acc += buffer.readu8(buf, bi) * (2 ^ accBits)
			accBits += 8
			bi += 1
		end
		local v = acc % twoD
		acc = math.floor(acc / twoD)
		accBits -= d
		if d == 12 then
			v = v % Q
		end
		poly[i] = v
	end
	return poly
end

-- Compress / decompress a single coefficient (FIPS 203 Section 4.2.1).
local function compressEle(x: number, d: number): number
	local t = 2 ^ d
	return math.floor((t * x + 1664) / Q) % t
end

local function decompressEle(x: number, d: number): number
	local t = 2 ^ (d - 1)
	return math.floor((Q * x + t) / (2 ^ d))
end

local function compressPoly(poly: { number }, d: number): { number }
	local p = {}
	for i = 0, 255 do
		p[i] = compressEle(poly[i], d)
	end
	return p
end

local function decompressPoly(poly: { number }, d: number): { number }
	local p = {}
	for i = 0, 255 do
		p[i] = decompressEle(poly[i], d)
	end
	return p
end

-- Encode / decode a whole vector (concatenated per-polynomial encodings).
local function encodeVector(v: Vector, d: number): buffer
	local out = buffer.create(K * 32 * d)
	for i = 1, K do
		local enc = byteEncode(v[i], d)
		buffer.copy(out, (i - 1) * 32 * d, enc, 0, 32 * d)
	end
	return out
end

local function decodeVector(buf: buffer, off: number, d: number): Vector
	local v = {}
	for i = 1, K do
		v[i] = byteDecode(buf, off + (i - 1) * 32 * d, d)
	end
	return v
end

local function compressVector(v: Vector, d: number): Vector
	local r = {}
	for i = 1, K do
		r[i] = compressPoly(v[i], d)
	end
	return r
end

local function decompressVector(v: Vector, d: number): Vector
	local r = {}
	for i = 1, K do
		r[i] = decompressPoly(v[i], d)
	end
	return r
end

-- === hash / PRF / XOF helpers ============================================

-- Concatenate a list of buffers into one.
local function concat(parts: { buffer }): buffer
	local total = 0
	for _, p in parts do
		total += buffer.len(p)
	end
	local out = buffer.create(total)
	local off = 0
	for _, p in parts do
		buffer.copy(out, off, p, 0, buffer.len(p))
		off += buffer.len(p)
	end
	return out
end

local function byteBuf(v: number): buffer
	local b = buffer.create(1)
	buffer.writeu8(b, 0, v)
	return b
end

-- H = SHA3-256, J = SHAKE256(.,32), G = SHA3-512 split into two halves.
local function H(s: buffer): buffer
	return keccak.sha3_256(s)
end

local function J(s: buffer): buffer
	return keccak.shake256(s, 32)
end

local function G(s: buffer): (buffer, buffer)
	local h = keccak.sha3_512(s)
	local a = buffer.create(32)
	local b = buffer.create(32)
	buffer.copy(a, 0, h, 0, 32)
	buffer.copy(b, 0, h, 32, 32)
	return a, b
end

-- PRF_eta(s, b) = SHAKE256(s || b, 64*eta).
local function PRF(eta: number, s: buffer, b: number): buffer
	return keccak.shake256(concat({ s, byteBuf(b) }), 64 * eta)
end

-- Generate the matrix A_hat from seed rho. A[i][j] = SampleNTT(rho || j || i),
-- matching FIPS 203 / the reference (indices j then i in the XOF input).
local function genMatrix(rho: buffer): Matrix
	local A = {}
	for i = 1, K do
		A[i] = {}
		for j = 1, K do
			local seed = concat({ rho, byteBuf(j - 1), byteBuf(i - 1) })
			A[i][j] = sampleNtt(seed)
		end
	end
	return A
end

-- === K-PKE ===============================================================

-- K-PKE.KeyGen (FIPS 203 Algorithm 13).
local function kpkeKeygen(d: buffer): (buffer, buffer)
	local rho, sigma = G(concat({ d, byteBuf(K) }))
	local A = genMatrix(rho)

	local nCount = 0
	local s: Vector = {}
	for i = 1, K do
		s[i] = samplePolyCbd(PRF(ETA1, sigma, nCount), ETA1)
		nCount += 1
	end
	local e: Vector = {}
	for i = 1, K do
		e[i] = samplePolyCbd(PRF(ETA1, sigma, nCount), ETA1)
		nCount += 1
	end

	vecNtt(s)
	vecNtt(e)
	local t = vecAdd(matVecMul(A, s, false), e)

	local ekPke = concat({ encodeVector(t, 12), rho })
	local dkPke = encodeVector(s, 12)
	return ekPke, dkPke
end

-- K-PKE.Encrypt (FIPS 203 Algorithm 14).
local function kpkeEncrypt(ekPke: buffer, m: buffer, r: buffer): buffer
	local t = decodeVector(ekPke, 0, 12)
	local rho = buffer.create(32)
	buffer.copy(rho, 0, ekPke, 384 * K, 32)
	local A = genMatrix(rho)

	local nCount = 0
	local y: Vector = {}
	for i = 1, K do
		y[i] = samplePolyCbd(PRF(ETA1, r, nCount), ETA1)
		nCount += 1
	end
	local e1: Vector = {}
	for i = 1, K do
		e1[i] = samplePolyCbd(PRF(ETA2, r, nCount), ETA2)
		nCount += 1
	end
	local e2 = samplePolyCbd(PRF(ETA2, r, nCount), ETA2)

	vecNtt(y)
	local u = vecInvNtt(matVecMul(A, y, true))
	u = vecAdd(u, e1)

	local mu = decompressPoly(byteDecode(m, 0, 1), 1)
	local v = invNtt(dotNtt(t, y))
	v = polyAdd(polyAdd(v, e2), mu)

	local c1 = encodeVector(compressVector(u, DU), DU)
	local c2 = byteEncode(compressPoly(v, DV), DV)
	return concat({ c1, c2 })
end

-- K-PKE.Decrypt (FIPS 203 Algorithm 15).
local function kpkeDecrypt(dkPke: buffer, c: buffer): buffer
	local c1Len = 32 * DU * K
	local u = decompressVector(decodeVector(c, 0, DU), DU)
	local v = decompressPoly(byteDecode(c, c1Len, DV), DV)
	local s = decodeVector(dkPke, 0, 12)

	vecNtt(u)
	local w = polySub(v, invNtt(dotNtt(s, u)))
	return byteEncode(compressPoly(w, 1), 1)
end

-- === ML-KEM ==============================================================

-- Constant-time buffer equality (length is public).
local function bufEqual(a: buffer, b: buffer): boolean
	if buffer.len(a) ~= buffer.len(b) then
		return false
	end
	local diff = 0
	for i = 0, buffer.len(a) - 1 do
		diff = bit32.bor(diff, bit32.bxor(buffer.readu8(a, i), buffer.readu8(b, i)))
	end
	return diff == 0
end

-- ML-KEM.KeyGen_internal (FIPS 203 Algorithm 16): deterministic from d, z.
function mlkem.keygenDerand(d: buffer, z: buffer): (buffer, buffer)
	assert(buffer.len(d) == 32 and buffer.len(z) == 32, "d and z must be 32 bytes")
	local ekPke, dkPke = kpkeKeygen(d)
	local ek = ekPke
	local dk = concat({ dkPke, ek, H(ek), z })
	return ek, dk
end

-- ML-KEM.Encaps_internal (FIPS 203 Algorithm 17): deterministic from m.
function mlkem.encapsDerand(ek: buffer, m: buffer): (buffer, buffer)
	assert(buffer.len(ek) == mlkem.EK_SIZE, "ek has wrong length")
	assert(buffer.len(m) == 32, "m must be 32 bytes")
	local sharedKey, r = G(concat({ m, H(ek) }))
	local c = kpkeEncrypt(ek, m, r)
	return sharedKey, c
end

-- ML-KEM.Decaps_internal (FIPS 203 Algorithm 18), with implicit rejection.
function mlkem.decaps(dk: buffer, c: buffer): buffer
	assert(buffer.len(dk) == mlkem.DK_SIZE, "dk has wrong length")
	assert(buffer.len(c) == mlkem.C_SIZE, "c has wrong length")

	local dkPke = buffer.create(384 * K)
	buffer.copy(dkPke, 0, dk, 0, 384 * K)
	local ek = buffer.create(mlkem.EK_SIZE)
	buffer.copy(ek, 0, dk, 384 * K, mlkem.EK_SIZE)
	local h = buffer.create(32)
	buffer.copy(h, 0, dk, 384 * K + mlkem.EK_SIZE, 32)
	local z = buffer.create(32)
	buffer.copy(z, 0, dk, 384 * K + mlkem.EK_SIZE + 32, 32)

	local mPrime = kpkeDecrypt(dkPke, c)
	local kPrime, rPrime = G(concat({ mPrime, h }))
	local kBar = J(concat({ z, c }))
	local cPrime = kpkeEncrypt(ek, mPrime, rPrime)

	-- Implicit rejection: on ciphertext mismatch return the z-derived key.
	if bufEqual(c, cPrime) then
		return kPrime
	else
		return kBar
	end
end

-- Randomness source: injected so both a real RNG and tests can drive it.
-- Defaults to nil; callers must set mlkem.randomBytes for the non-derand API.
mlkem.randomBytes = (nil :: any) :: ((n: number) -> buffer)?

local function randBytes(n: number): buffer
	local rng = mlkem.randomBytes
	assert(rng ~= nil, "mlkem.randomBytes must be set to use the randomized API")
	return rng(n)
end

-- Randomized ML-KEM.KeyGen (FIPS 203 Algorithm 19).
function mlkem.keygen(): (buffer, buffer)
	return mlkem.keygenDerand(randBytes(32), randBytes(32))
end

-- Randomized ML-KEM.Encaps (FIPS 203 Algorithm 20).
function mlkem.encaps(ek: buffer): (buffer, buffer)
	return mlkem.encapsDerand(ek, randBytes(32))
end

return mlkem
