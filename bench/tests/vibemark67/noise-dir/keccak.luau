--!strict
-- Keccak-f[1600] permutation and the SHA-3 / SHAKE functions (FIPS 202).
--
-- These are the hash and XOF primitives ML-KEM depends on. Luau's `bit32`
-- only handles 32-bit words, so each 64-bit Keccak lane is stored as a pair of
-- 32-bit halves: `lo` (bits 0..31) and `hi` (bits 32..63). The state is 25
-- lanes, held as two parallel arrays `lo[1..25]` / `hi[1..25]`, with the lane
-- at coordinates (x, y) stored at index (x + 5*y) + 1.

local keccak = {}

-- Rotation offsets for the ρ step, flattened by lane index (x + 5*y).
local RHO = {
	[0] = 0, 1, 62, 28, 27,
	36, 44, 6, 55, 20,
	3, 10, 43, 25, 39,
	41, 45, 15, 21, 8,
	18, 2, 61, 56, 14,
}

-- Round constants for the ι step, split into low and high 32-bit halves.
local RC_LO = {
	0x00000001, 0x00008082, 0x0000808a, 0x80008000, 0x0000808b, 0x80000001,
	0x80008081, 0x00008009, 0x0000008a, 0x00000088, 0x80008009, 0x8000000a,
	0x8000808b, 0x0000008b, 0x00008089, 0x00008003, 0x00008002, 0x00000080,
	0x0000800a, 0x8000000a, 0x80008081, 0x00008080, 0x80000001, 0x80008008,
}
local RC_HI = {
	0x00000000, 0x00000000, 0x80000000, 0x80000000, 0x00000000, 0x00000000,
	0x80000000, 0x80000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
	0x00000000, 0x80000000, 0x80000000, 0x80000000, 0x80000000, 0x80000000,
	0x00000000, 0x80000000, 0x80000000, 0x80000000, 0x00000000, 0x80000000,
}

-- Rotate a 64-bit lane (lo, hi) left by `r` bits (0..63). Returns (lo, hi).
local function rotl64(lo: number, hi: number, r: number): (number, number)
	if r == 0 then
		return lo, hi
	elseif r == 32 then
		return hi, lo
	elseif r < 32 then
		local nlo = bit32.bor(bit32.lshift(lo, r), bit32.rshift(hi, 32 - r))
		local nhi = bit32.bor(bit32.lshift(hi, r), bit32.rshift(lo, 32 - r))
		return nlo, nhi
	else
		local s = r - 32
		local nlo = bit32.bor(bit32.lshift(hi, s), bit32.rshift(lo, 32 - s))
		local nhi = bit32.bor(bit32.lshift(lo, s), bit32.rshift(hi, 32 - s))
		return nlo, nhi
	end
end

-- The Keccak-f[1600] permutation: 24 rounds of θ, ρ, π, χ, ι, in place.
local function keccakF(lo: { number }, hi: { number })
	local Clo = table.create(5)
	local Chi = table.create(5)
	local Blo = table.create(25)
	local Bhi = table.create(25)

	for round = 1, 24 do
		-- θ: column parities.
		for x = 0, 4 do
			local l = lo[x + 1]
			local h = hi[x + 1]
			for y = 1, 4 do
				local idx = x + 5 * y + 1
				l = bit32.bxor(l, lo[idx])
				h = bit32.bxor(h, hi[idx])
			end
			Clo[x + 1] = l
			Chi[x + 1] = h
		end
		for x = 0, 4 do
			local xm1 = (x + 4) % 5
			local xp1 = (x + 1) % 5
			local rlo, rhi = rotl64(Clo[xp1 + 1], Chi[xp1 + 1], 1)
			local Dlo = bit32.bxor(Clo[xm1 + 1], rlo)
			local Dhi = bit32.bxor(Chi[xm1 + 1], rhi)
			for y = 0, 4 do
				local idx = x + 5 * y + 1
				lo[idx] = bit32.bxor(lo[idx], Dlo)
				hi[idx] = bit32.bxor(hi[idx], Dhi)
			end
		end

		-- ρ (rotate) and π (permude) combined, into B.
		for x = 0, 4 do
			for y = 0, 4 do
				local idx = x + 5 * y
				local rl, rh = rotl64(lo[idx + 1], hi[idx + 1], RHO[idx])
				local nidx = y + 5 * ((2 * x + 3 * y) % 5)
				Blo[nidx + 1] = rl
				Bhi[nidx + 1] = rh
			end
		end

		-- χ: nonlinear mixing along rows.
		for y = 0, 4 do
			for x = 0, 4 do
				local idx = x + 5 * y
				local i0 = idx + 1
				local i1 = (x + 1) % 5 + 5 * y + 1
				local i2 = (x + 2) % 5 + 5 * y + 1
				lo[i0] = bit32.bxor(Blo[i0], bit32.band(bit32.bnot(Blo[i1]), Blo[i2]))
				hi[i0] = bit32.bxor(Bhi[i0], bit32.band(bit32.bnot(Bhi[i1]), Bhi[i2]))
			end
		end

		-- ι: add round constant to lane (0,0).
		lo[1] = bit32.bxor(lo[1], RC_LO[round])
		hi[1] = bit32.bxor(hi[1], RC_HI[round])
	end
end

-- Extract byte `k` (0..7) of a 64-bit lane held as (lo, hi).
local function laneByte(lo: number, hi: number, k: number): number
	if k < 4 then
		return bit32.band(bit32.rshift(lo, k * 8), 0xff)
	else
		return bit32.band(bit32.rshift(hi, (k - 4) * 8), 0xff)
	end
end

-- Sponge construction: absorb `msg` at the given rate (bytes) with domain
-- separation `suffix`, then squeeze `outLen` bytes.
local function sponge(rate: number, suffix: number, msg: buffer, outLen: number): buffer
	local rateLanes = rate // 8
	local lo = table.create(25)
	local hi = table.create(25)
	for i = 1, 25 do
		lo[i] = 0
		hi[i] = 0
	end

	local n = buffer.len(msg)

	-- Absorb full rate-sized blocks directly from the message.
	local off = 0
	while n - off >= rate do
		for i = 0, rateLanes - 1 do
			local o = off + i * 8
			lo[i + 1] = bit32.bxor(lo[i + 1], buffer.readu32(msg, o))
			hi[i + 1] = bit32.bxor(hi[i + 1], buffer.readu32(msg, o + 4))
		end
		keccakF(lo, hi)
		off += rate
	end

	-- Final block: remaining bytes, then pad10*1 with the domain suffix.
	local rem = n - off
	local block = buffer.create(rate)
	buffer.copy(block, 0, msg, off, rem)
	buffer.writeu8(block, rem, bit32.bxor(buffer.readu8(block, rem), suffix))
	buffer.writeu8(block, rate - 1, bit32.bxor(buffer.readu8(block, rate - 1), 0x80))
	for i = 0, rateLanes - 1 do
		local o = i * 8
		lo[i + 1] = bit32.bxor(lo[i + 1], buffer.readu32(block, o))
		hi[i + 1] = bit32.bxor(hi[i + 1], buffer.readu32(block, o + 4))
	end
	keccakF(lo, hi)

	-- Squeeze.
	local out = buffer.create(outLen)
	local produced = 0
	while produced < outLen do
		for i = 1, rateLanes do
			for k = 0, 7 do
				if produced < outLen then
					buffer.writeu8(out, produced, laneByte(lo[i], hi[i], k))
					produced += 1
				end
			end
		end
		if produced < outLen then
			keccakF(lo, hi)
		end
	end
	return out
end

keccak.keccakF = keccakF

-- Fixed-output SHA-3 hashes (domain suffix 0x06).
function keccak.sha3_256(msg: buffer): buffer
	return sponge(136, 0x06, msg, 32)
end

function keccak.sha3_512(msg: buffer): buffer
	return sponge(72, 0x06, msg, 64)
end

-- Extendable-output functions (domain suffix 0x1f).
function keccak.shake128(msg: buffer, outLen: number): buffer
	return sponge(168, 0x1f, msg, outLen)
end

function keccak.shake256(msg: buffer, outLen: number): buffer
	return sponge(136, 0x1f, msg, outLen)
end

return keccak
