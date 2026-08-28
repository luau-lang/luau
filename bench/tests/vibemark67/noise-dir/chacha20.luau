--!strict
-- ChaCha20 stream cipher (RFC 8439).
--
-- This module implements only the raw ChaCha20 cipher: the block function and
-- keystream/XOR. Message authentication (Poly1305) lives in a separate module.
--
-- Everything operates on Luau `buffer`s. The internal state is a 16-word array
-- of 32-bit unsigned integers held as Lua numbers (exact, since 2^32 < 2^53).

local wordmath = require("./wordmath")

local chacha20 = {}

-- The four little-endian words of the ASCII constant "expand 32-byte k".
local CONST0 = 0x61707865
local CONST1 = 0x3320646e
local CONST2 = 0x79622d32
local CONST3 = 0x6b206574

local TWO32 = wordmath.TWO32
local addmod32 = wordmath.addmod32
local rotl = wordmath.rotl32

-- The ChaCha quarter round, operating in place on four indices of state `s`.
-- Indices are 1-based (Lua arrays); callers translate the RFC's 0-based
-- positions by adding 1.
local function quarterRound(s: { number }, ai: number, bi: number, ci: number, di: number)
	local a, b, c, d = s[ai], s[bi], s[ci], s[di]

	a = addmod32(a, b)
	d = rotl(bit32.bxor(d, a), 16)

	c = addmod32(c, d)
	b = rotl(bit32.bxor(b, c), 12)

	a = addmod32(a, b)
	d = rotl(bit32.bxor(d, a), 8)

	c = addmod32(c, d)
	b = rotl(bit32.bxor(b, c), 7)

	s[ai], s[bi], s[ci], s[di] = a, b, c, d
end

-- One "double round": four column quarter rounds then four diagonal ones.
-- (RFC indices shown in comments; code uses the +1 translation.)
local function doubleRound(s: { number })
	-- Column rounds: (0,4,8,12) (1,5,9,13) (2,6,10,14) (3,7,11,15)
	quarterRound(s, 1, 5, 9, 13)
	quarterRound(s, 2, 6, 10, 14)
	quarterRound(s, 3, 7, 11, 15)
	quarterRound(s, 4, 8, 12, 16)
	-- Diagonal rounds: (0,5,10,15) (1,6,11,12) (2,7,8,13) (3,4,9,14)
	quarterRound(s, 1, 6, 11, 16)
	quarterRound(s, 2, 7, 12, 13)
	quarterRound(s, 3, 8, 9, 14)
	quarterRound(s, 4, 5, 10, 15)
end

-- Build the initial 16-word ChaCha20 state from key/counter/nonce.
-- `key` is 32 bytes, `nonce` is 12 bytes, `counter` is a 32-bit block counter.
local function initialState(key: buffer, counter: number, nonce: buffer): { number }
	assert(buffer.len(key) == 32, "ChaCha20 key must be 32 bytes")
	assert(buffer.len(nonce) == 12, "ChaCha20 nonce must be 12 bytes")

	local s = table.create(16)
	s[1] = CONST0
	s[2] = CONST1
	s[3] = CONST2
	s[4] = CONST3
	-- Key: 8 little-endian words at positions 4..11 (1-based 5..12).
	for i = 0, 7 do
		s[5 + i] = buffer.readu32(key, i * 4)
	end
	-- Block counter at position 12 (1-based 13).
	s[13] = counter % TWO32
	-- Nonce: 3 little-endian words at positions 13..15 (1-based 14..16).
	for i = 0, 2 do
		s[14 + i] = buffer.readu32(nonce, i * 4)
	end
	return s
end

-- Compute one 64-byte keystream block for the given counter, writing it into
-- `out` at byte offset `outOff`.
local function block(key: buffer, counter: number, nonce: buffer, out: buffer, outOff: number)
	local state = initialState(key, counter, nonce)

	-- Working copy that the rounds mutate.
	local work = table.create(16)
	for i = 1, 16 do
		work[i] = state[i]
	end

	-- 20 rounds = 10 double rounds.
	for _ = 1, 10 do
		doubleRound(work)
	end

	-- Add the original input words and serialize little-endian.
	for i = 0, 15 do
		local word = addmod32(work[i + 1], state[i + 1])
		buffer.writeu32(out, outOff + i * 4, word)
	end
end

chacha20.BLOCK_SIZE = 64

-- Expose the block function for callers that need raw keystream (e.g. Poly1305
-- key generation uses ChaCha20 with counter 0).
-- Returns a fresh 64-byte buffer.
function chacha20.block(key: buffer, counter: number, nonce: buffer): buffer
	local out = buffer.create(64)
	block(key, counter, nonce, out, 0)
	return out
end

-- Encrypt (or decrypt — the operation is symmetric) `input` into a new buffer
-- using ChaCha20 with the given key, nonce and starting block counter.
function chacha20.xor(key: buffer, counter: number, nonce: buffer, input: buffer): buffer
	local n = buffer.len(input)
	local out = buffer.create(n)
	local ks = buffer.create(64)

	local offset = 0
	local blockCounter = counter
	while offset < n do
		block(key, blockCounter, nonce, ks, 0)

		local remaining = n - offset
		local chunk = if remaining < 64 then remaining else 64
		for i = 0, chunk - 1 do
			local x = bit32.bxor(buffer.readu8(input, offset + i), buffer.readu8(ks, i))
			buffer.writeu8(out, offset + i, x)
		end

		offset += chunk
		blockCounter = (blockCounter + 1) % TWO32
	end

	return out
end

return chacha20
