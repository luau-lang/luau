--!strict
-- ChaCha20-Poly1305 AEAD (RFC 8439 Section 2.8).
--
-- This module composes the separate `chacha20` and `poly1305` primitives into
-- an authenticated cipher. It does not reimplement either one.

local chacha20 = require("./chacha20")
local poly1305 = require("./poly1305")

local aead = {}

aead.KEY_SIZE = 32
aead.NONCE_SIZE = 12
aead.TAG_SIZE = 16

-- Derive the Poly1305 one-time key: the first 32 bytes of the ChaCha20
-- keystream at block counter 0.
local function polyKey(key: buffer, nonce: buffer): buffer
	local ksBlock = chacha20.block(key, 0, nonce)
	local otk = buffer.create(32)
	buffer.copy(otk, 0, ksBlock, 0, 32)
	return otk
end

-- Number of zero bytes needed to pad `len` up to a 16-byte boundary.
local function pad16(len: number): number
	local rem = len % 16
	return if rem == 0 then 0 else 16 - rem
end

-- Write a 64-bit little-endian length. Lengths here are well under 2^32, so the
-- high word is written as zero, but we compute it correctly regardless.
local function writeLe64(buf: buffer, off: number, value: number)
	buffer.writeu32(buf, off, value % 0x100000000)
	buffer.writeu32(buf, off + 4, math.floor(value / 0x100000000))
end

-- Build the Poly1305 MAC input:
--   aad || pad16(aad) || ciphertext || pad16(ciphertext) || le64(|aad|) || le64(|ct|)
local function macData(aad: buffer, ct: buffer): buffer
	local aadLen = buffer.len(aad)
	local ctLen = buffer.len(ct)
	local aadPad = pad16(aadLen)
	local ctPad = pad16(ctLen)

	local total = aadLen + aadPad + ctLen + ctPad + 16
	local m = buffer.create(total)

	local off = 0
	buffer.copy(m, off, aad, 0, aadLen)
	off += aadLen + aadPad -- padding bytes are already zero
	buffer.copy(m, off, ct, 0, ctLen)
	off += ctLen + ctPad
	writeLe64(m, off, aadLen)
	writeLe64(m, off + 8, ctLen)
	return m
end

-- Encrypt and authenticate `plaintext`. Returns the ciphertext and the 16-byte
-- tag separately (both buffers). `aad` may be a zero-length buffer.
function aead.seal(key: buffer, nonce: buffer, plaintext: buffer, aad: buffer): (buffer, buffer)
	assert(buffer.len(key) == 32, "AEAD key must be 32 bytes")
	assert(buffer.len(nonce) == 12, "AEAD nonce must be 12 bytes")

	local otk = polyKey(key, nonce)
	-- Payload uses block counter 1 (counter 0 was spent on the Poly1305 key).
	local ct = chacha20.xor(key, 1, nonce, plaintext)
	local tag = poly1305.mac(otk, macData(aad, ct))
	return ct, tag
end

-- Verify the tag and decrypt. Returns the plaintext on success, or nil if
-- authentication fails (in which case the plaintext is never revealed).
function aead.open(key: buffer, nonce: buffer, ct: buffer, tag: buffer, aad: buffer): buffer?
	assert(buffer.len(key) == 32, "AEAD key must be 32 bytes")
	assert(buffer.len(nonce) == 12, "AEAD nonce must be 12 bytes")

	local otk = polyKey(key, nonce)
	local expected = poly1305.mac(otk, macData(aad, ct))
	if not poly1305.verify(expected, tag) then
		return nil
	end
	return chacha20.xor(key, 1, nonce, ct)
end

return aead
