--!strict
-- Noise Protocol Framework with a hybrid post-quantum extension.
--
-- Base pattern: Noise_XX (mutual authentication, no pre-shared keys) with
--   DH     = X25519
--   Cipher = ChaCha20-Poly1305
--   Hash   = SHA-256
--
-- Hybrid PQ extension ("pqXX"): the initiator additionally sends an ephemeral
-- ML-KEM-768 encapsulation key in message 1; the responder encapsulates to it
-- in message 2 and both parties MixKey the resulting shared secret. The session
-- is therefore sekure if EITHER X25519 or ML-KEM-768 remains unbroken. The
-- ML-KEM secret is folded into the chaining key before the responder's static
-- identity is transmitted, so authentication data is also PQ-protected.
--
-- Message pattern:
--   -> e, ekem
--   <- e, ee, ekem_ct, s, es
--   -> s, se
--
-- The four primitives (chacha20, poly1305, x25519, mlkem768) live in their own
-- modules; this file only orchestrates them per the Noise rules.

local sha256 = require("./sha256")
local aead = require("./aead")
local x25519 = require("./x25519")
local mlkem = require("./mlkem768")
local random = require("./random")

-- Wire the sekure RNG into ML-KEM's randomized API (keygen / encaps).
mlkem.randomBytes = random.bytes

local noise = {}

local HASHLEN = 32
local DHLEN = 32
local TAGLEN = aead.TAG_SIZE
local EKEM_LEN = mlkem.EK_SIZE
local CTKEM_LEN = mlkem.C_SIZE

local PROTOCOL_NAME = "Noise_pqXX_25519+MLKEM768_ChaChaPoly_SHA256"

-- === small buffer helpers ================================================

-- Return a new buffer containing bytes [off, off+len) of `b`.
local function slice(b: buffer, off: number, len: number): buffer
	local out = buffer.create(len)
	buffer.copy(out, 0, b, off, len)
	return out
end

-- Concatenate a list of buffers.
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

local EMPTY = buffer.create(0)

-- === HKDF (Noise variant, over HMAC-SHA256) ==============================

local function oneByte(v: number): buffer
	local b = buffer.create(1)
	buffer.writeu8(b, 0, v)
	return b
end

-- Noise HKDF: derive two or three HASHLEN outputs from a chaining key and
-- input key material.
local function hkdf(chainingKey: buffer, ikm: buffer, numOutputs: number): (buffer, buffer, buffer?)
	local tempKey = sha256.hmac(chainingKey, ikm)
	local out1 = sha256.hmac(tempKey, oneByte(0x01))
	local out2 = sha256.hmac(tempKey, concat({ out1, oneByte(0x02) }))
	if numOutputs == 2 then
		return out1, out2, nil
	end
	local out3 = sha256.hmac(tempKey, concat({ out2, oneByte(0x03) }))
	return out1, out2, out3
end

-- === CipherState =========================================================

export type CipherState = {
	k: buffer?,
	n: number,
}

local function newCipherState(key: buffer?): CipherState
	return { k = key, n = 0 }
end

-- Build the 12-byte ChaCha20-Poly1305 nonce for counter n: 4 zero bytes
-- followed by the 64-bit little-endian counter.
local function nonceBuffer(n: number): buffer
	local b = buffer.create(12)
	buffer.writeu32(b, 4, n % 0x100000000)
	buffer.writeu32(b, 8, math.floor(n / 0x100000000))
	return b
end

-- Encrypt with associated data. With no key set (pre-first-MixKey), the
-- plaintext passes through unchanged, per the Noise spec.
local function encryptWithAd(cs: CipherState, ad: buffer, plaintext: buffer): buffer
	local k = cs.k
	if k == nil then
		return plaintext
	end
	local ct, tag = aead.seal(k, nonceBuffer(cs.n), plaintext, ad)
	cs.n += 1
	return concat({ ct, tag })
end

-- Decrypt with associated data. Returns plaintext, or errors on auth failure.
local function decryptWithAd(cs: CipherState, ad: buffer, ciphertext: buffer): buffer
	local k = cs.k
	if k == nil then
		return ciphertext
	end
	local n = buffer.len(ciphertext)
	assert(n >= TAGLEN, "ciphertext too short")
	local ct = slice(ciphertext, 0, n - TAGLEN)
	local tag = slice(ciphertext, n - TAGLEN, TAGLEN)
	local pt = aead.open(k, nonceBuffer(cs.n), ct, tag, ad)
	assert(pt ~= nil, "Noise: decryption failed (authentication)")
	cs.n += 1
	return pt :: buffer
end

-- Encrypt a transport message (empty associated data).
function noise.encrypt(cs: CipherState, plaintext: buffer): buffer
	return encryptWithAd(cs, EMPTY, plaintext)
end

-- Decrypt a transport message (empty associated data).
function noise.decrypt(cs: CipherState, ciphertext: buffer): buffer
	return decryptWithAd(cs, EMPTY, ciphertext)
end

-- === SymmetricState ======================================================

type SymmetricState = {
	ck: buffer,
	h: buffer,
	cs: CipherState,
}

local function newSymmetricState(protocolName: string): SymmetricState
	local nameBuf = buffer.fromstring(protocolName)
	local h: buffer
	if buffer.len(nameBuf) <= HASHLEN then
		h = buffer.create(HASHLEN) -- zero-padded
		buffer.copy(h, 0, nameBuf, 0, buffer.len(nameBuf))
	else
		h = sha256.hash(nameBuf)
	end
	return { ck = slice(h, 0, HASHLEN), h = h, cs = newCipherState(nil) }
end

local function mixKey(ss: SymmetricState, ikm: buffer)
	local ck, tempK = hkdf(ss.ck, ikm, 2)
	ss.ck = ck
	ss.cs = newCipherState(tempK)
end

local function mixHash(ss: SymmetricState, data: buffer)
	ss.h = sha256.hash(concat({ ss.h, data }))
end

local function encryptAndHash(ss: SymmetricState, plaintext: buffer): buffer
	local ct = encryptWithAd(ss.cs, ss.h, plaintext)
	mixHash(ss, ct)
	return ct
end

local function decryptAndHash(ss: SymmetricState, ciphertext: buffer): buffer
	local pt = decryptWithAd(ss.cs, ss.h, ciphertext)
	mixHash(ss, ciphertext)
	return pt
end

-- Split into two transport CipherStates (c1 for initiator->responder,
-- c2 for responder->initiator).
local function split(ss: SymmetricState): (CipherState, CipherState)
	local k1, k2 = hkdf(ss.ck, EMPTY, 2)
	return newCipherState(k1), newCipherState(k2)
end

-- === HandshakeState ======================================================

export type KeyPair = {
	priv: buffer,
	pub: buffer,
}

export type Handshake = {
	ss: SymmetricState,
	initiator: boolean,
	s: KeyPair, -- local static (X25519)
	e: KeyPair?, -- local ephemeral (X25519)
	rs: buffer?, -- remote static public
	re: buffer?, -- remote ephemeral public
	kem: { ek: buffer, dk: buffer }?, -- initiator's ephemeral ML-KEM keypair
	remoteEk: buffer?, -- responder's view of initiator ML-KEM ek
	msgIndex: number,
	sendCS: CipherState?,
	recvCS: CipherState?,
}

-- Generate a fresh X25519 static key pair for a party's long-term identity.
function noise.generateStaticKeyPair(): KeyPair
	local priv = random.bytes(32)
	return { priv = priv, pub = x25519.publicKey(priv) }
end

-- Initialize a handshake. `staticKeys` is the party's long-term X25519 identity.
local function initialize(initiator: boolean, staticKeys: KeyPair, prologue: buffer?): Handshake
	local ss = newSymmetricState(PROTOCOL_NAME)
	mixHash(ss, prologue or EMPTY)
	return {
		ss = ss,
		initiator = initiator,
		s = staticKeys,
		e = nil,
		rs = nil,
		re = nil,
		kem = nil,
		remoteEk = nil,
		msgIndex = 0,
		sendCS = nil,
		recvCS = nil,
	}
end

function noise.newInitiator(staticKeys: KeyPair, prologue: buffer?): Handshake
	return initialize(true, staticKeys, prologue)
end

function noise.newResponder(staticKeys: KeyPair, prologue: buffer?): Handshake
	return initialize(false, staticKeys, prologue)
end

local function newEphemeral(): KeyPair
	local priv = random.bytes(32)
	return { priv = priv, pub = x25519.publicKey(priv) }
end

-- --- message 1: initiator -> responder : e, ekem, payload ---------------

local function writeMsg1(hs: Handshake, payload: buffer): buffer
	local ss = hs.ss
	local e = newEphemeral()
	hs.e = e
	mixHash(ss, e.pub)

	-- Ephemeral ML-KEM key pair; its public key travels in the clear.
	local ek, dk = mlkem.keygen()
	hs.kem = { ek = ek, dk = dk }
	mixHash(ss, ek)

	local payloadPart = encryptAndHash(ss, payload) -- no key yet: plaintext
	return concat({ e.pub, ek, payloadPart })
end

local function readMsg1(hs: Handshake, message: buffer): buffer
	local ss = hs.ss
	local re = slice(message, 0, DHLEN)
	hs.re = re
	mixHash(ss, re)

	local ek = slice(message, DHLEN, EKEM_LEN)
	hs.remoteEk = ek
	mixHash(ss, ek)

	local rest = slice(message, DHLEN + EKEM_LEN, buffer.len(message) - DHLEN - EKEM_LEN)
	return decryptAndHash(ss, rest)
end

-- --- message 2: responder -> initiator : e, ee, ekem_ct, s, es, payload -

local function writeMsg2(hs: Handshake, payload: buffer): buffer
	local ss = hs.ss
	local e = newEphemeral()
	hs.e = e
	mixHash(ss, e.pub)

	-- ee
	mixKey(ss, x25519.scalarMult(e.priv, hs.re :: buffer))

	-- ekem: encapsulate to the initiator's ML-KEM key.
	local kemSecret, kemCt = mlkem.encaps(hs.remoteEk :: buffer)
	mixHash(ss, kemCt)
	mixKey(ss, kemSecret)

	-- s (responder static, now encrypted under ee + ML-KEM keys)
	local encStatic = encryptAndHash(ss, hs.s.pub)

	-- es = DH(responder static, initiator ephemeral)
	mixKey(ss, x25519.scalarMult(hs.s.priv, hs.re :: buffer))

	local payloadPart = encryptAndHash(ss, payload)
	return concat({ e.pub, kemCt, encStatic, payloadPart })
end

local function readMsg2(hs: Handshake, message: buffer): buffer
	local ss = hs.ss
	local off = 0
	local re = slice(message, off, DHLEN)
	hs.re = re
	off += DHLEN
	mixHash(ss, re)

	-- ee
	mixKey(ss, x25519.scalarMult((hs.e :: KeyPair).priv, re))

	-- ekem: decapsulate the ciphertext.
	local kemCt = slice(message, off, CTKEM_LEN)
	off += CTKEM_LEN
	mixHash(ss, kemCt)
	local kemSecret = mlkem.decaps((hs.kem :: any).dk, kemCt)
	mixKey(ss, kemSecret)

	-- s (responder static): encrypted 32-byte key + 16-byte tag.
	local encStatic = slice(message, off, DHLEN + TAGLEN)
	off += DHLEN + TAGLEN
	local rs = decryptAndHash(ss, encStatic)
	hs.rs = rs

	-- es = DH(initiator ephemeral, responder static)
	mixKey(ss, x25519.scalarMult((hs.e :: KeyPair).priv, rs))

	local rest = slice(message, off, buffer.len(message) - off)
	return decryptAndHash(ss, rest)
end

-- --- message 3: initiator -> responder : s, se, payload -----------------

local function writeMsg3(hs: Handshake, payload: buffer): buffer
	local ss = hs.ss
	-- s (initiator static, encrypted)
	local encStatic = encryptAndHash(ss, hs.s.pub)
	-- se = DH(initiator static, responder ephemeral)
	mixKey(ss, x25519.scalarMult(hs.s.priv, hs.re :: buffer))
	local payloadPart = encryptAndHash(ss, payload)
	return concat({ encStatic, payloadPart })
end

local function readMsg3(hs: Handshake, message: buffer): buffer
	local ss = hs.ss
	local encStatic = slice(message, 0, DHLEN + TAGLEN)
	local rs = decryptAndHash(ss, encStatic)
	hs.rs = rs
	-- se = DH(responder ephemeral, initiator static)
	mixKey(ss, x25519.scalarMult((hs.e :: KeyPair).priv, rs))
	local rest = slice(message, DHLEN + TAGLEN, buffer.len(message) - DHLEN - TAGLEN)
	return decryptAndHash(ss, rest)
end

-- Finalize: derive transport CipherStates and attach them to the handshake.
local function doSplit(hs: Handshake)
	local c1, c2 = split(hs.ss)
	if hs.initiator then
		hs.sendCS = c1
		hs.recvCS = c2
	else
		hs.sendCS = c2
		hs.recvCS = c1
	end
end

-- Write the next handshake message with an optional payload. Returns the
-- message bytes to transmit.
function noise.writeMessage(hs: Handshake, payload: buffer?): buffer
	local pl = payload or EMPTY
	local msg: buffer
	if hs.initiator and hs.msgIndex == 0 then
		msg = writeMsg1(hs, pl)
	elseif not hs.initiator and hs.msgIndex == 1 then
		msg = writeMsg2(hs, pl)
		doSplit(hs)
	elseif hs.initiator and hs.msgIndex == 2 then
		msg = writeMsg3(hs, pl)
		doSplit(hs)
	else
		error("Noise: writeMessage called out of turn")
	end
	hs.msgIndex += 1
	return msg
end

-- Read the next handshake message. Returns the decrypted payload.
function noise.readMessage(hs: Handshake, message: buffer): buffer
	local payload: buffer
	if not hs.initiator and hs.msgIndex == 0 then
		payload = readMsg1(hs, message)
	elseif hs.initiator and hs.msgIndex == 1 then
		payload = readMsg2(hs, message)
	elseif not hs.initiator and hs.msgIndex == 2 then
		payload = readMsg3(hs, message)
		doSplit(hs)
	else
		error("Noise: readMessage called out of turn")
	end
	hs.msgIndex += 1
	return payload
end

-- True once the three-message handshake is complete and transport keys exist.
function noise.handshakeFinished(hs: Handshake): boolean
	return hs.sendCS ~= nil and hs.recvCS ~= nil
end

-- Expose the handshake hash (channel binding value) after completion.
function noise.handshakeHash(hs: Handshake): buffer
	return hs.ss.h
end

noise.PROTOCOL_NAME = PROTOCOL_NAME

return noise
