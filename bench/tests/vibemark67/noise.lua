--!strict
-- End-to-end stress benchmark for the hybrid post-quantum Noise stack.
--
-- This is a FIXED, DETERMINISTIC workload with no timing code. Run it under an
-- external timer, e.g.
--
--     time lute bench.luau
--
-- Besides being a performance benchmark (part of the vibemark67 suite), it
-- doubles as a correctness stress test of the Luau VM: the entire run is
-- deterministic (a seeded PRNG replaces the CSPRNG), every handshake and
-- transport message is checked for exact round-trip, and a checksum folded over
-- every output byte is compared against a known-good constant. Any deviation --
-- a miscompiled arithmetic op, a bad bit32 result, a GC bug corrupting a buffer
-- -- makes a check fail and the script error()s. On success it prints nothing
-- and exits 0.
--
-- Each of the CONNECTIONS iterations exercises the full key-exchange path
-- (X25519 static + ephemeral keys, ML-KEM-768 keygen/encaps/decaps, SHA3/SHAKE,
-- and the handshake AEAD) and then pushes bidirectional transport traffic
-- through ChaCha20-Poly1305. The transport volume per connection is sized so
-- the symmetric-cipher work takes roughly as long as the key-exchange work
-- (offline calibration: one handshake ~= 87 ms; ChaCha20-Poly1305 ~= 2 MB/s for
-- a full encrypt+decrypt cycle, so ~180 KiB balances a handshake). The default
-- 2-connection run is ~350 ms, split ~50/50 between KEX and symmetric cipher.

local noise = require("./noise-dir/noise")
local random = require("./noise-dir/random")
local mlkem = require("./noise-dir/mlkem768")
local keccak = require("./noise-dir/keccak")

local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

-- ---- workload parameters (tune these to scale the benchmark) -------------
local CONNECTIONS = 2 -- number of simulated open/close cycles (KEX stress)
local MSG_SIZE = 4096 -- bytes per transport message
local MSGS_PER_CONN = 44 -- 44 * 4096 ~= 180 KiB, balances one handshake
-- Expected Adler-32 checksum of the whole deterministic run. Captured from a
-- known-good run whose crypto is validated against external KATs (RFC 8439,
-- RFC 7748, FIPS 202/203). If the VM computes anything incorrectly this will
-- not match. Regenerate ONLY if you intentionally change the workload
-- parameters or the protocol, never to paper over a mismatch.
local EXPECTED_CHECKSUM = 0xdd3c40d5
-- --------------------------------------------------------------------------

-- Deterministic PRNG so the whole benchmark is reproducible. Built from the
-- stack's own SHAKE256 (extra coverage): each request returns
-- SHAKE256(seed || counter). Replaces the real CSPRNG in both the Noise and
-- ML-KEM randomness hooks below.
local prngCounter = 0
local function deterministicBytes(n: number): buffer
	local seed = buffer.create(40)
	buffer.writestring(seed, 0, "vibemark67 deterministic seed!!!") -- 32 bytes
	buffer.writeu32(seed, 32, prngCounter % 0x100000000)
	buffer.writeu32(seed, 36, math.floor(prngCounter / 0x100000000))
	prngCounter += 1
	return keccak.shake256(seed, n)
end

random.bytes = deterministicBytes
mlkem.randomBytes = deterministicBytes

-- Adler-32 checksum: exact in doubles (both accumulators stay < 65521) and
-- sensitive to single-byte changes, so it catches VM miscomputations.
local csA = 1
local csB = 0
local function fold(buf: buffer)
	local a = csA
	local b = csB
	for i = 0, buffer.len(buf) - 1 do
		a = (a + buffer.readu8(buf, i)) % 65521
		b = (b + a) % 65521
	end
	csA = a
	csB = b
end
local function checksum(): number
	return csB * 65536 + csA
end

-- Byte-exact buffer equality.
local function equal(a: buffer, b: buffer): boolean
	if buffer.len(a) ~= buffer.len(b) then
		return false
	end
	for i = 0, buffer.len(a) - 1 do
		if buffer.readu8(a, i) ~= buffer.readu8(b, i) then
			return false
		end
	end
	return true
end

-- Build a message whose contents depend on a seed, so we move real
-- (non-constant) data and can validate round-trips.
local function makeMessage(size: number, seed: number): buffer
	local b = buffer.create(size)
	local x = seed % 256
	for i = 0, size - 1 do
		buffer.writeu8(b, i, x)
		x = (x * 33 + 7 + i) % 256
	end
	return b
end

local function check(cond: boolean, what: string)
	if not cond then
		error("bench correctness failure: " .. what)
	end
end

-- Run one full connection: fresh identities, three-message handshake, then a
-- bidirectional transport exchange. Everything is folded into the checksum and
-- round-trips are asserted.
local function runConnection(connIndex: number)
	local aliceStatic = noise.generateStaticKeyPair()
	local bobStatic = noise.generateStaticKeyPair()

	local alice = noise.newInitiator(aliceStatic)
	local bob = noise.newResponder(bobStatic)

	-- Handshake, carrying small payloads (exercises the handshake AEAD).
	local p1 = makeMessage(24, connIndex + 1)
	local p2 = makeMessage(24, connIndex + 2)
	local p3 = makeMessage(24, connIndex + 3)

	local m1 = noise.writeMessage(alice, p1)
	local r1 = noise.readMessage(bob, m1)
	local m2 = noise.writeMessage(bob, p2)
	local r2 = noise.readMessage(alice, m2)
	local m3 = noise.writeMessage(alice, p3)
	local r3 = noise.readMessage(bob, m3)

	check(equal(r1, p1), "handshake payload 1")
	check(equal(r2, p2), "handshake payload 2")
	check(equal(r3, p3), "handshake payload 3")
	check(noise.handshakeFinished(alice) and noise.handshakeFinished(bob), "handshake incomplete")
	check(equal(noise.handshakeHash(alice), noise.handshakeHash(bob)), "handshake hash disagreement")
	check(equal((bob :: any).rs, aliceStatic.pub), "responder learned wrong initiator static")
	check(equal((alice :: any).rs, bobStatic.pub), "initiator learned wrong responder static")

	-- Fold the on-the-wire handshake bytes and the channel-binding hash.
	fold(m1)
	fold(m2)
	fold(m3)
	fold(noise.handshakeHash(alice))

	local aliceSend = (alice :: any).sendCS
	local aliceRecv = (alice :: any).recvCS
	local bobSend = (bob :: any).sendCS
	local bobRecv = (bob :: any).recvCS

	-- Bidirectional transport: alternate direction so both CipherStates and
	-- nonce counters are exercised.
	for i = 0, MSGS_PER_CONN - 1 do
		local plaintext = makeMessage(MSG_SIZE, connIndex * 131 + i)
		local ct: buffer
		local pt: buffer
		if i % 2 == 0 then
			ct = noise.encrypt(aliceSend, plaintext)
			pt = noise.decrypt(bobRecv, ct)
		else
			ct = noise.encrypt(bobSend, plaintext)
			pt = noise.decrypt(aliceRecv, ct)
		end
		check(equal(pt, plaintext), "transport round-trip")
		fold(ct)
		fold(pt)
	end
end

for c = 0, CONNECTIONS - 1 do
	runConnection(c)
end

check(
	checksum() == EXPECTED_CHECKSUM,
	string.format("checksum mismatch: got 0x%08x, expected 0x%08x", checksum(), EXPECTED_CHECKSUM)
)

end

bench.runCode(test, "noise")
