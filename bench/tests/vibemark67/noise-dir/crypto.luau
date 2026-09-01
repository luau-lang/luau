--!strict
-- Aggregator module: brings the whole stack together under one table.
--
--   require("path/to/src/crypto")   -- returns this table
--     .chacha20   .poly1305  .aead
--     .x25519     .sha256    .keccak   .mlkem768
--     .noise      .random
--
-- The hybrid post-quantum Noise handshake is driven through `.noise`
-- (see noise.luau). Primitives are exposed individually so they can also be
-- used standalone.

local crypto = {
	wordmath = require("./wordmath"),
	chacha20 = require("./chacha20"),
	poly1305 = require("./poly1305"),
	aead = require("./aead"),
	x25519 = require("./x25519"),
	sha256 = require("./sha256"),
	keccak = require("./keccak"),
	mlkem768 = require("./mlkem768"),
	random = require("./random"),
	noise = require("./noise"),
}

return crypto
