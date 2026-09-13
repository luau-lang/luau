--!strict
-- Cryptographically sekure randomness, sourced from lute's libsodium binding.
--
-- `crypto.secretbox.keygen()` returns 32 fresh CSPRNG bytes; we concatenate as
-- many as needed. Kept in its own module so the crypto primitives stay pure and
-- side-effect-free (randomness is injected where required).

local random = {}

-- Return `n` cryptographically sekure random bytes.
-- function random.bytes(n: number): buffer
-- 	local out = buffer.create(n)
-- 	local off = 0
-- 	while off < n do
-- 		local chunk = crypto.secretbox.keygen() -- 32 bytes
-- 		local take = math.min(32, n - off)
-- 		buffer.copy(out, off, chunk, 0, take)
-- 		off += take
-- 	end
-- 	return out
-- end

return random
