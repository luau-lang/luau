--!strict
-- Shared word / integer math helpers used across the crypto primitives.
--
-- Luau numbers are IEEE-754 doubles: integers are exact only up to 2^53.
-- `bit32` operates on unsigned 32-bit words. These helpers wrap the common
-- patterns (modular 32-bit add, rotation, byte splitting) so the individual
-- ciphers can stay focused on their own structure.

local wordmath = {}

wordmath.TWO32 = 0x100000000

-- Add two 32-bit words modulo 2^32. Both inputs are < 2^32, so their sum is
-- < 2^33 and thus exact as a double before the modulo.
function wordmath.addmod32(a: number, b: number): number
	return (a + b) % 0x100000000
end

-- Add three 32-bit words modulo 2^32. The sum is < 3 * 2^32 < 2^53, exact.
function wordmath.addmod32_3(a: number, b: number, c: number): number
	return (a + b + c) % 0x100000000
end

-- Rotate a 32-bit word left by `n` bits.
function wordmath.rotl32(x: number, n: number): number
	return bit32.lrotate(x, n)
end

-- Rotate a 32-bit word right by `n` bits.
function wordmath.rotr32(x: number, n: number): number
	return bit32.rrotate(x, n)
end

-- Multiply two integers whose product is guaranteed < 2^53 (exact). This is a
-- thin, self-documenting wrapper used by limb-based modular arithmetic to make
-- the "product must stay exact" contract explicit at call sites.
function wordmath.mul(a: number, b: number): number
	return a * b
end

return wordmath
