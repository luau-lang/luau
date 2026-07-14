-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("testing integers")

local function noinline(x: integer): integer local s, r = pcall(function(y) return y end, x) return r end

-- Integers have the 'integer' type
assert(type(123i)=="integer")

-- Integer constants
assert(123i == integer.add(100i,23i))
assert(0xf_fi == 255i)
assert(1_000i == 1000i)
assert(0xABABi == 43947i)
assert(0b1000_1000i == 136i)
assert(0xABCDEFi == 11259375i)
assert(0xfedcbai == 16702650i)
assert(-0x8000000000000000i == 0x8000000000000000i)
assert(-0xFFFFFFFFFFFFFFFFi == 1i)

-- Built-in equality operator
assert(76i==76i)
assert(76i~=32i)
assert(rawequal(76i, 76i))
assert(not rawequal(76i, 32i))
assert(not rawequal(76i, 76))

-- integer.create
assert(typeof(integer.create(4711))=="integer")
assert(integer.create(4711) == 4711i)
assert(integer.create(-3) == -3i)
assert(integer.create(1e18) == 1000000000000000000i)
assert(integer.create(-1e18) == -1000000000000000000i)

-- isolate tests which take a VM exit in native code
function testCreateVmExit()
    assert(integer.create(2.5) == nil)
    assert(integer.create(1e30) == nil)
    assert(integer.create(0/0) == nil)
    assert(integer.create(math.huge) == nil)
    assert(integer.create(-math.huge) == nil)
    assert(integer.create(1.0000000005) == nil)
    assert(integer.create(0.9999999995) == nil)
end
testCreateVmExit()

-- integer.fromstring
assert(integer.fromstring("30") == 30i)
assert(integer.fromstring("-4711") == -4711i)
assert(integer.fromstring("2.5") == nil)
assert(integer.fromstring("blah") == nil)
assert(integer.fromstring("  47  ") == 47i)
assert(integer.fromstring("+2") == 2i)
assert(integer.fromstring("-2") == -2i)
assert(integer.fromstring("0x123") == 291i)
assert(integer.fromstring("0X200") == 512i)
assert(integer.fromstring("0xFFFFFFFFFF") == 0xFFFFFFFFFFi)
assert(integer.fromstring("0x100000000") == 4294967296i)
assert(integer.fromstring("0x1000000000000") == 0x1000000000000i)
assert(integer.fromstring("0x20000000000001") == 0x20000000000001i)
assert(integer.fromstring("123", 4) == 27i)
assert(integer.fromstring("ZZZ", 36) == 46655i)
assert(not pcall(function() integer.fromstring("123", 1) end))
assert(not pcall(function() integer.fromstring("123", 37) end))
assert(not pcall(function() integer.fromstring("123", 0) end))

-- integer.fromstring with values beyond 32-bit range
assert(integer.fromstring("4294967296") == 4294967296i)
assert(integer.fromstring("1000000000000") == 1000000000000i)
assert(integer.fromstring("-8589934592") == -8589934592i)
assert(integer.fromstring("9223372036854775807") == 0x7FFFFFFFFFFFFFFFi)
assert(integer.fromstring("-9223372036854775808") == integer.sub(-0x7FFFFFFFFFFFFFFFi, 1i))
assert(integer.fromstring("FFFFFFFF", 16) == 4294967295i)
assert(integer.fromstring("100000000", 16) == 4294967296i)
assert(integer.fromstring("0x8000000000000000") == 0x8000000000000000i)
assert(integer.fromstring("0x7FFFFFFFFFFFFFFF") == 0x7FFFFFFFFFFFFFFFi)
assert(integer.fromstring("0xFFFFFFFFFFFFFFFF") == 0xFFFFFFFFFFFFFFFFi)
assert(integer.fromstring("-1", 2) == -1i)
assert(integer.fromstring("-11", 2) == -3i)
assert(integer.fromstring("-1", 10) == -1i)
assert(integer.fromstring("-11", 10) == -11i)
assert(integer.fromstring("-0x1", 10) == -1i)
assert(integer.fromstring("-0x11", 10) == -17i)
assert(integer.fromstring("-1", 16) == -1i)
assert(integer.fromstring("-11", 16) == -17i)
assert(integer.fromstring("8000000000000000", 16) == 0x8000000000000000i)
assert(integer.fromstring("7FFFFFFFFFFFFFFF", 16) == 0x7FFFFFFFFFFFFFFFi)
assert(integer.fromstring("FFFFFFFFFFFFFFFF", 16) == 0xFFFFFFFFFFFFFFFFi)

-- integer.neg
local x = integer.create(3411)
assert(integer.neg(x) == -3411i)
assert(integer.neg(integer.minsigned) == integer.minsigned)
assert(integer.neg(integer.maxsigned) == integer.add(integer.minsigned, 1i))

assert(integer.neg(noinline(3411i)) == -3411i)
assert(integer.neg(noinline(integer.minsigned)) == integer.minsigned)

-- integer.add
local x = integer.create(456)
local y = integer.create(123)
assert(integer.add(x, y) == 579i)
assert(integer.add(integer.maxsigned, 1i) == integer.minsigned)
assert(integer.add(integer.maxsigned, integer.maxsigned) == -2i)

assert(integer.add(noinline(456i), 123i) == 579i)
assert(integer.add(noinline(integer.maxsigned), 1i) == integer.minsigned)

-- integer.sub
local x = integer.create(999)
local y = integer.create(777)
assert(integer.sub(x, y) == 222i)
assert(integer.sub(integer.minsigned, 1i) == integer.maxsigned)
assert(integer.sub(integer.minsigned, integer.maxsigned) == 1i)

assert(integer.sub(noinline(999i), 777i) == 222i)
assert(integer.sub(noinline(integer.minsigned), 1i) == integer.maxsigned)

-- integer.mul
local x = integer.create(7)
local y = integer.create(9)
assert(integer.mul(x, y) == 63i)
assert(integer.mul(integer.maxsigned, 2i) == -2i)
assert(integer.mul(integer.minsigned, -1i) == integer.minsigned)

assert(integer.mul(noinline(7i), 9i) == 63i)
assert(integer.mul(noinline(integer.maxsigned), 2i) == -2i)

-- integer.div (truncated division)
assert(integer.div(32i, 8i) == 4i)
assert(integer.div(-7i, 3i) == -2i)

assert(integer.div(noinline(32i), 8i) == 4i)
assert(integer.div(noinline(-7i), 3i) == -2i)

local success,errmsg = pcall(function() integer.div(5i,0i) end)
assert(not success)
assert(string.find(errmsg,"division by zero"))
local success,errmsg = pcall(function() integer.div(integer.minsigned,-1i) end)
assert(not success)
assert(string.find(errmsg,"integer overflow"))

-- integer.idiv (floored signed division)
assert(integer.idiv(32i, 7i) == 4i)
assert(integer.idiv(-7i, 3i) == -3i)

assert(integer.idiv(noinline(32i), 7i) == 4i)
assert(integer.idiv(noinline(-7i), 3i) == -3i)

local success,errmsg = pcall(function() integer.idiv(5i,0i) end)
assert(not success)
assert(string.find(errmsg,"division by zero"))
local success,errmsg = pcall(function() integer.idiv(integer.minsigned,-1i) end)
assert(not success)
assert(string.find(errmsg,"integer overflow"))

-- integer.udiv (unsigned division)
assert(integer.udiv(32i, 7i) == 4i)

assert(integer.udiv(noinline(32i), 7i) == 4i)

local success,errmsg = pcall(function() integer.udiv(5i,0i) end)
assert(not success)
assert(string.find(errmsg,"division by zero"))

-- integer.urem (unsigned remainder)
assert(integer.urem(34i, 7i) == 6i)

assert(integer.urem(noinline(34i), 7i) == 6i)

local success,errmsg = pcall(function() integer.urem(5i,0i) end)
assert(not success)
assert(string.find(errmsg,"division by zero"))

-- integer.mod (floored modulus)
assert(integer.mod(7i, 3i) == 1i)
assert(integer.mod(-7i, 3i) == 2i)
assert(integer.mod(7i, -3i) == -2i)
assert(integer.mod(-7i, -3i) == -1i)

assert(integer.mod(noinline(7i), 3i) == 1i)
assert(integer.mod(noinline(-7i), 3i) == 2i)
assert(integer.mod(noinline(7i), -3i) == -2i)
assert(integer.mod(noinline(-7i), -3i) == -1i)

local success,errmsg = pcall(function() integer.mod(5i,0i) end)
assert(not success)
assert(string.find(errmsg,"division by zero"))
assert(integer.mod(integer.minsigned,-1i) == 0i)

-- integer.rem
assert(integer.rem(35i, 8i) == 3i)
assert(integer.rem(-7i, 3i) == -1i)

assert(integer.rem(noinline(35i), 8i) == 3i)
assert(integer.rem(noinline(-7i), 3i) == -1i)

local success,errmsg = pcall(function() integer.rem(5i,0i) end)
assert(not success)
assert(string.find(errmsg,"division by zero"))
assert(integer.rem(integer.minsigned,-1i) == 0i)

-- integer.min and integer.max
local x = integer.create(99)
local y = integer.create(5)
assert(integer.min(x, y) == 5i)
assert(integer.min(y, x) == 5i)
assert(integer.max(x, y) == 99i)
assert(integer.max(y, x) == 99i)
assert(integer.min(17i, 12i, 48i, 13i, -2i) == -2i)
assert(integer.max(17i, 12i, 48i, 13i, 94i) == 94i)

assert(integer.min(noinline(99i), 5i) == 5i)
assert(integer.max(noinline(99i), 5i) == 99i)
assert(integer.min(noinline(17i), 12i, 48i, 13i, -2i) == -2i)
assert(integer.max(noinline(17i), 12i, 48i, 13i, 94i) == 94i)

-- integer.clamp
local mi = integer.create(6)
local mx = integer.create(18)
local a1 = integer.create(3)
local a2 = integer.create(11)
local a3 = integer.create(47)
assert(integer.clamp(a1, mi, mx) == 6i)
assert(integer.clamp(a2, mi, mx) == 11i)
assert(integer.clamp(a3, mi, mx) == 18i)

assert(integer.clamp(noinline(3i), 6i, 18i) == 6i)
assert(integer.clamp(noinline(11i), 6i, 18i) == 11i)
assert(integer.clamp(noinline(47i), 6i, 18i) == 18i)

local success,errmsg = pcall(function() integer.clamp(10i, 20i, 5i) end)
assert(not success)
assert(string.find(errmsg, "max must be greater than or equal to min"))

-- Bitwise operations
assert(integer.band(14i, 7i) == 6i)
assert(integer.bor(48i, 24i) == 56i)
assert(integer.bnot(10000i) == -10001i)
assert(integer.bxor(7i, 10i) == 13i)
assert(integer.band(65535i, 255i, 192i) == 192i)
assert(integer.bor(1i, 2i, 4i, 8i, 13i) == 15i)
assert(integer.bxor(255i, 252i, 1i, 12i) == 14i)
assert(integer.btest(65535i, 255i, 192i))
assert(integer.band() == -1i)
assert(integer.bor() == 0i)
assert(integer.bxor() == 0i)
assert(integer.btest() == true)
assert(integer.band(42i) == 42i)
assert(integer.bor(42i) == 42i)
assert(integer.bxor(42i) == 42i)
assert(integer.btest(42i) == true)
assert(integer.btest(0i) == false)

assert(integer.band(noinline(14i), 7i) == 6i)
assert(integer.bor(noinline(48i), 24i) == 56i)
assert(integer.bnot(noinline(10000i)) == -10001i)
assert(integer.bxor(noinline(7i), 10i) == 13i)
assert(integer.band(noinline(65535i), 255i, 192i) == 192i)
assert(integer.bor(noinline(1i), 2i, 4i, 8i, 13i) == 15i)
assert(integer.bxor(noinline(255i), 252i, 1i, 12i) == 14i)
assert(integer.btest(noinline(65535i), 255i, 192i))
assert(integer.btest(noinline(42i)) == true)
assert(integer.btest(noinline(0i)) == false)

-- Comparisons
assert(integer.lt(3i,4i))
assert(not integer.lt(4i,4i))
assert(not integer.lt(5i,4i))
assert(integer.le(3i,4i))
assert(integer.le(4i,4i))
assert(not integer.le(5i,4i))
assert(integer.ult(5i,-3i))
assert(not integer.ult(-5i,6i))
assert(integer.ule(5i,-3i))
assert(integer.ule(-3i,-3i))
assert(integer.gt(4i,3i))
assert(not integer.gt(4i,4i))
assert(not integer.gt(4i,5i))
assert(integer.ge(4i,3i))
assert(integer.ge(4i,4i))
assert(not integer.ge(4i,5i))
assert(integer.ugt(-3i,5i))
assert(not integer.ugt(6i,-5i))
assert(integer.uge(-3i,5i))
assert(integer.uge(-3i,-3i))

assert(integer.lt(noinline(3i), 4i))
assert(not integer.lt(noinline(4i), 4i))
assert(not integer.lt(noinline(5i), 4i))
assert(integer.le(noinline(3i), 4i))
assert(integer.le(noinline(4i), 4i))
assert(not integer.le(noinline(5i), 4i))
assert(integer.ult(noinline(5i), -3i))
assert(not integer.ult(noinline(-5i), 6i))
assert(integer.ule(noinline(5i), -3i))
assert(integer.ule(noinline(-3i), -3i))
assert(integer.gt(noinline(4i), 3i))
assert(not integer.gt(noinline(4i), 4i))
assert(not integer.gt(noinline(4i), 5i))
assert(integer.ge(noinline(4i), 3i))
assert(integer.ge(noinline(4i), 4i))
assert(not integer.ge(noinline(4i), 5i))
assert(integer.ugt(noinline(-3i), 5i))
assert(not integer.ugt(noinline(6i), -5i))
assert(integer.uge(noinline(-3i), 5i))
assert(integer.uge(noinline(-3i), -3i))

-- Shifts
assert(integer.lshift(1i, 8i) == 256i)
assert(integer.lshift(256i, -7i) == 2i)
assert(integer.lshift(1i, 64i) == 0i)
assert(integer.lshift(256i, -64i) == 0i)
assert(integer.rshift(512i, 3i) == 64i)
assert(integer.rshift(512i, -2i) == 2048i)
assert(integer.rshift(256i, 64i) == 0i)
assert(integer.rshift(256i, -64i) == 0i)
assert(integer.arshift(512i, 3i) == 64i)
assert(integer.arshift(512i, -2i) == 2048i)
assert(integer.arshift(256i, 64i) == 0i)
assert(integer.arshift(-256i, 64i) == -1i)
assert(integer.arshift(256i, -64i) == 0i)
assert(integer.arshift(-256i, -64i) == 0i)
assert(integer.arshift(-1i, -3i) == -8i)
assert(integer.arshift(integer.minsigned, -1i) == 0i)

assert(integer.lshift(noinline(1i), 8i) == 256i)
assert(integer.lshift(noinline(256i), -7i) == 2i)
assert(integer.lshift(noinline(1i), 64i) == 0i)
assert(integer.rshift(noinline(512i), 3i) == 64i)
assert(integer.rshift(noinline(512i), -2i) == 2048i)
assert(integer.rshift(noinline(256i), 64i) == 0i)
assert(integer.arshift(noinline(512i), 3i) == 64i)
assert(integer.arshift(noinline(-256i), 64i) == -1i)
assert(integer.arshift(noinline(-1i), -3i) == -8i)

-- Rotations
assert(integer.lrotate(0x6003000000000000i, 4i) == 0x0030000000000006i)
assert(integer.lrotate(0x1000200000000000i, 69i) == 0x0004000000000002i)
assert(integer.lrotate(0x842842842842i, -1i) == 0x421421421421i)
assert(integer.lrotate(1i, 0i) == 1i)
assert(integer.lrotate(1i, 64i) == 1i)
assert(integer.lrotate(1i, -64i) == 1i)
assert(integer.lrotate(1i, integer.minsigned) == 1i)
assert(integer.rrotate(0x8420i, 5i) == 0x421i)
assert(integer.rrotate(0x10i, -3i) == 0x80i)
assert(integer.rrotate(0x10i, -67i) == 0x80i)
assert(integer.rrotate(0x7FFFFFFFFFFFFFFFi, 63i) == -2i)
assert(integer.rrotate(1i, 0i) == 1i)
assert(integer.rrotate(1i, 64i) == 1i)
assert(integer.rrotate(1i, -64i) == 1i)
assert(integer.rrotate(1i, integer.minsigned) == 1i)

assert(integer.lrotate(noinline(0x6003000000000000i), 4i) == 0x0030000000000006i)
assert(integer.lrotate(noinline(0x1000200000000000i), 69i) == 0x0004000000000002i)
assert(integer.lrotate(noinline(0x842842842842i), -1i) == 0x421421421421i)
assert(integer.lrotate(noinline(1i), 0i) == 1i)
assert(integer.lrotate(noinline(1i), 64i) == 1i)
assert(integer.rrotate(noinline(0x8420i), 5i) == 0x421i)
assert(integer.rrotate(noinline(0x10i), -3i) == 0x80i)
assert(integer.rrotate(noinline(0x7FFFFFFFFFFFFFFFi), noinline(63i)) == -2i)
assert(integer.rrotate(noinline(1i), 0i) == 1i)
assert(integer.rrotate(noinline(1i), 64i) == 1i)

-- extract
assert(integer.extract(0xBADBEEFi, 0i, 16i) == 0xBEEFi)
assert(integer.extract(0xBADBEEFi, 16i, 12i) == 0xBADi)
assert(integer.extract(0xBADBEEFi, 3i) == 1i)
assert(integer.extract(0xBADBEEFi, 4i) == 0i)
assert(integer.extract(0xBADBEEFi, 0i, 64i) == 0xBADBEEFi)
assert(integer.extract(0xFFFFFFFFFFFFFFFFi, 0i, 64i) == 0xFFFFFFFFFFFFFFFFi)

assert(integer.extract(noinline(0xBADBEEFi), 0i, 16i) == 0xBEEFi)
assert(integer.extract(noinline(0xBADBEEFi), 16i, 12i) == 0xBADi)
assert(integer.extract(noinline(0xBADBEEFi), 3i) == 1i)
assert(integer.extract(noinline(0xBADBEEFi), 4i) == 0i)
assert(integer.extract(noinline(0xFFFFFFFFFFFFFFFFi), 0i, 64i) == 0xFFFFFFFFFFFFFFFFi)

local success,errmsg = pcall(function() integer.extract(0xBADBEEFi, -1i) end)
assert(not success)
assert(string.find(errmsg, "field cannot be negative"))
local success,errmsg = pcall(function() integer.extract(0xBADBEEFi, 4i, -2i) end)
assert(not success)
assert(string.find(errmsg, "width must be positive"))
local success,errmsg = pcall(function() integer.extract(0xBADBEEFi, 33i, 33i) end)
assert(not success)
assert(string.find(errmsg, "trying to access non%-existent bits"))
local success,errmsg = pcall(function() integer.extract(1i, 64i, 1i) end)
assert(not success)
local success,errmsg = pcall(function() integer.extract(1i, integer.maxsigned, 1i) end)
assert(not success)

-- replace
assert(integer.replace(0xBADBEEFi, 0x500Di, 16i, 16i) == 0x500DBEEFi)
assert(integer.replace(0xFFFFFFFFFFFFi, 0xEEEi, 28i, 12i) == 0xFFEEEFFFFFFFi)
assert(integer.replace(0xFFFFFFFFFFFFi, 0i, 6i) == 0xFFFFFFFFFFBFi)
assert(integer.replace(0xBADBEEFi, 0x123i, 0i, 64i) == 0x123i)

assert(integer.replace(noinline(0xBADBEEFi), 0x500Di, 16i, 16i) == 0x500DBEEFi)
assert(integer.replace(noinline(0xFFFFFFFFFFFFi), 0xEEEi, 28i, 12i) == 0xFFEEEFFFFFFFi)
assert(integer.replace(noinline(0xFFFFFFFFFFFFi), 0i, 6i) == 0xFFFFFFFFFFBFi)
assert(integer.replace(noinline(0xBADBEEFi), 0x123i, 0i, 64i) == 0x123i)

local success,errmsg = pcall(function() integer.replace(1i, 2i, -3i) end)
assert(not success)
assert(string.find(errmsg, "field cannot be negative"))
local success,errmsg = pcall(function() integer.replace(1i, 2i, 3i, -4i) end)
assert(not success)
assert(string.find(errmsg, "width must be positive"))
local success,errmsg = pcall(function() integer.replace(1i, 2i, 40i, 50i) end)
assert(not success)
assert(string.find(errmsg, "trying to access non%-existent bits"))
local success,errmsg = pcall(function() integer.replace(1i, 2i, 64i, 1i) end)
assert(not success)
local success,errmsg = pcall(function() integer.replace(1i, 2i, integer.maxsigned, 1i) end)
assert(not success)

-- btest 
assert(not integer.btest(0xAAAAi, 0x5555i))
assert(integer.btest(12i, 4i))
assert(integer.btest(0x100000000i, 0x100000000i))
assert(integer.btest(0x8000000000000000i, 0x8000000000000000i))

assert(not integer.btest(noinline(0xAAAAi), 0x5555i))
assert(integer.btest(noinline(12i), 4i))
assert(integer.btest(noinline(0x100000000i), 0x100000000i))
assert(integer.btest(noinline(0x8000000000000000i), 0x8000000000000000i))

-- countrz/countlz
assert(integer.countrz(1i) == 0i)
assert(integer.countrz(8i) == 3i)
assert(integer.countrz(0x200000i) == 21i)
assert(integer.countrz(0i) == 64i)
assert(integer.countlz(1i) == 63i)
assert(integer.countlz(8i) == 60i)
assert(integer.countlz(0x200000i) == 42i)
assert(integer.countlz(0i) == 64i)

assert(integer.countrz(noinline(1i)) == 0i)
assert(integer.countrz(noinline(8i)) == 3i)
assert(integer.countrz(noinline(0x200000i)) == 21i)
assert(integer.countrz(noinline(0i)) == 64i)
assert(integer.countlz(noinline(1i)) == 63i)
assert(integer.countlz(noinline(8i)) == 60i)
assert(integer.countlz(noinline(0x200000i)) == 42i)
assert(integer.countlz(noinline(0i)) == 64i)

-- bswap
assert(integer.bswap(0x1122334455667748i) == 0x4877665544332211i)
assert(integer.bswap(0x0BADBEEFC001D00Di) == 0x0DD001C0EFBEAD0Bi)

assert(integer.bswap(noinline(0x1122334455667748i)) == 0x4877665544332211i)
assert(integer.bswap(noinline(0x0BADBEEFC001D00Di)) == 0x0DD001C0EFBEAD0Bi)

-- table indexing and hashes
local x = {}
x[3i] = 5
assert(x[3i] == 5)

-- buffers (should eventually move to buffers.luau)

local function simple_integer_ops()
  local b = buffer.create(16)

  buffer.writeinteger(b, 0, 0x123456789ABCDEF0i)
  buffer.writeinteger(b, 8, 0x1233211233211233i)

  assert(buffer.readi8(b, 0) == -16)
  assert(buffer.readi8(b, 1) == -34)
  assert(buffer.readi8(b, 2) == -68)
  assert(buffer.readi8(b, 3) == -102)
  assert(buffer.readi8(b, 4) == 120)
  assert(buffer.readi8(b, 5) == 86)
  assert(buffer.readi8(b, 6) == 52)
  assert(buffer.readi8(b, 7) == 18)
  assert(buffer.readi8(b, 8) == 0x33)
  assert(buffer.readi8(b, 9) == 0x12)
  assert(buffer.readi8(b, 10) == 0x21)
  assert(buffer.readi8(b, 11) == 0x33)
  assert(buffer.readi8(b, 12) == 0x12)
  assert(buffer.readi8(b, 13) == 0x21)
  assert(buffer.readi8(b, 14) == 0x33)
  assert(buffer.readi8(b, 15) == 0x12)

  assert(buffer.readinteger(b, 0) == 0x123456789ABCDEF0i)
  assert(buffer.readinteger(b, 8) == 0x1233211233211233i)

  assert(is_native_if_supported())
end

simple_integer_ops()

local function buffer_integer_boundary_values()
  local b = buffer.create(64)

  -- zero
  buffer.writeinteger(b, 0, 0i)
  assert(buffer.readinteger(b, 0) == 0i)

  -- max signed int64
  buffer.writeinteger(b, 8, 0x7FFFFFFFFFFFFFFFi)
  assert(buffer.readinteger(b, 8) == integer.maxsigned)

  -- min signed int64
  buffer.writeinteger(b, 16, integer.minsigned)
  assert(buffer.readinteger(b, 16) == integer.minsigned)

  -- -1 (all bits set)
  buffer.writeinteger(b, 24, -1i)
  assert(buffer.readinteger(b, 24) == -1i)
  assert(buffer.readu8(b, 24) == 0xFF)
  assert(buffer.readu8(b, 31) == 0xFF)

  -- +1
  buffer.writeinteger(b, 32, 1i)
  assert(buffer.readinteger(b, 32) == 1i)

  -- non-zero offset: verify 8-byte read/write at offset 40
  buffer.writeinteger(b, 40, 0xDEADBEEFCAFEBABEi)
  assert(buffer.readinteger(b, 40) == 0xDEADBEEFCAFEBABEi)

  -- verify adjacent writes don't clobber each other
  buffer.writeinteger(b, 0, 0x1111111111111111i)
  buffer.writeinteger(b, 8, 0x2222222222222222i)
  assert(buffer.readinteger(b, 0) == 0x1111111111111111i)
  assert(buffer.readinteger(b, 8) == 0x2222222222222222i)

  assert(is_native_if_supported())
end

buffer_integer_boundary_values()

-- stress tests for CHECK_BUFFER_LEN with constant offsets at exact boundaries
-- these exercise the native codegen constant-offset lowering path and range merging

local function buffer_integer_exact_boundary()
  -- exact-fit single access: offset + 8 == buffer.len
  local b8 = buffer.create(8)
  buffer.writeinteger(b8, 0, 0xAABBCCDDEEFF0011i)
  assert(buffer.readinteger(b8, 0) == 0xAABBCCDDEEFF0011i)

  -- exact-fit at end of buffer: 8 + 8 == 16
  local b16 = buffer.create(16)
  buffer.writeinteger(b16, 0, 0x1111111111111111i)
  buffer.writeinteger(b16, 8, 0x2222222222222222i)
  assert(buffer.readinteger(b16, 0) == 0x1111111111111111i)
  assert(buffer.readinteger(b16, 8) == 0x2222222222222222i)

  -- three adjacent writes filling buffer exactly: 0, 8, 16 in 24-byte buffer
  -- triggers range merging across all three constant offsets
  local b24 = buffer.create(24)
  buffer.writeinteger(b24, 0, 0xAAAAAAAAAAAAAAAAi)
  buffer.writeinteger(b24, 8, 0xBBBBBBBBBBBBBBBBi)
  buffer.writeinteger(b24, 16, 0xCCCCCCCCCCCCCCCCi)
  assert(buffer.readinteger(b24, 0) == 0xAAAAAAAAAAAAAAAAi)
  assert(buffer.readinteger(b24, 8) == 0xBBBBBBBBBBBBBBBBi)
  assert(buffer.readinteger(b24, 16) == 0xCCCCCCCCCCCCCCCCi)

  -- interleaved offsets to provoke negative minOffset in range merging
  -- write at 16 first, then read at 0, then write at 24
  local b32 = buffer.create(32)
  buffer.writeinteger(b32, 16, 0x3333333333333333i)
  buffer.writeinteger(b32, 0, 0x4444444444444444i)
  buffer.writeinteger(b32, 24, 0x5555555555555555i)
  buffer.writeinteger(b32, 8, 0x6666666666666666i)
  assert(buffer.readinteger(b32, 0) == 0x4444444444444444i)
  assert(buffer.readinteger(b32, 8) == 0x6666666666666666i)
  assert(buffer.readinteger(b32, 16) == 0x3333333333333333i)
  assert(buffer.readinteger(b32, 24) == 0x5555555555555555i)

  -- five adjacent writes filling 40-byte buffer exactly
  local b40 = buffer.create(40)
  buffer.writeinteger(b40, 0, 1i)
  buffer.writeinteger(b40, 8, 2i)
  buffer.writeinteger(b40, 16, 3i)
  buffer.writeinteger(b40, 24, 4i)
  buffer.writeinteger(b40, 32, 5i)
  assert(buffer.readinteger(b40, 0) == 1i)
  assert(buffer.readinteger(b40, 8) == 2i)
  assert(buffer.readinteger(b40, 16) == 3i)
  assert(buffer.readinteger(b40, 24) == 4i)
  assert(buffer.readinteger(b40, 32) == 5i)

  assert(is_native_if_supported())
end

buffer_integer_exact_boundary()

local function buffer_integer_variable_offset(start: number)
  local b = buffer.create(64)

  buffer.writeinteger(b, start, 0x0123456789ABCDEFi)
  buffer.writeinteger(b, start + 8, 0xFEDCBA9876543210i)
  buffer.writeinteger(b, start + 16, -1i)

  assert(buffer.readinteger(b, start) == 0x0123456789ABCDEFi)
  assert(buffer.readinteger(b, start + 8) == 0xFEDCBA9876543210i)
  assert(buffer.readinteger(b, start + 16) == -1i)

  -- buffer load-store propagation
  local a = buffer.readinteger(b, start)
  local c = buffer.readinteger(b, start)
  assert(a == c)

  assert(buffer.readu8(b, start) == 0xEF)
  assert(buffer.readu8(b, start + 7) == 0x01)
  assert(buffer.readu8(b, start + 8) == 0x10)
  assert(buffer.readu8(b, start + 15) == 0xFE)

  assert(is_native_if_supported())
end

buffer_integer_variable_offset(0)
buffer_integer_variable_offset(8)
buffer_integer_variable_offset(16)
buffer_integer_variable_offset(24)

-- constants

assert(integer.minsigned == integer.lshift(1i, 63i))
assert(integer.maxsigned == 0x7FFFFFFFFFFFFFFFi)

-- strings (should eventually move to strings.luau)

assert(string.format("%d", 3i) == "3")
assert(string.format("%d", -4i) == "-4")
assert(string.format("%d", 0x8000000000000000i) == "-9223372036854775808")
assert(string.format("%i", 42i) == "42")
assert(string.format("%i", -42i) == "-42")
assert(string.format("%x", 0x8000000000000000i) == "8000000000000000")
assert(string.format("%x", 0xFFFFFFFFFFFFFFFFi) == "ffffffffffffffff")
assert(string.format("%X", 0xABCDi) == "ABCD")
assert(string.format("%o", 8i) == "10")
assert(string.format("%o", 0xFFFFFFFFFFFFFFFFi) == "1777777777777777777777")
assert(string.format("%u", 0xFFFFFFFFFFFFFFFFi) == "18446744073709551615")
assert(string.format("%u", -1i) == "18446744073709551615")
assert(string.format("%u", 0x8000000000000000i) == "9223372036854775808")
assert(string.format("%*", 42i) == "42")
assert(string.format("%*", -1i) == "-1")
assert(string.format("%*", 0x8000000000000000i) == "-9223372036854775808")

-- Built-in tostring
assert(tostring(-48i) == "-48")
assert(tostring(0x8000000000000000i) == "-9223372036854775808")
assert(tostring(0xFFFFFFFFFFFFFFFFi) == "-1")

-- Unary minus can only be used on a literal directly
local intval = 123i
local ok, err = pcall(function() return -intval end)
assert(not ok)
local ok2, err2 = pcall(function() return -(123i) end)
assert(not ok2)

assert(is_native_if_supported())

return('OK')
