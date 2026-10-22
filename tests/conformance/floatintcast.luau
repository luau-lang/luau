-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("testing float-to-int conversion ranges")

local keys = {1e300, -1e300, 2^31, -2^31, 2^31-1, 0/0, math.huge, -math.huge}

local function expectError(fn, message, ...)
  local ok, err = pcall(fn, ...)
  assert(not ok)
  assert(string.find(tostring(err), message, 1, true), tostring(err))
end

-- Calling through a parameter exercises the library/API implementation.
local function call(fn, ...)
  return fn(...)
end

for _, key in keys do
  local t = {"array"}
  assert(t[key] == nil)
  assert(rawget(t, key) == nil)
  expectError(next, "invalid key to 'next'", t, key)

  if key ~= key then
    expectError(function() t[key] = "set" end, "table index is NaN")
    expectError(rawset, "table index is NaN", t, key, "raw")
  else
    t[key] = "set"
    assert(t[key] == "set")
    assert(rawget(t, key) == "set")
    rawset(t, key, "raw")
    assert(rawget(t, key) == "raw")
    assert(t[key] == "raw")
    assert(next(t, key) == nil)
  end
end

-- Rehash numeric hash keys while growing the array part.
local rehashed = {}
for _, key in keys do
  if key == key then
    rehashed[key] = "hash"
  end
end
for i = 1, 256 do
  rehashed[i] = i
end
for _, key in keys do
  if key == key then
    assert(rehashed[key] == "hash")
  end
end
for i = 1, 256 do
  assert(rehashed[i] == i)
end

local fractional = {"one", "two"}
for _, key in {0, -1, 1.5, -1.5, 2147483647.5, -2147483648.5} do
  fractional[key] = key
  assert(fractional[key] == key)
  assert(rawget(fractional, key) == key)
  rawset(fractional, key, "raw")
  assert(fractional[key] == "raw")
  next(fractional, key)
end
assert(fractional[1] == "one" and fractional[2] == "two")

local source = "abc"
for _, index in keys do
  local atMax = index == 2147483647
  assert(string.sub(source, index) == (atMax and "" or source))
  assert(call(string.sub, source, index) == (atMax and "" or source))
  assert(call(string.sub, source, 1, index) == (atMax and source or ""))
  if not atMax then
    assert(call(string.rep, "x", index) == "")
  end
end
assert(call(string.sub, source, 1.9, 2.9) == "ab")
assert(call(string.rep, "x", 2.9) == "xx")

local reads = {
  {buffer.readi8, function(b, p) return buffer.readi8(b, p) end},
  {buffer.readu8, function(b, p) return buffer.readu8(b, p) end},
  {buffer.readi16, function(b, p) return buffer.readi16(b, p) end},
  {buffer.readu16, function(b, p) return buffer.readu16(b, p) end},
  {buffer.readi32, function(b, p) return buffer.readi32(b, p) end},
  {buffer.readu32, function(b, p) return buffer.readu32(b, p) end},
  {buffer.readf32, function(b, p) return buffer.readf32(b, p) end},
  {buffer.readf64, function(b, p) return buffer.readf64(b, p) end},
}
local writes = {
  {buffer.writei8, function(b, p, v) buffer.writei8(b, p, v) end},
  {buffer.writeu8, function(b, p, v) buffer.writeu8(b, p, v) end},
  {buffer.writei16, function(b, p, v) buffer.writei16(b, p, v) end},
  {buffer.writeu16, function(b, p, v) buffer.writeu16(b, p, v) end},
  {buffer.writei32, function(b, p, v) buffer.writei32(b, p, v) end},
  {buffer.writeu32, function(b, p, v) buffer.writeu32(b, p, v) end},
  {buffer.writef32, function(b, p, v) buffer.writef32(b, p, v) end},
  {buffer.writef64, function(b, p, v) buffer.writef64(b, p, v) end},
}
local longValue
if buffer.readinteger then
  longValue = integer.create(1)
  table.insert(reads, {buffer.readinteger, function(b, p) return buffer.readinteger(b, p) end})
  table.insert(writes, {buffer.writeinteger, function(b, p, v) buffer.writeinteger(b, p, v) end})
end

local b = buffer.create(32)
for _, offset in keys do
  for _, read in reads do
    expectError(read[1], "buffer access out of bounds", b, offset)
    expectError(read[2], "buffer access out of bounds", b, offset)
  end
  for i, write in writes do
    local value = i == 9 and longValue or 1
    expectError(write[1], "buffer access out of bounds", b, offset, value)
    expectError(write[2], "buffer access out of bounds", b, offset, value)
  end
end
buffer.writeu8(b, 0.75, 42)
assert(buffer.readu8(b, 0) == 42)
assert(call(buffer.readu8, b, -0.75) == 42)
expectError(buffer.readu8, "buffer access out of bounds", b, -1)
expectError(buffer.readu8, "buffer access out of bounds", b, 32)

-- Direct calls take the VM fastcall path; `call` forces the library implementation.
local function outcome(ok, ...)
  if ok then
    return table.pack(true, ...)
  end
  return table.pack(false)
end

local function sameOutcome(a, b)
  if a.n ~= b.n then
    return false
  end
  for i = 1, a.n do
    if a[i] ~= b[i] and (a[i] == a[i] or b[i] == b[i]) then
      return false
    end
  end
  return true
end

local function sel(i, ...)
  local r = select(i, ...)
  return r
end

local function selLibrary(i, ...)
  local r = call(select, i, ...)
  return r
end

local fastcalls = {
  {function(v) return math.ldexp(1, v) end, function(v) return call(math.ldexp, 1, v) end},
  {function(v) return bit32.arshift(1, v) end, function(v) return call(bit32.arshift, 1, v) end},
  {function(v) return bit32.lshift(1, v) end, function(v) return call(bit32.lshift, 1, v) end},
  {function(v) return bit32.rshift(1, v) end, function(v) return call(bit32.rshift, 1, v) end},
  {function(v) return bit32.lrotate(1, v) end, function(v) return call(bit32.lrotate, 1, v) end},
  {function(v) return bit32.rrotate(1, v) end, function(v) return call(bit32.rrotate, 1, v) end},
  {function(v) return bit32.extract(1, v) end, function(v) return call(bit32.extract, 1, v) end},
  {function(v) return bit32.extract(1, 0, v) end, function(v) return call(bit32.extract, 1, 0, v) end},
  {function(v) return bit32.replace(1, 1, v) end, function(v) return call(bit32.replace, 1, 1, v) end},
  {function(v) return bit32.replace(1, 1, 0, v) end, function(v) return call(bit32.replace, 1, 1, 0, v) end},
  {function(v) return string.byte("abc", v) end, function(v) return call(string.byte, "abc", v) end},
  {function(v) return string.byte("abc", 1, v) end, function(v) return call(string.byte, "abc", 1, v) end},
  {function(v) return string.char(v) end, function(v) return call(string.char, v) end},
  {function(v) return string.sub("abc", v, 3) end, function(v) return call(string.sub, "abc", v, 3) end},
  {function(v) return string.sub("abc", 1, v) end, function(v) return call(string.sub, "abc", 1, v) end},
  {function(v) return table.unpack({1, 2}, 1, v) end, function(v) return call(table.unpack, {1, 2}, 1, v) end},
  {function(v) return #table.create(v) end, function(v) return #call(table.create, v) end},
}

local fastcallKeys = table.move(keys, 1, #keys, 9, {0, 1, 2, 3, -1, 1.5, 2.9, -0.5})

for _, key in fastcallKeys do
  for _, case in fastcalls do
    assert(sameOutcome(outcome(pcall(case[1], key)), outcome(pcall(case[2], key))), key)
  end
  assert(sameOutcome(outcome(pcall(sel, key, "a", "b", "c")), outcome(pcall(selLibrary, key, "a", "b", "c"))), key)
end

-- Library functions: out-of-range numbers either raise the existing error or produce the result x86-64 hardware gave.
do
  local huge = {1e300, -1e300, 2^63, -2^63, 2^64, 0/0, math.huge, -math.huge, -2^63 - 4096}
  local all = {1e300, -1e300, 2^31, -2^31, 2^63, -2^63, 0/0, math.huge, -math.huge}

  -- string.format converts out-of-range values to INT_MIN (%c) or INT64_MIN (integer formats)
  for _, v in huge do
    assert(string.format("%c", v) == "\0")
    assert(string.format("%d", v) == "-9223372036854775808")
    assert(string.format("%i", v) == "-9223372036854775808")
    assert(string.format("%5d", v) == "-9223372036854775808")
    assert(string.format("%o", v) == "1000000000000000000000")
    assert(string.format("%u", v) == "9223372036854775808")
    assert(string.format("%x", v) == "8000000000000000")
    assert(string.format("%X", v) == "8000000000000000")
  end
  local formatValues = {2^31, -2^31, 2^63 + 2^62, 2^32, -1.5, 0.5}
  local formatX = {"80000000", "ffffffff80000000", "c000000000000000", "100000000", "ffffffffffffffff", "0"}
  local formatD = {"2147483648", "-2147483648", "-9223372036854775808", "4294967296", "-1", "0"}
  local formatU = {"2147483648", "18446744071562067968", "13835058055282163712", "4294967296", "18446744073709551615", "0"}
  for i, v in formatValues do
    if i <= 4 then
      assert(string.format("%c", v) == "\0")
    end
    assert(string.format("%x", v) == formatX[i])
    assert(string.format("%d", v) == formatD[i])
    assert(string.format("%u", v) == formatU[i])
  end

  -- string.pack raises the overflow error for sized integers, and packs INT64_MIN for 8+ byte integers
  for _, v in huge do
    for _, f in {"i1", "i2", "i4", "i7", "j"} do
      expectError(string.pack, "integer overflow", f, v)
    end
    for _, f in {"I1", "I4", "I7", "J"} do
      expectError(string.pack, "unsigned overflow", f, v)
    end
    assert(string.pack("<i8", v) == "\0\0\0\0\0\0\0\x80")
    assert(string.pack("<I8", v) == "\0\0\0\0\0\0\0\x80")
    assert(string.pack("<i16", v) == "\0\0\0\0\0\0\0\x80\xff\xff\xff\xff\xff\xff\xff\xff")
    assert(string.pack("<I16", v) == "\0\0\0\0\0\0\0\x80\0\0\0\0\0\0\0\0")
  end
  assert(string.pack("<i8", formatValues[3]) == "\0\0\0\0\0\0\0\x80")
  assert(string.pack("<I4", formatValues[1]) == "\0\0\0\x80")
  expectError(string.pack, "unsigned overflow", "I4", formatValues[2])

  -- buffer.readbits/writebits reject offsets that don't fit, including near 2^63 where offset + count overflows
  local b = buffer.create(8)
  for _, v in table.move(huge, 1, #huge, 1, {2^31, -2^31, 2^63 - 1024, 2^62, 2^53}) do
    expectError(buffer.readbits, "buffer access out of bounds", b, v, 32)
    expectError(buffer.writebits, "buffer access out of bounds", b, v, 32, 1)
    expectError(call, "buffer access out of bounds", buffer.readbits, b, v, 1)
    expectError(call, "buffer access out of bounds", buffer.writebits, b, v, 1, 1)
  end
  local negativeHalf = formatValues[5] + 1
  call(buffer.writebits, b, negativeHalf, 1, 1)
  assert(call(buffer.readbits, b, negativeHalf, 8) == 1)

  -- buffer.copy/fill compute their default size from very negative offsets without overflowing
  for _, v in {-2^31, -2^31 + 1, 2^31 - 1, 1e300} do
    expectError(buffer.copy, "buffer access out of bounds", b, 0, b, v)
    expectError(buffer.fill, "buffer access out of bounds", b, v, 0)
  end

  -- os.date/os.difftime treat out-of-range times as INT64_MIN: an invalid date, or that value in a difference
  for _, v in huge do
    assert(os.date("!%Y", v) == nil)
    assert(os.date("%Y", v) == nil)
    assert(os.date("!*t", v) == nil)
    assert(os.difftime(v, 0) == -2^63)
    assert(os.difftime(0, v) == 2^63)
    assert(os.difftime(v) == -2^63)
  end
  assert(os.date("!%Y", formatValues[1]) == "2038")
  -- Windows rejects times before the epoch
  local year1901 = os.date("!%Y", formatValues[2])
  assert(year1901 == nil or year1901 == "1901")
  assert(os.difftime(formatValues[1], formatValues[2]) == 2^32)
  -- years near INT_MAX wrap when converted back to a calendar year (when the platform's gmtime supports them)
  local lastYear = os.date("!*t", 67767976233316800)
  local wrappedYear = os.date("!*t", 67767976233532800)
  assert(lastYear == nil or lastYear.year == 2147483647)
  assert(wrappedYear == nil or wrappedYear.year == -2147483648)

  -- os.time wraps date arithmetic on out-of-range fields (INT_MIN) the way x86-64 did
  for _, v in all do
    assert(os.time({year = v, month = 1, day = 1}) == 140486342673600)
    assert(os.time({year = 2000, month = v, day = 1}) == 81059861851200)
    assert(os.time({year = 2000, month = 1, day = v}) == nil)
    assert(os.time({year = 2000, month = 1, day = 1, hour = v}) == nil)
    assert(os.time({year = 2000, month = 1, day = 1, sec = v}) == nil)
  end
  local intMax = formatValues[1] - 1
  assert(os.time({year = 10000000, month = intMax, day = 1}) == 25481090894400)
  assert(os.time({year = -10000000, month = 1, day = 1}) == 55453487284800)
  assert(os.time({year = intMax, month = 1, day = 1}) == 140486311137600)
  assert(os.time({year = 6000000, month = 1, day = 1}) == nil)
  assert(os.time({year = 2000, month = 1, day = 1}) == 946728000)

  -- math.noise maps out-of-range cells (and NaN/inf) to cell 0 instead of converting them to int
  for _, v in {0/0, math.huge, -math.huge} do
    local r = math.noise(v, 0.3, 0.7)
    assert(r ~= r)
    r = math.noise(0.3, 0.7, v)
    assert(r ~= r)
  end
  local zero = formatValues[6] - 0.5
  for _, v in {2^31, -2^31, 2^32, 2^63, -2^63, 2^64, 1e300, -1e300} do
    local r = math.noise(v, 0.3, 0.7)
    -- without FixMathNoisePrecision, 1e300 becomes inf as a float
    assert(r == math.noise(zero, 0.3, 0.7) or (r ~= r and math.abs(v) > 1e300 / 2))
    r = math.noise(0.3, v, 0.7)
    assert(r == math.noise(0.3, zero, 0.7) or (r ~= r and math.abs(v) > 1e300 / 2))
  end

  -- bit32 shifts and rotates by INT_MIN (from -2^31 or any out-of-range value) behave like any |shift| >= 32
  for _, s in {-2^31, 1e300, -1e300, 0/0, math.huge, -math.huge} do
    for _, x in {1, 0x80000000, 0xffffffff, 0x12345678} do
      assert(bit32.lshift(x, s) == 0 and call(bit32.lshift, x, s) == 0)
      assert(bit32.rshift(x, s) == 0 and call(bit32.rshift, x, s) == 0)
      assert(bit32.arshift(x, s) == 0 and call(bit32.arshift, x, s) == 0)
      assert(bit32.lrotate(x, s) == x and call(bit32.lrotate, x, s) == x)
      assert(bit32.rrotate(x, s) == x and call(bit32.rrotate, x, s) == x)
    end
  end
  local minusIntMax = -intMax
  assert(bit32.rrotate(1, minusIntMax) == 0x80000000 and call(bit32.rrotate, 1, minusIntMax) == 0x80000000)
  assert(bit32.lrotate(1, minusIntMax) == 2 and call(bit32.lrotate, 1, minusIntMax) == 2)
  assert(bit32.rshift(1, minusIntMax) == 0 and call(bit32.rshift, 1, minusIntMax) == 0)

  -- bit32.extract/replace with field + width overflowing int: fastcall and library must both reject it
  local fields = {intMax, intMax - 1, intMax - 31, 32, 31, 1}
  for _, f in fields do
    for _, w in fields do
      if f + w > 32 then
        expectError(function() return bit32.extract(0xffffffff, f, w) end, "trying to access non-existent bits")
        expectError(call, "trying to access non-existent bits", bit32.extract, 0xffffffff, f, w)
        expectError(function() return bit32.replace(0xffffffff, 0, f, w) end, "trying to access non-existent bits")
        expectError(call, "trying to access non-existent bits", bit32.replace, 0xffffffff, 0, f, w)
      end
    end
  end
  assert(bit32.extract(0xffffffff, fields[5], fields[6]) == 1)
  assert(bit32.replace(0xffffffff, 0, fields[5], fields[6]) == 0x7fffffff)
end

return "OK"
