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

return "OK"
