-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("testing native code generation")

assert((function(x, y)
  -- trigger a linear sequence
  local t1 = x + 2
  local t2 = x - 7

  local a = x * 10
  local b = a + 1
  a = y -- update 'a' version
  local t = {} -- call to allocate table forces a spill
  local c = x * 10
  return c, b, t, t1, t2
end)(5, 10) == 50)

assert((function(x)
  local oops -- split to prevent inlining
  function oops()
  end

  -- x is checked to be a number here; we can not execute a reentry from oops() because optimizer assumes this holds until return
  local y = math.abs(x)
  oops()
  return y * x
end)("42") == 1764)

local function fuzzfail1(...)
  repeat
    _ = nil
  until not {}
  for _ in ... do
    for l0=_,_ do
    end
    return
  end
end

local function fuzzfail2()
  local _
  do
    repeat
      _ = typeof(_),{_=_,}
      _ = _(_._)
    until _
  end
end

assert(pcall(fuzzfail2) == false)

local function fuzzfail3()
  function _(...)
    _({_,_,true,},{...,},_,not _)
  end
  _()
end

assert(pcall(fuzzfail3) == false)

local function fuzzfail4()
  local _ = setmetatable({},setmetatable({_=_,},_))
  return _(_:_())
end

assert(pcall(fuzzfail4) == false)

local function fuzzfail5()
  local _ = bit32.band
  _(_(_,0),_)
  _(_,_)
end

assert(pcall(fuzzfail5) == false)

local function fuzzfail6(_)
  return bit32.extract(_,671088640,_)
end

assert(pcall(fuzzfail6, 1) == false)

local function fuzzfail7(_)
  return bit32.extract(_,_,671088640)
end

assert(pcall(fuzzfail7, 1) == false)

local function fuzzfail8(...)
  local _ = _,_
  _.n0,_,_,_,_,_,_,_,_._,_,_,_[...],_,_,_ = nil
  _,n0,_,_,_,_,_,_,_,_,l0,_,_,_,_ = nil
  function _()
  end
  _._,_,_,_,_,_,_,_,_,_,_[...],_,n0[l0],_ = nil
  _[...],_,_,_,_,_,_,_,_()[_],_,_,_,_,_ = _(),...
end

assert(pcall(fuzzfail8) == false)

local function fuzzfail9()
  local _ = bit32.bor
  local x = _(_(_,_),_(_,_),_(-16834560,_),_(_(- _,-2130706432)),- _),_(_(_,_),_(-16834560,-2130706432))
end

assert(pcall(fuzzfail9) == false)

local function fuzzfail10()
  local _
  _ = false,if _ then _ else _
  _ = not _
  l0,_[l0] = not _
end

assert(pcall(fuzzfail10) == false)

local function fuzzfail11(x, ...)
  return bit32.arshift(bit32.bnot(x),(...))
end

assert(fuzzfail11(0xffff0000, 8) == 0xff)

local function fuzzfail12()
  _,_,_,_,_,_,_,_ = not _, not _, not _, not _, not _, not _, not _, not _
end

assert(pcall(fuzzfail12) == true)

local function fuzzfail13()
  _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ = not _, not _, not _, not _, not _, not _, not _, not _, not _, not _, not _, not _, not _, not _, not _, not _
end

assert(pcall(fuzzfail13) == true)

local function fuzzfail14()
  for l0=771751936,_ do
    for l0=771751936,0 do
      while 538970624 do
      end
    end
  end
end

assert(pcall(fuzzfail14) == false)

local function fuzzfail15()
  local a
  if a then
    repeat until a
  else
    local b = `{a}`
    a = nil
  end
end

assert(pcall(fuzzfail15) == true)

local function fuzzfail16()
  _ = {[{[2]=77,_=_,[2]=_,}]=not _,}
  _ = {77,[2]=11008,[2]=_,[0]=_,}
end

assert(pcall(fuzzfail16) == true)

local function fuzzfail17()
  return bit32.extract(1293942816,1293942816)
end

assert(pcall(fuzzfail17) == false)

local function fuzzfail18()
  return bit32.extract(7890276,0)
end

assert(pcall(fuzzfail18) == true)
assert(fuzzfail18() == 0)

local function fuzzfail19()
  local _ = 2
  _ += _
  _ = _,_ >= _,{_ >= _,_ >= _,_(),}

  local _ = 2
  do
    _ = assert({n0=_,_,n0=_,}),{_={_[_()],},_,}
  end
end

assert(pcall(fuzzfail19) == false)

local function fuzzfail20()
  assert(true)
  assert(false,(_),true)
  _ = nil
end

assert(pcall(fuzzfail20) == false)

local function fuzzfail21(...)
  local _ = assert,_
  if _ then else return _ / _ end
  _(_)
  _(_,_)
  assert(...,_)
  _((not _),_)
  _(true,_ / _)
  _(_,_())
  return _
end

assert(pcall(fuzzfail21) == false)

local function fuzzfail22(...)
  local _ = {false,},true,...,l0
  while _ do
  _ = true,{unpack(0,_),},l0
  _.n126 = nil
  _ = {not _,_=not _,n0=_,_,n0=not _,},_ < _
  return _ > _
  end
  return `""`
end

assert(pcall(fuzzfail22) == false)

local function fuzzfail23(...)
  local _ = {false,},_,...,l0
  while _ do
  _ = true,{unpack(_),},l0
  _ = {{[_]=nil,_=not _,_,true,_=nil,},not _,not _,_,bxor=- _,}
  do end
  break
  end
  do end
  local _ = _,true
  do end
  local _ = _,true
end

assert(pcall(fuzzfail23) == false)

local function arraySizeInv1()
  local t = {1, 2, nil, nil, nil, nil, nil, nil, nil, true}

  table.insert(t, 3)

  return t[10]
end

assert(arraySizeInv1() == true)

local function arraySizeInv2()
  local t = {1, 2, nil, nil, nil, nil, nil, nil, nil, true}

  local u = {a = t}
  table.insert(u.a, 3) -- aliased modifiction of 't' register through other value

  return t[10]
end

assert(arraySizeInv2() == true)

local function nilInvalidatesSlot()
  local function tabs()
    local t = { x=1, y=2, z=3 }
    setmetatable(t, { __index = function(t, k) return 42 end })
    return t, t
  end

  local t1, t2 = tabs()

  for i=1,2 do
    local a = t1.x
    t2.x = nil
    local b = t1.x
    t2.x = 1
    assert(a == 1 and b == 42)
  end
end

nilInvalidatesSlot()

local function arraySizeOpt1(a)
  a[1] += 2
  a[1] *= 3

  table.insert(a, 3)
  table.insert(a, 4)
  table.insert(a, 5)
  table.insert(a, 6)

  a[1] += 4
  a[1] *= 5

  return a[1] + a[5]
end

assert(arraySizeOpt1({1}) == 71)

local function arraySizeOpt2(a, i)
  a[i] += 2
  a[i] *= 3

  table.insert(a, 3)
  table.insert(a, 4)
  table.insert(a, 5)
  table.insert(a, 6)

  a[i] += 4
  a[i] *= 5

  return a[i] + a[5]
end

assert(arraySizeOpt2({1}, 1) == 71)

function deadLoopBody(n)
  local r = 0
  if n and false then
    for i = 1, n do
      r += 1
    end
  end
  return r
end

assert(deadLoopBody(5) == 0)

function arrayIndexingSpecialNumbers1(a, b, c)
  local arr = table.create(100000)
  arr[a] = 9
  arr[b-1] = 80
  arr[b] = 700
  arr[b+1] = 6000
  arr[c-1] = 50000
  arr[c] = 400000
  arr[c+1] = 3000000

  return arr[1] + arr[255] + arr[256] + arr[257] + arr[65535] + arr[65536] + arr[65537]
end

assert(arrayIndexingSpecialNumbers1(1, 256, 65536) == 3456789)

function loopIteratorProtocol(a, t)
  local sum = 0

  do
    local a, b, c, d, e, f, g = {}, {}, {}, {}, {}, {}, {}
  end

  for k, v in ipairs(t) do
    if k == 10 then sum += math.abs('-8') end

    sum += k
  end

  return sum
end

assert(loopIteratorProtocol(0, table.create(100, 5)) == 5058)

function valueTrackingIssue1()
  local b = buffer.create(1)
  buffer.writeu8(b, 0, 0)
  local v1

  local function closure()
    assert(type(b) == "buffer") -- b is the first upvalue
    v1 = nil -- v1 is the second upvalue

    -- prevent inlining
    for i = 1, 100 do print(`{b} is {b}`) end
  end

  closure()
end

valueTrackingIssue1()

local function vec3compsum(a: vector)
  return a.X + a.Y + a.Z
end

assert(vec3compsum(vector(1, 2, 4)) == 7.0)

local function vec3add(a: vector, b: vector) return a + b end
local function vec3sub(a: vector, b: vector) return a - b end
local function vec3mul(a: vector, b: vector) return a * b end
local function vec3div(a: vector, b: vector) return a / b end
local function vec3neg(a: vector) return -a end

assert(vec3add(vector(10, 20, 40), vector(1, 0, 2)) == vector(11, 20, 42))
assert(vec3sub(vector(10, 20, 40), vector(1, 0, 2)) == vector(9, 20, 38))
assert(vec3mul(vector(10, 20, 40), vector(1, 0, 2)) == vector(10, 0, 80))
assert(vec3div(vector(10, 20, 40), vector(1, 0, 2)) == vector(10, math.huge, 20))
assert(vec3neg(vector(10, 20, 40)) == vector(-10, -20, -40))

local function vec3mulnum(a: vector, b: number) return a * b end
local function vec3mulconst(a: vector) return a * 4 end

assert(vec3mulnum(vector(10, 20, 40), 4) == vector(40, 80, 160))
assert(vec3mulconst(vector(10, 20, 40), 4) == vector(40, 80, 160))

local function bufferbounds(zero)
  local b1 = buffer.create(1)
  local b2 = buffer.create(2)
  local b4 = buffer.create(4)
  local b8 = buffer.create(8)
  local b10 = buffer.create(10)

  -- only one valid position and size for a 1 byte buffer
  buffer.writei8(b1, zero + 0, buffer.readi8(b1, zero + 0))
  buffer.writeu8(b1, zero + 0, buffer.readu8(b1, zero + 0))

  -- 2 byte buffer
  buffer.writei8(b2, zero + 0, buffer.readi8(b2, zero + 0))
  buffer.writeu8(b2, zero + 0, buffer.readu8(b2, zero + 0))
  buffer.writei8(b2, zero + 1, buffer.readi8(b2, zero + 1))
  buffer.writeu8(b2, zero + 1, buffer.readu8(b2, zero + 1))
  buffer.writei16(b2, zero + 0, buffer.readi16(b2, zero + 0))
  buffer.writeu16(b2, zero + 0, buffer.readu16(b2, zero + 0))

  -- 4 byte buffer
  buffer.writei8(b4, zero + 0, buffer.readi8(b4, zero + 0))
  buffer.writeu8(b4, zero + 0, buffer.readu8(b4, zero + 0))
  buffer.writei8(b4, zero + 3, buffer.readi8(b4, zero + 3))
  buffer.writeu8(b4, zero + 3, buffer.readu8(b4, zero + 3))
  buffer.writei16(b4, zero + 0, buffer.readi16(b4, zero + 0))
  buffer.writeu16(b4, zero + 0, buffer.readu16(b4, zero + 0))
  buffer.writei16(b4, zero + 2, buffer.readi16(b4, zero + 2))
  buffer.writeu16(b4, zero + 2, buffer.readu16(b4, zero + 2))
  buffer.writei32(b4, zero + 0, buffer.readi32(b4, zero + 0))
  buffer.writeu32(b4, zero + 0, buffer.readu32(b4, zero + 0))
  buffer.writef32(b4, zero + 0, buffer.readf32(b4, zero + 0))

  -- 8 byte buffer
  buffer.writei8(b8, zero + 0, buffer.readi8(b8, zero + 0))
  buffer.writeu8(b8, zero + 0, buffer.readu8(b8, zero + 0))
  buffer.writei8(b8, zero + 7, buffer.readi8(b8, zero + 7))
  buffer.writeu8(b8, zero + 7, buffer.readu8(b8, zero + 7))
  buffer.writei16(b8, zero + 0, buffer.readi16(b8, zero + 0))
  buffer.writeu16(b8, zero + 0, buffer.readu16(b8, zero + 0))
  buffer.writei16(b8, zero + 6, buffer.readi16(b8, zero + 6))
  buffer.writeu16(b8, zero + 6, buffer.readu16(b8, zero + 6))
  buffer.writei32(b8, zero + 0, buffer.readi32(b8, zero + 0))
  buffer.writeu32(b8, zero + 0, buffer.readu32(b8, zero + 0))
  buffer.writef32(b8, zero + 0, buffer.readf32(b8, zero + 0))
  buffer.writei32(b8, zero + 4, buffer.readi32(b8, zero + 4))
  buffer.writeu32(b8, zero + 4, buffer.readu32(b8, zero + 4))
  buffer.writef32(b8, zero + 4, buffer.readf32(b8, zero + 4))
  buffer.writef64(b8, zero + 0, buffer.readf64(b8, zero + 0))

  -- 'any' size buffer
  buffer.writei8(b10, zero + 0, buffer.readi8(b10, zero + 0))
  buffer.writeu8(b10, zero + 0, buffer.readu8(b10, zero + 0))
  buffer.writei8(b10, zero + 9, buffer.readi8(b10, zero + 9))
  buffer.writeu8(b10, zero + 9, buffer.readu8(b10, zero + 9))
  buffer.writei16(b10, zero + 0, buffer.readi16(b10, zero + 0))
  buffer.writeu16(b10, zero + 0, buffer.readu16(b10, zero + 0))
  buffer.writei16(b10, zero + 8, buffer.readi16(b10, zero + 8))
  buffer.writeu16(b10, zero + 8, buffer.readu16(b10, zero + 8))
  buffer.writei32(b10, zero + 0, buffer.readi32(b10, zero + 0))
  buffer.writeu32(b10, zero + 0, buffer.readu32(b10, zero + 0))
  buffer.writef32(b10, zero + 0, buffer.readf32(b10, zero + 0))
  buffer.writei32(b10, zero + 6, buffer.readi32(b10, zero + 6))
  buffer.writeu32(b10, zero + 6, buffer.readu32(b10, zero + 6))
  buffer.writef32(b10, zero + 6, buffer.readf32(b10, zero + 6))
  buffer.writef64(b10, zero + 0, buffer.readf64(b10, zero + 0))
  buffer.writef64(b10, zero + 2, buffer.readf64(b10, zero + 2))

  assert(is_native())
end

bufferbounds(0)

function deadStoreChecks1()
  local a = 1.0
  local b = 0.0

  local function update()
    b += a
    for i = 1, 100 do print(`{b} is {b}`) end
  end

  update()
  a = 10
  update()
  a = 100
  update()

  return b
end

assert(deadStoreChecks1() == 111)

local function extramath1(a)
  return type(math.sign(a))
end

assert(extramath1(2) == "number")
assert(extramath1("2") == "number")

local function extramath2(a)
  return type(math.modf(a))
end

assert(extramath2(2) == "number")
assert(extramath2("2") == "number")

local function extramath3(a)
  local b, c = math.modf(a)
  return type(c)
end

assert(extramath3(2) == "number")
assert(extramath3("2") == "number")

local function slotcachelimit1()
  local tbl = {
    f1 = function() return 1 end,
    f2 = function() return 2 end,
    f3 = function() return 3 end,
    f4 = function() return 4 end,
    f5 = function() return 5 end,
    f6 = function() return 6 end,
    f7 = function() return 7 end,
    f8 = function() return 8 end,
    f9 = function() return 9 end,
    f10 = function() return 10 end,
    f11 = function() return 11 end,
    f12 = function() return 12 end,
    f13 = function() return 13 end,
    f14 = function() return 14 end,
    f15 = function() return 15 end,
    f16 = function() return 16 end,
  }

  local lookup = {
    [tbl.f1] = 1,
    [tbl.f2] = 2,
    [tbl.f3] = 3,
    [tbl.f4] = 4,
    [tbl.f5] = 5,
    [tbl.f6] = 6,
    [tbl.f7] = 7,
    [tbl.f8] = 8,
    [tbl.f9] = 9,
    [tbl.f10] = 10,
    [tbl.f11] = 11,
    [tbl.f12] = 12,
    [tbl.f13] = 13,
    [tbl.f14] = 14,
    [tbl.f15] = 15,
    [tbl.f16] = 16,
  }

  assert(is_native())

  return lookup
end

slotcachelimit1()

local function slotcachelimit2(foo, size)
  local c1 = foo(vector.create(size.X, size.Y, size.Z))
  local c2 = foo(vector.create(-size.X, size.Y, size.Z))
  local c3 = foo(vector.create(-size.X, -size.Y, size.Z))
  local c4 = foo(vector.create(-size.X, -size.Y, -size.Z))
  local c5 = foo(vector.create(size.X, -size.Y, -size.Z))
  local c6 = foo(vector.create(size.X, size.Y, -size.Z))
  local c7 = foo(vector.create(size.X, -size.Y, size.Z))
  local c8 = foo(vector.create(-size.X, size.Y, -size.Z))
  local max = vector.create(math.max(c1.X, c2.X, c3.X, c4.X, c5.X, c6.X, c7.X, c8.X), math.max(c1.Y, c2.Y, c3.Y, c4.Y, c5.Y, c6.Y, c7.Y, c8.Y), math.max(c1.Z, c2.Z, c3.Z, c4.Z, c5.Z, c6.Z, c7.Z, c8.Z))
  local min = vector.create(math.min(c1.X, c2.X, c3.X, c4.X, c5.X, c6.X, c7.X, c8.X), math.min(c1.Y, c2.Y, c3.Y, c4.Y, c5.Y, c6.Y, c7.Y, c8.Y), math.min(c1.Z, c2.Z, c3.Z, c4.Z, c5.Z, c6.Z, c7.Z, c8.Z))

  assert(is_native())
  return max - min
end

slotcachelimit2(function(a) return -a end, vector.create(1, 2, 3))

return('OK')
