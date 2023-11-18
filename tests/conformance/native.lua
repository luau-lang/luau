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

return('OK')
