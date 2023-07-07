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

return('OK')
