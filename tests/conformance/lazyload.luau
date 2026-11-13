-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- Loaded with luau_loadlazy: every function below main is built on its first call
print "testing lazy loading"

-- a function can call itself through a metamethod on its first call
local reentered = 0
local function readLib()
  return _G.Lib.value
end
_G.Lib = setmetatable({}, { __index = function(t, k)
  reentered += 1
  if reentered == 1 then
    return readLib()
  end
  return 42
end })
assert(readLib() == 42)
assert(reentered == 2)

-- debug info of functions that never ran
local function neverCalled(a, b, ...) end
assert(debug.info(neverCalled, "s") == "lazyload.luau")
assert(debug.info(neverCalled, "l") == 21)
assert(debug.info(neverCalled, "n") == "neverCalled")
assert(select(2, debug.info(neverCalled, "a")) == true)

-- errors raised by lazily built functions report their own lines
local function fails()
  error("boom")
end
local ok, err = pcall(fails)
assert(not ok and err == "lazyload.luau:29: boom")

-- first calls from C, coroutines, metamethods and varargs
assert(pcall(function() return 1 end))
assert(coroutine.wrap(function(a) return a + 1 end)(1) == 2)
local mt = setmetatable({}, { __add = function(a, b) return 7 end })
assert(mt + 1 == 7)
local function count(...) return select("#", ...) end
assert(count(1, nil, 3) == 3)

-- recursion into a function on its first call
local function fib(n) return if n < 2 then n else fib(n - 1) + fib(n - 2) end
assert(fib(15) == 610)

-- closures built later keep working after a collection
local makers = {}
for i = 1, 100 do
  makers[i] = function() return function() return i end end
end
collectgarbage()
for i = 1, 100 do
  assert(makers[i]()() == i)
end

-- with optimizations, a closure without upvalues is a constant of its parent as well as its child
local function makeDoubler()
  return function(x) return x * 2 end
end
assert(makeDoubler()(21) == 42)

return 'OK'
