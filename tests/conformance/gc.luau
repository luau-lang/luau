-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This file is based on Lua 5.x tests -- https://github.com/lua/lua/tree/master/testes
print('testing garbage collection')

collectgarbage()

_G["while"] = 234

limit = 5000



contCreate = 0

print('tables')
while contCreate <= limit do
  local a = {}; a = nil
  contCreate = contCreate+1
end

a = "a"

contCreate = 0
print('strings')
while contCreate <= limit do
  a = contCreate .. "b";
  a = string.gsub(a, '(%d%d*)', string.upper)
  a = "a"
  contCreate = contCreate+1
end


contCreate = 0

a = {}

print('functions')
function a:test ()
  while contCreate <= limit do
    loadstring(string.format("function temp(a) return 'a%d' end", contCreate))()
    assert(temp() == string.format('a%d', contCreate))
    contCreate = contCreate+1
  end
end

a:test()

-- collection of functions without locals, globals, etc.
do local f = function () end end


print('long strings')
x = "01234567890123456789012345678901234567890123456789012345678901234567890123456789"
assert(string.len(x)==80)
s = ''
n = 0
k = 300
while n < k do s = s..x; n=n+1; j=tostring(n)  end
assert(string.len(s) == k*80)
s = string.sub(s, 1, 20000)
s, i = string.gsub(s, '(%d%d%d%d)', math.sin)
assert(i==20000/4)
s = nil
x = nil

assert(_G["while"] == 234)


local bytes = gcinfo()
while 1 do
  local nbytes = gcinfo()
  if nbytes < bytes then break end   -- run until gc
  bytes = nbytes
  a = {}
end


local function dosteps (siz)
  collectgarbage()
  collectgarbage("stop")
  local a = {}
  for i=1,100 do a[i] = {{}}; local b = {} end
  local x = gcinfo()
  local i = 0
  repeat
    i = i+1
  until collectgarbage("step", siz)
  assert(gcinfo() < x)
  return i
end

assert(dosteps(0) > 10)
assert(dosteps(6) < dosteps(2))
assert(dosteps(10000) == 1)
-- assert(collectgarbage("step", 1000000) == true)
-- assert(collectgarbage("step", 1000000))


do
  local x = gcinfo()
  collectgarbage()
  collectgarbage("stop")
  repeat
    local a = {}
  until gcinfo() > 1000
  collectgarbage("restart")
  repeat
    local a = {}
  until gcinfo() < 1000
end

lim = 15
a = {}
-- fill a with `collectable' indices
for i=1,lim do a[{}] = i end
b = {}
for k,v in pairs(a) do b[k]=v end
-- remove all indices and collect them
for n in pairs(b) do
  a[n] = nil
  assert(type(n) == 'table' and next(n) == nil)
  collectgarbage()
end
b = nil
collectgarbage()
for n in pairs(a) do error("cannot be here") end
for i=1,lim do a[i] = i end
for i=1,lim do assert(a[i] == i) end


print('weak tables')
a = {}; setmetatable(a, {__mode = 'k'});
-- fill a with some `collectable' indices
for i=1,lim do a[{}] = i end
-- and some non-collectable ones
for i=1,lim do local t={}; a[t]=t end
for i=1,lim do a[i] = i end
for i=1,lim do local s=string.rep('@', i); a[s] = s..'#' end
collectgarbage()
local i = 0
for k,v in pairs(a) do assert(k==v or k..'#'==v); i=i+1 end
assert(i == 3*lim)

a = {}; setmetatable(a, {__mode = 'v'});
a[1] = string.rep('b', 21)
collectgarbage()
assert(a[1])   -- strings are *values*
a[1] = nil
-- fill a with some `collectable' values (in both parts of the table)
for i=1,lim do a[i] = {} end
for i=1,lim do a[i..'x'] = {} end
-- and some non-collectable ones
for i=1,lim do local t={}; a[t]=t end
for i=1,lim do a[i+lim]=i..'x' end
collectgarbage()
local i = 0
for k,v in pairs(a) do assert(k==v or k-lim..'x' == v); i=i+1 end
assert(i == 2*lim)

a = {}; setmetatable(a, {__mode = 'vk'});
local x, y, z = {}, {}, {}
-- keep only some items
a[1], a[2], a[3] = x, y, z
a[string.rep('$', 11)] = string.rep('$', 11)
-- fill a with some `collectable' values
for i=4,lim do a[i] = {} end
for i=1,lim do a[{}] = i end
for i=1,lim do local t={}; a[t]=t end
collectgarbage()
assert(next(a) ~= nil)
local i = 0
for k,v in pairs(a) do
  assert((k == 1 and v == x) or
         (k == 2 and v == y) or
         (k == 3 and v == z) or k==v);
  i = i+1
end
assert(i == 4)
x,y,z=nil
collectgarbage()
assert(next(a) == string.rep('$', 11))

-- shrinking tables reduce their capacity; confirming the shrinking is difficult but we can at least test the surface level behavior
a = {}; setmetatable(a, {__mode = 'ks'})
for i=1,lim do a[{}] = i end
collectgarbage()
assert(next(a) == nil)

-- shrinking a table should be robust against an OOM
do
    local count = 100000
    local extra = 1000
    a = {}
    setmetatable(a, {__mode = 'ks'})

    -- items to be collected
    for i = 1,count do a[{}] = i end

    -- items that will still be reachable
    for i = 1,extra do a[tostring(i)] = {} end

    setblockallocations(true)
    collectgarbage()
    setblockallocations(false)

    for k,v in a do assert(type(k) == "string") end
end

-- shrinking strings should be robust against an OOM
do
    local count = 100000
    a = {}

    for i = 1,count do a[tostring(count)] = i end
    table.clear(a);

    setblockallocations(true)
    collectgarbage()
    setblockallocations(false)
end

-- shrinking thread stacks should be robust against an OOM
do
    function recurse(n, ...) return n <= 1 and (1 + #{...}) or recurse(n-1, table.unpack(table.create(4000, 1))) + 1 end
    recurse(100)

    setblockallocations(true)
    collectgarbage()
    setblockallocations(false)
end

-- testing userdata
collectgarbage("stop")   -- stop collection
local u = newproxy(true)
local s = 0
local a = {[u] = 0}; setmetatable(a, {__mode = 'vk'})
for i=1,10 do a[newproxy(true)] = i end
-- for k in pairs(a) do assert(getmetatable(k) == getmetatable(u)) end
local a1 = {}; for k,v in pairs(a) do a1[k] = v end
for k,v in pairs(a1) do a[v] = k end
for i =1,10 do assert(a[i]) end
getmetatable(u).a = a1
getmetatable(u).u = u
do
  local u = u
  getmetatable(u).__gc = function (o)
    assert(a[o] == 10-s)
    assert(a[10-s] == nil) -- udata already removed from weak table
    assert(getmetatable(o) == getmetatable(u))
    assert(getmetatable(o).a[o] == 10-s)
    s=s+1
  end
end
a1, u = nil
assert(next(a) ~= nil)
collectgarbage()
-- assert(s==11)
collectgarbage()
assert(next(a) == nil)  -- finalized keys are removed in two cycles


-- __gc x weak tables
local u = newproxy(true)
setmetatable(getmetatable(u), {__mode = "v"})
getmetatable(u).__gc = function (o) os.exit(1) end  -- cannot happen
collectgarbage()

local u = newproxy(true)
local m = getmetatable(u)
m.x = {[{0}] = 1; [0] = {1}}; setmetatable(m.x, {__mode = "kv"});
m.__gc = function (o)
  assert(next(getmetatable(o).x) == nil)
  m = 10
end
u, m = nil
collectgarbage()
-- assert(m==10)


-- errors during collection
u = newproxy(true)
getmetatable(u).__gc = function () error "!!!" end
u = nil
-- assert(not pcall(collectgarbage))
collectgarbage()


if not rawget(_G, "_soft") then
  print("deep structures")
  local a = {}
  for i = 1,200000 do
    a = {next = a}
  end
  collectgarbage()
end

-- create many threads with self-references and open upvalues
do
  local thread_id = 0
  local threads = {}

  function fn(thread)
      local x = {}
      threads[thread_id] = function()
                               thread = x
                           end
      coroutine.yield()
  end

  while thread_id < 1000 do
      local thread = coroutine.create(fn)
      coroutine.resume(thread, thread)
      thread_id = thread_id + 1
  end

  collectgarbage()

  -- ensure that we no longer have a lot of reachable threads for subsequent tests
  threads = {}
end

-- create a userdata to be collected when state is closed
do
  local newproxy,assert,type,print,getmetatable =
        newproxy,assert,type,print,getmetatable
  local u = newproxy(true)
  local tt = getmetatable(u)
  ___Glob = {u}   -- avoid udata being collected before program end
  tt.__gc = function (o)
    assert(getmetatable(o) == tt)
    -- create new objects during GC
    local a = 'xuxu'..(10+3)..'joao', {}
    ___Glob = o  -- resurrect object!
    newproxy(o)  -- creates a new one with same metatable
    print(">>> closing state " .. "<<<\n")
  end
end

-- create several udata to raise errors when collected while closing state
do
  local u = newproxy(true)
  getmetatable(u).__gc = function (o) return o + 1 end
  table.insert(___Glob, u)  -- preserve udata until the end
  for i = 1,10 do table.insert(___Glob, newproxy(true)) end
end

-- create threads that die together with their unmarked upvalues
do
  local t = {}

  for i = 1,100 do
    local c = coroutine.wrap(function()
      local uv = {i + 1}
      local function f()
        return uv[1] * 10
      end
      coroutine.yield(uv[1])
      uv = {i + 2}
      coroutine.yield(f())
    end)

    assert(c() == i + 1)
    table.insert(t, c)
  end

  for i = 1,100 do
    t[i] = nil
  end

  collectgarbage()
end

-- create a lot of threads with upvalues to force a case where full gc happens after we've marked some upvalues
do
  local t = {}
  for i = 1,100 do
    local c = coroutine.wrap(function()
      local uv = {i + 1}
      local function f()
        return uv[1] * 10
      end
      coroutine.yield(uv[1])
      uv = {i + 2}
      coroutine.yield(f())
    end)

    assert(c() == i + 1)
    table.insert(t, c)
  end

  t = {}

  collectgarbage()
end

return('OK')
