-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This file is based on Lua 5.x tests -- https://github.com/lua/lua/tree/master/testes
print"testing sort"

function checksort(t, f, ...)
  assert(#t == select('#', ...))
  local copy = table.clone(t)
  table.sort(copy, f)
  for i=1,#t do assert(copy[i] == select(i, ...)) end
end

-- basic edge cases
checksort({}, nil)
checksort({1}, nil, 1)

-- small inputs
checksort({1, 2}, nil, 1, 2)
checksort({2, 1}, nil, 1, 2)

checksort({1, 2, 3}, nil, 1, 2, 3)
checksort({2, 1, 3}, nil, 1, 2, 3)
checksort({1, 3, 2}, nil, 1, 2, 3)
checksort({3, 2, 1}, nil, 1, 2, 3)
checksort({3, 1, 2}, nil, 1, 2, 3)

-- "large" input
checksort({3, 8, 1, 7, 10, 2, 5, 4, 9, 6}, nil, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
checksort({"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}, nil, "Apr", "Aug", "Dec", "Feb", "Jan", "Jul", "Jun", "Mar", "May", "Nov", "Oct", "Sep")

-- duplicates
checksort({3, 1, 1, 7, 1, 3, 5, 1, 9, 3}, nil, 1, 1, 1, 1, 3, 3, 3, 5, 7, 9)

-- predicates
checksort({3, 8, 1, 7, 10, 2, 5, 4, 9, 6}, function (a, b) return a > b end, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

-- can't sort readonly tables
assert(pcall(table.sort, table.freeze({2, 1})) == false)

-- first argument must be a table, second argument must be nil or function
assert(pcall(table.sort) == false)
assert(pcall(table.sort, "abc") == false)
assert(pcall(table.sort, {}, 42) == false)
assert(pcall(table.sort, {}, {}) == false)

-- legacy Lua tests
function check (a, f)
  f = f or function (x,y) return x<y end;
  for n=table.getn(a),2,-1 do
    assert(not f(a[n], a[n-1]))
  end
end

a = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
     "Oct", "Nov", "Dec"}

table.sort(a)
check(a)

limit = 30000
if rawget(_G, "_soft") then limit = 5000 end

a = {}
for i=1,limit do
  a[i] = math.random()
end

local x = os.clock()
table.sort(a)
print(string.format("Sorting %d elements in %.2f sec.", limit, os.clock()-x))
check(a)

x = os.clock()
table.sort(a)
print(string.format("Re-sorting %d elements in %.2f sec.", limit, os.clock()-x))
check(a)

a = {}
for i=1,limit do
  a[i] = math.random()
end

x = os.clock(); i=0
table.sort(a, function(x,y) i=i+1; return y<x end)
print(string.format("Invert-sorting other %d elements in %.2f sec., with %i comparisons",
      limit, os.clock()-x, i))
check(a, function(x,y) return y<x end)


table.sort{}  -- empty array

for i=1,limit do a[i] = false end
x = os.clock();
table.sort(a, function(x,y) return nil end)
print(string.format("Sorting %d equal elements in %.2f sec.", limit, os.clock()-x))
check(a, function(x,y) return nil end)
for i,v in pairs(a) do assert(not v or i=='n' and v==limit) end

a = {"álo", "\0first :-)", "alo", "then this one", "45", "and a new"}
table.sort(a)
check(a)

-- TODO: assert that pcall returns false for new sort implementation (table is modified during sorting)
pcall(table.sort, a, function (x, y)
          loadstring(string.format("a[%q] = ''", x))()
          collectgarbage()
          return x<y
        end)

tt = {__lt = function (a,b) return a.val < b.val end}
a = {}
for i=1,10 do  a[i] = {val=math.random(100)}; setmetatable(a[i], tt); end
table.sort(a)
check(a, tt.__lt)
check(a)

return"OK"
