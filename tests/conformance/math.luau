-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This file is based on Lua 5.x tests -- https://github.com/lua/lua/tree/master/testes
print("testing numbers and math lib")

do
  local a,b,c = "2", " 3e0 ", " 10  "
  assert(a+b == 5 and -b == -3 and b+"2" == 5 and "10"-c == 0)
  assert(type(a) == 'string' and type(b) == 'string' and type(c) == 'string')
  assert(a == "2" and b == " 3e0 " and c == " 10  " and -c == -"  10 ")
  assert(c%a == 0 and a^b == 8)
end


do
  local a,b = math.modf(3.5)
  assert(a == 3 and b == 0.5)
  assert(math.huge > 10e30)
  assert(-math.huge < -10e30)
end

function f(...)
  if select('#', ...) == 1 then
    return (...)
  else
    return "***"
  end
end

assert(pcall(tonumber) == false)
assert(tonumber{} == nil)
assert(tonumber'+0.01' == 1/100 and tonumber'+.01' == 0.01 and
       tonumber'.01' == 0.01    and tonumber'-1.' == -1 and
       tonumber'+1.' == 1)
assert(tonumber'+ 0.01' == nil and tonumber'+.e1' == nil and
       tonumber'1e' == nil     and tonumber'1.0e+' == nil and
       tonumber'.' == nil)
assert(tonumber('-12') == -10-2)
assert(tonumber('-1.2e2') == - - -120)
assert(f(tonumber('1  a')) == nil)
assert(f(tonumber('e1')) == nil)
assert(f(tonumber('e  1')) == nil)
assert(f(tonumber(' 3.4.5 ')) == nil)
assert(f(tonumber('')) == nil)
assert(f(tonumber('', 8)) == nil)
assert(f(tonumber('  ')) == nil)
assert(f(tonumber('  ', 9)) == nil)
assert(f(tonumber('99', 8)) == nil)
assert(tonumber('  1010  ', 2) == 10)
assert(tonumber('10', 36) == 36)
--assert(tonumber('\n  -10  \n', 36) == -36)
--assert(tonumber('-fFfa', 16) == -(10+(16*(15+(16*(15+(16*15)))))))
assert(tonumber('fFfa', 15) == nil)
--assert(tonumber(string.rep('1', 42), 2) + 1 == 2^42)
assert(tonumber(string.rep('1', 32), 2) + 1 == 2^32)
--assert(tonumber('-fffffFFFFF', 16)-1 == -2^40)
assert(tonumber('ffffFFFF', 16)+1 == 2^32)

assert(1.1 == 1.+.1)
assert(100.0 == 1E2 and .01 == 1e-2)
assert(1111111111111111-1111111111111110== 1000.00e-03)
--     1234567890123456
assert(1.1 == '1.'+'.1')
assert('1111111111111111'-'1111111111111110' == tonumber"  +0.001e+3 \n\t")
assert(10000000000000001 == 10000000000000000)

function eq (a,b,limit)
  if not limit then limit = 10E-10 end
  return math.abs(a-b) <= limit
end

assert(0.1e-30 > 0.9E-31 and 0.9E30 < 0.1e31)

assert(0.123456 > 0.123455)

assert(tonumber('+1.23E30') == 1.23*10^30)

-- testing order operators
assert(not(1<1) and (1<2) and not(2<1))
assert(not('a'<'a') and ('a'<'b') and not('b'<'a'))
assert((1<=1) and (1<=2) and not(2<=1))
assert(('a'<='a') and ('a'<='b') and not('b'<='a'))
assert(not(1>1) and not(1>2) and (2>1))
assert(not('a'>'a') and not('a'>'b') and ('b'>'a'))
assert((1>=1) and not(1>=2) and (2>=1))
assert(('a'>='a') and not('a'>='b') and ('b'>='a'))
assert((unk and unk > 0) == nil) -- validate precedence between and and >

-- testing mod operator
assert(-4%3 == 2)
assert(4%-3 == -2)
assert(math.pi - math.pi % 1 == 3)
assert(math.pi - math.pi % 0.001 == 3.141)

do
  local a = 3 % 0;
  assert(a ~= a) -- Expect NaN
  assert(((2^53+1) % 2) == 0)
  assert((1234 % (2^53+1)) == 1234)
end

local function testbit(a, n)
  return a/2^n % 2 >= 1
end

assert(eq(math.sin(-9.8)^2 + math.cos(-9.8)^2, 1))
assert(eq(math.tan(math.pi/4), 1))
assert(eq(math.sin(math.pi/2), 1) and eq(math.cos(math.pi/2), 0))
assert(eq(math.atan(1), math.pi/4) and eq(math.acos(0), math.pi/2) and
       eq(math.asin(1), math.pi/2))
assert(eq(math.deg(math.pi/2), 90) and eq(math.rad(90), math.pi/2))
assert(math.abs(-10) == 10)
assert(eq(math.atan2(1,0), math.pi/2))
assert(math.ceil(4.5) == 5.0)
assert(math.floor(4.5) == 4.0)
assert(10 % 3 == 1)
assert(eq(math.sqrt(10)^2, 10))
assert(eq(math.log10(2), math.log(2)/math.log(10)))
assert(eq(math.log(2, 2), 1))
assert(eq(math.log(9, 3), 2))
assert(eq(math.log(100, 10), 2))
assert(eq(math.exp(0), 1))
assert(eq(math.sin(10), math.sin(10%(2*math.pi))))
local v,e = math.frexp(math.pi)
assert(eq(math.ldexp(v,e), math.pi))

assert(eq(math.tanh(3.5), math.sinh(3.5)/math.cosh(3.5)))

assert(tonumber(' 1.3e-2 ') == 1.3e-2)
assert(tonumber(' -1.00000000000001 ') == -1.00000000000001)

-- testing constant limits
-- 2^23 = 8388608
assert(8388609 + -8388609 == 0)
assert(8388608 + -8388608 == 0)
assert(8388607 + -8388607 == 0)

if rawget(_G, "_soft") then return end

f = "a = {"
i = 1
repeat
  f = f .. "{" .. math.sin(i) .. ", " .. math.cos(i) .. ", " .. (i/3) .. "},\n"
  i=i+1
until i > 1000
f = f .. "}"
assert(loadstring(f))()

assert(eq(a[300][1], math.sin(300)))
assert(eq(a[600][1], math.sin(600)))
assert(eq(a[500][2], math.cos(500)))
assert(eq(a[800][2], math.cos(800)))
assert(eq(a[200][3], 200/3))
assert(eq(a[1000][3], 1000/3, 0.001))
print('+')

do   -- testing NaN
  local NaN -- to avoid constant folding
  NaN = 10e500 - 10e400

  assert(NaN ~= NaN)
  assert(not (NaN == NaN))

  assert(not (NaN < NaN))
  assert(not (NaN <= NaN))
  assert(not (NaN > NaN))
  assert(not (NaN >= NaN))

  assert(not (0 == NaN))
  assert(not (0 < NaN))
  assert(not (0 <= NaN))
  assert(not (0 > NaN))
  assert(not (0 >= NaN))

  assert(not (NaN == 0))
  assert(not (NaN < 0))
  assert(not (NaN <= 0))
  assert(not (NaN > 0))
  assert(not (NaN >= 0))

  assert(if NaN < 0 then false else true)
  assert(if NaN <= 0 then false else true)
  assert(if NaN > 0 then false else true)
  assert(if NaN >= 0 then false else true)

  local a = {}
  assert(not pcall(function () a[NaN] = 1 end))
  assert(a[NaN] == nil)
  a[1] = 1
  assert(not pcall(function () a[NaN] = 1 end))
  assert(a[NaN] == nil)
end

-- extra NaN tests, hidden in a function
do
  function neq(a) return a ~= a end
  function eq(a) return a == a end
  function lt(a) return a < a end
  function le(a) return a <= a end
  function gt(a) return a > a end
  function ge(a) return a >= a end

  local NaN -- to avoid constant folding
  NaN = 10e500 - 10e400

  assert(neq(NaN))
  assert(not eq(NaN))
  assert(not lt(NaN))
  assert(not le(NaN))
  assert(not gt(NaN))
  assert(not ge(NaN))
end

-- require "checktable"
-- stat(a)

a = nil

-- testing implicit conversions

local a,b = '10', '20'
assert(a*b == 200 and a+b == 30 and a-b == -10 and a/b == 0.5 and -b == -20)
assert(a == '10' and b == '20')


math.randomseed(0)

local i = 0
local Max = 0
local Min = 2
repeat
  local t = math.random()
  Max = math.max(Max, t)
  Min = math.min(Min, t)
  i=i+1
  flag = eq(Max, 1, 0.001) and eq(Min, 0, 0.001)
until flag or i>10000
assert(0 <= Min and Max<1)
assert(flag);

for i=1,10 do
  local t = math.random(5)
  assert(1 <= t and t <= 5)
end

i = 0
Max = -200
Min = 200
repeat
  local t = math.random(-10,0)
  Max = math.max(Max, t)
  Min = math.min(Min, t)
  i=i+1
  flag = (Max == 0 and Min == -10)
until flag or i>10000
assert(-10 <= Min and Max<=0)
assert(flag);

assert(select(2, pcall(math.random, 1, 2, 3)):match("wrong number of arguments"))

-- argument count
function nothing() end

assert(pcall(math.abs) == false)
assert(pcall(function() return math.abs(nothing()) end) == false)

-- min/max
assert(math.min(1) == 1)
assert(math.min(1, 2) == 1)
assert(math.min(1, 2, -1) == -1)
assert(math.min(1, -1, 2) == -1)
assert(math.min(1, -1, 2, -2) == -2)
assert(math.max(1) == 1)
assert(math.max(1, 2) == 2)
assert(math.max(1, 2, -1) == 2)
assert(math.max(1, -1, 2) == 2)
assert(math.max(1, -1, 2, -2) == 2)

local ma, mb, mc, md

assert(pcall(function()
  ma = 1
  mb = -1
  mc = 2
  md = -2
end) == true)

-- min/max without contant-folding
assert(math.min(ma) == 1)
assert(math.min(ma, mc) == 1)
assert(math.min(ma, mc, mb) == -1)
assert(math.min(ma, mb, mc) == -1)
assert(math.min(ma, mb, mc, md) == -2)
assert(math.max(ma) == 1)
assert(math.max(ma, mc) == 2)
assert(math.max(ma, mc, mb) == 2)
assert(math.max(ma, mb, mc) == 2)
assert(math.max(ma, mb, mc, md) == 2)

local inf = math.huge * 2
local nan = 0 / 0

assert(math.min(nan, 2) ~= math.min(nan, 2))
assert(math.min(1, nan) == 1)
assert(math.max(nan, 2) ~= math.max(nan, 2))
assert(math.max(1, nan) == 1)

local function noinline(x, ...) local s, r = pcall(function(y) return y end, x) return r end

-- noise
assert(math.noise(0.5) == 0)
assert(math.noise(0.5, 0.5) == -0.25)
assert(math.noise(0.5, 0.5, -0.5) == 0.125)
assert(math.noise(455.7204209769105, 340.80410508750134, 121.80087666537628) == 0.5010709762573242)

assert(math.noise(noinline(0.5)) == 0)
assert(math.noise(noinline(0.5), 0.5) == -0.25)
assert(math.noise(noinline(0.5), 0.5, -0.5) == 0.125)
assert(math.noise(noinline(455.7204209769105), 340.80410508750134, 121.80087666537628) == 0.5010709762573242)

-- sign
assert(math.sign(0) == 0)
assert(math.sign(42) == 1)
assert(math.sign(-42) == -1)
assert(math.sign(inf) == 1)
assert(math.sign(-inf) == -1)
assert(math.sign(nan) == 0)

assert(math.sign(noinline(0)) == 0)
assert(math.sign(noinline(42)) == 1)
assert(math.sign(noinline(-42)) == -1)
assert(math.sign(noinline(inf)) == 1)
assert(math.sign(noinline(-inf)) == -1)
assert(math.sign(noinline(nan)) == 0)

-- clamp
assert(math.clamp(-1, 0, 1) == 0)
assert(math.clamp(0.5, 0, 1) == 0.5)
assert(math.clamp(2, 0, 1) == 1)
assert(math.clamp(4, 0, 0) == 0)

assert(math.clamp(noinline(-1), 0, 1) == 0)
assert(math.clamp(noinline(0.5), 0, 1) == 0.5)
assert(math.clamp(noinline(2), 0, 1) == 1)
assert(math.clamp(noinline(4), 0, 0) == 0)

-- round
assert(math.round(0) == 0)
assert(math.round(0.4) == 0)
assert(math.round(0.5) == 1)
assert(math.round(3.5) == 4)
assert(math.round(-0.4) == 0)
assert(math.round(-0.5) == -1)
assert(math.round(-3.5) == -4)
assert(math.round(math.huge) == math.huge)
assert(math.round(0.49999999999999994) == 0)
assert(math.round(-0.49999999999999994) == 0)

assert(math.round(noinline(0)) == 0)
assert(math.round(noinline(0.4)) == 0)
assert(math.round(noinline(0.5)) == 1)
assert(math.round(noinline(3.5)) == 4)
assert(math.round(noinline(-0.4)) == 0)
assert(math.round(noinline(-0.5)) == -1)
assert(math.round(noinline(-3.5)) == -4)
assert(math.round(noinline(math.huge)) == math.huge)
assert(math.round(noinline(0.49999999999999994)) == 0)
assert(math.round(noinline(-0.49999999999999994)) == 0)

-- fmod
assert(math.fmod(3, 2) == 1)
assert(math.fmod(-3, 2) == -1)
assert(math.fmod(3, -2) == 1)
assert(math.fmod(-3, -2) == -1)

assert(math.fmod(noinline(3), 2) == 1)
assert(math.fmod(noinline(-3), 2) == -1)
assert(math.fmod(noinline(3), -2) == 1)
assert(math.fmod(noinline(-3), -2) == -1)

-- pow
assert(math.pow(2, 0) == 1)
assert(math.pow(2, 2) == 4)
assert(math.pow(4, 0.5) == 2)
assert(math.pow(-2, 2) == 4)

assert(math.pow(noinline(2), 0) == 1)
assert(math.pow(noinline(2), 2) == 4)
assert(math.pow(noinline(4), 0.5) == 2)
assert(math.pow(noinline(-2), 2) == 4)

-- map
assert(math.map(0, -1, 1, 0, 2) == 1)
assert(math.map(1, 1, 4, 0, 2) == 0)
assert(math.map(2.5, 1, 4, 0, 2) == 1)
assert(math.map(4, 1, 4, 0, 2) == 2)
assert(math.map(1, 1, 4, 2, 0) == 2)
assert(math.map(2.5, 1, 4, 2, 0) == 1)
assert(math.map(4, 1, 4, 2, 0) == 0)
assert(math.map(1, 4, 1, 2, 0) == 0)
assert(math.map(2.5, 4, 1, 2, 0) == 1)
assert(math.map(4, 4, 1, 2, 0) == 2)
assert(math.map(-8, 0, 4, 0, 2) == -4)
assert(math.map(16, 0, 4, 0, 2) == 8)

-- lerp basics
assert(math.lerp(1, 5, 0) == 1)
assert(math.lerp(1, 5, 1) == 5)
assert(math.lerp(1, 5, 0.5) == 3)
assert(math.lerp(1, 5, 1.5) == 7)
assert(math.lerp(1, 5, -0.5) == -1)
assert(math.lerp(1, 5, noinline(0.5)) == 3)

-- lerp properties
local sq2, sq3 = math.sqrt(2), math.sqrt(3)
assert(math.lerp(sq2, sq3, 0) == sq2) -- exact at 0
assert(math.lerp(sq2, sq3, 1) == sq3) -- exact at 1
assert(math.lerp(-sq3, sq2, 1) == sq2) -- exact at 1 (fails for a + t*(b-a))
assert(math.lerp(sq2, sq2, sq2 / 2) <= math.lerp(sq2, sq2, 1)) -- monotonic (fails for a*t + b*(1-t))
assert(math.lerp(-sq3, sq2, 1) <= math.sqrt(2)) -- bounded (fails for a + t*(b-a))
assert(math.lerp(sq2, sq2, sq2 / 2) == sq2) -- consistent (fails for a*t + b*(1-t))

assert(tostring(math.pow(-2, 0.5)) == "nan")

-- test that fastcalls return correct number of results
assert(select('#', math.floor(1.4)) == 1)
assert(select('#', math.ceil(1.6)) == 1)
assert(select('#', math.sqrt(9)) == 1)
assert(select('#', math.deg(9)) == 1)
assert(select('#', math.rad(9)) == 1)
assert(select('#', math.sin(1.5)) == 1)
assert(select('#', math.atan2(1.5, 0.5)) == 1)
assert(select('#', math.modf(1.5)) == 2)
assert(select('#', math.frexp(1.5)) == 2)

-- test that fastcalls that return variadic results return them correctly in variadic position
assert(select(1, math.modf(1.5)) == 1)
assert(select(2, math.modf(1.5)) == 0.5)
assert(select(1, math.frexp(1.5)) == 0.75)
assert(select(2, math.frexp(1.5)) == 1)

-- most of the tests above go through fastcall path
-- to make sure the basic implementations are also correct we test these functions with string->number coercions
assert(math.abs("-4") == 4)
assert(math.acos("1") == 0)
assert(math.asin("0") == 0)
assert(math.atan2("0", "0") == 0)
assert(math.atan("0") == 0)
assert(math.ceil("1.5") == 2)
assert(math.cosh("0") == 1)
assert(math.cos("0") == 1)
assert(math.deg("0") == 0)
assert(math.exp("0") == 1)
assert(math.floor("1.5") == 1)
assert(math.fmod("1.5", 1) == 0.5)
local v,e = math.frexp("1.5")
assert(v == 0.75 and e == 1)
assert(math.ldexp("0.75", 1) == 1.5)
assert(math.log10("10") == 1)
assert(math.log("0") == -inf)
assert(math.log("8", 2) == 3)
assert(math.log("10", 10) == 1)
assert(math.log("16", 4) == 2)
assert(math.max("1", 2) == 2)
assert(math.max(2, "1") == 2)
assert(math.max(1, 2, "3") == 3)
assert(math.min("1", 2) == 1)
assert(math.min(2, "1") == 1)
assert(math.min(1, 2, "3") == 1)
local v,f = math.modf("1.5")
assert(v == 1 and f == 0.5)
assert(math.pow("2", 2) == 4)
assert(math.rad("0") == 0)
assert(math.sinh("0") == 0)
assert(math.sin("0") == 0)
assert(math.sqrt("4") == 2)
assert(math.tanh("0") == 0)
assert(math.tan("0") == 0)
assert(math.clamp("0", 2, 3) == 2)
assert(math.clamp("4", 2, 3) == 3)
assert(math.sign("2") == 1)
assert(math.sign("-2") == -1)
assert(math.sign("0") == 0)
assert(math.round("1.8") == 2)
assert(math.lerp("1", "5", 0.5) == 3)

return('OK')
