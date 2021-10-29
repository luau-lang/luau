--[[
The Great Computer Language Shootout
http://shootout.alioth.debian.org/

contributed by Ian Osgood
]]
local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

function A(i,j)
  return 1/((i+j)*(i+j+1)/2+i+1);
end

function Au(u,v)
  for i = 0,#u-1 do
    local t = 0;
    for j = 0,#u-1 do
      t = t + A(i,j) * u[j + 1];
    end
    v[i + 1] = t;
  end
end

function Atu(u,v)
  for i = 0,#u-1 do
    local t = 0;
    for j = 0,#u-1 do
      t = t + A(j,i) * u[j + 1];
    end
    v[i + 1] = t;
  end
end

function AtAu(u,v,w)
  Au(u,w);
  Atu(w,v);
end

function spectralnorm(n)
  local u, v, w, vv, vBv = {}, {}, {}, 0, 0;
  for i = 1,n do
    u[i] = 1; v[i] = 0; w[i] = 0;
  end
  for i = 0,9 do
    AtAu(u,v,w);
    AtAu(v,u,w);
  end
  for i = 1,n do
    vBv = vBv + u[i]*v[i];
    vv  = vv + v[i]*v[i];
  end
  return math.sqrt(vBv/vv);
end

local total = 0;
local i = 6

while i <= 48 do
    total = total + spectralnorm(i);
    i = i * 2
end

local expected = 5.086694231303284;

if (total ~= expected) then
    assert(false, "ERROR: bad result: expected " .. expected .. " but got " .. total)
end

end

bench.runCode(test, "math-spectral-norm")
