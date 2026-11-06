-- --bench-args: --fflags=DebugLuauUserDefinedClasses,DebugLuauUserDefinedClassesRuntime,LuauEmitCallFeedback
local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

class Complex
 private _re
 private _im

 public function __init(self, re, im)
    self._re = re
    self._im = im
 end

 function conj(x)
  return Complex(x._re, -x._im)
 end

 function norm2(x)
  return (x * Complex.conj(x))._re
 end

 function abs(x)
  return math.sqrt(Complex.norm2(x))
 end

 private function _add(x,y)
  return Complex(x._re+y._re,x._im+y._im)
 end

 private function _mul(x,y)
  return Complex(x._re*y._re-x._im*y._im, x._re*y._im+x._im*y._re)
 end

 function __add(x,y)
  return Complex._add(x,y)
 end

 function __mul(x,y)
  return Complex._mul(x,y)
 end
end

function test()

local function abs(x)
 return math.sqrt(Complex.norm2(x))
end

xmin=-2.0	xmax=2.0	ymin=-2.0	ymax=2.0
N=(arg and arg[1]) or 64

function level(x,y)
 local c=Complex(x, y)
 local l=0
 local z=c
 repeat
  z=z*z+c
  l=l+1
 until abs(z)>2.0 or l>255
 return l-1
end

dx=(xmax-xmin)/N
dy=(ymax-ymin)/N

print("P2")
print("# mandelbrot set",xmin,xmax,ymin,ymax,N)
print(N,N,255)
local S = 0
for i=1,N do
 local x=xmin+(i-1)*dx
 for j=1,N do
  local y=ymin+(j-1)*dy
  S = S + level(x,y)
 end
end
print(S)

end

bench.runCode(test, "mandel classes")
