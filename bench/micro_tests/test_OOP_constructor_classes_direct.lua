-- --bench-args: --fflags=DebugLuauUserDefinedClasses,DebugLuauUserDefinedClassesRuntime
local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

class Number
    public x
    function new(x)
        return Number { x = x }
    end
    function Get(self)
        return self.x
    end
end

function test()

    local ts0 = os.clock()
    for i=1,1_000_000 do
        local n = Number { x = 42 }
    end
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "OOP: class constructor")
