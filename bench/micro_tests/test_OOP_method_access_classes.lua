-- --bench-args: --fflags=DebugLuauUserDefinedClasses,DebugLuauUserDefinedClassesRuntime
local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

class Number
    public value
    function Get(self)
        return self.value
    end
end

function test()

    local n = Number { value = 42 }

    local ts0 = os.clock()
    for i=1,10000000 do
        local _ = n.Get
    end
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "OOP: method access class")