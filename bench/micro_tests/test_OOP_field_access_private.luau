-- --bench-args: --fflags=DebugLuauUserDefinedClasses,DebugLuauUserDefinedClassesRuntime,LuauEmitCallFeedback
local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

-- Private members are only accessible from methods of the declaring class, so the loop lives in a method
class Number
    private _value
    function __init(self, value)
        self._value = value
    end

    function Run(self, iterations)
        for i=1,iterations do
            local _ = self._value
        end
    end
end

function test()
    local n = Number(42)

    local ts0 = os.clock()
    n:Run(10_000_000)
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "OOP: private field access")
