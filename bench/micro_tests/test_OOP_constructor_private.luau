-- --bench-args: --fflags=DebugLuauUserDefinedClasses,DebugLuauUserDefinedClassesRuntime,LuauEmitCallFeedback
local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

class Number
    private _x
    function __init(self, x)
        self._x = x
    end
end

function test()
    local ts0 = os.clock()
    for i=1,1_000_000 do
        local n = Number(42)
    end
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "OOP: private field constructor")
