-- --bench-args: --fflags=DebugLuauUserDefinedClasses,DebugLuauUserDefinedClassesRuntime,LuauEmitCallFeedback
local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

open class Base
    public x
    function __init(self, x)
        self.x = x
    end
end

class Derived extends Base
    public y
    function __init(self, x, y)
        Base.__init(self, x)
        self.y = y
    end
end

function test()
    local ts0 = os.clock()
    for i=1,1_000_000 do
        local n = Derived(42, 24)
    end
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "OOP: inherited user-defined constructor")
