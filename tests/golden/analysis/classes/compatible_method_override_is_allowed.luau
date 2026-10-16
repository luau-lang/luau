--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

open class Base
    function method(self, x: number | string): {}?
        return {}
    end

    function staticMethod(x: number | string): {}?
        return {}
    end
end

class ChildOne extends Base
    function method(self, x: number | string | nil): {x: string}
        return {x = "hello"}
    end

    function staticMethod(x: number | string | nil): {x: string}
        return {x = "hello"}
    end
end
