--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

open class Base
    function method(x: number | string): {}?
        return {}
    end

    function instanceMethod(self, x: number | string): {}?
        return {}
    end

    function staticMethod(x: number | string): {}?
        return {}
    end
end

class ChildTwo extends Base
    function method(x: number): {} | string
        return ""
    end

    function instanceMethod(s: string, x: number | string): {}?
        return {}
    end

    function staticMethod(self): {}?
        return {}
    end
end
