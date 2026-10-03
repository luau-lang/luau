--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

open class Base
    function method(self, x: number | string): {}?
        return {}
    end
end

class ChildTwo extends Base
    function method(self, x: number): {} | string
        return ""
    end
end
