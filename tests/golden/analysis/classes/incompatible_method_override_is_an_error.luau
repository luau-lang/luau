--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

open class Base
    function method(self, x: number | string): {}?
        return {}
    end
end

class Child extends Base
    function method(self, x: number): {} | string
        return ""
    end
end

open class ChildTwo extends Base end

class GrandChild extends ChildTwo
    function method(self, x: number): {} | string
        return ""
    end
end