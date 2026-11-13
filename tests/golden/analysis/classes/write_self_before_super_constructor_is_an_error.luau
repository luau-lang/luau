--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

open class Base
    public value: number?

    function __init(self) end
end

class Derived extends Base
    function __init(self)
        self.value = 1
        Base.__init(self)
        self.value = 2
    end
end
