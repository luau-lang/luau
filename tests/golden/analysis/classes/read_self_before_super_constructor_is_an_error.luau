--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

open class Base
    public value: number?

    function __init(self) end
end

class Derived extends Base
    function __init(self)
        print(self.value)
        Base.__init(self)
        print(self.value)
    end
end
