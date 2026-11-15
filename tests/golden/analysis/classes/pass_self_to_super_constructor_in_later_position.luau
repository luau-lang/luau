--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

open class Base
    function __init(self, x) end
end

class Derived extends Base
    function __init(self)
        Base.__init(self, self)
    end
end
