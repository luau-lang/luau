--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true
open class Base end

class Child extends Base end

local _b: typeof(Base) = Child