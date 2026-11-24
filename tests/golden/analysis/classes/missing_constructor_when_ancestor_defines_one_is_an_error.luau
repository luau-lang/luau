--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

open class Base
    function __init(self) end
end

open class Middle extends Base
end

class Leaf extends Middle
end

open class Overriding extends Base
    function __init(self) end
end

class OverridingLeaf extends Overriding
end
