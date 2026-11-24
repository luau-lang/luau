--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

open class Base
    public x: number
end

open class Middle extends Base
end

class Leaf extends Middle
end

class ChildWithConstructor extends Base
    function __init(self) end
end

open class WithConstructor
    function __init(self) end
end

class ChildAlsoWithConstructor extends WithConstructor
    function __init(self) end
end
