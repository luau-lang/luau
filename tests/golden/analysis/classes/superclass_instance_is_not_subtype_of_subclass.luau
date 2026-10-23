--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

open class Animal
    function __init(self) end
end

class Dog extends Animal
    function __init(self) end
end

local a = Animal()
local d: Dog = a
