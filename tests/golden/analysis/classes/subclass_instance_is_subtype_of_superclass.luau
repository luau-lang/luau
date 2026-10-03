--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

open class Animal
    function __init(self) end
end

class Dog extends Animal
    function __init(self) end
end

local d = Dog()
local a: Animal = d
