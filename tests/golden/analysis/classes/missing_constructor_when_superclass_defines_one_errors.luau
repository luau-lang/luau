--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

open class Animal
    public name: string

    function __init(self, name: string)
        self.name = name
    end
end

class Dog extends Animal
    function bark(self) end
end
