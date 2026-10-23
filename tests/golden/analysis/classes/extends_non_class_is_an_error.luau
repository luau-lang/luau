--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

function sqrt(n: number) return n end

class Dog extends sqrt
    function __init(self) end
end
