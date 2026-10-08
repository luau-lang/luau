--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

open class Point
    function __eq(self, other: Point): boolean
        return true
    end
end

class NamedPoint extends Point
    function __eq(self, other: NamedPoint): boolean
        return true
    end
end
