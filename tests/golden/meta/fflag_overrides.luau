--!golden ok
--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

class Point
    public x: number
end

local point = Point({ x = 42 })
assert(point.x == 42)
