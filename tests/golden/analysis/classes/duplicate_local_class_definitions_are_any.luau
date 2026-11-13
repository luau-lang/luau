--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true
local class Point
    public x: number
end
local _first: string = Point

local class Point
    public y: string
end
local _second: number = Point
