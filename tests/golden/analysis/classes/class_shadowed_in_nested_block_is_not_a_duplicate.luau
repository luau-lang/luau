--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true
local class Point
    public x: number
end
local _outer: string = Point

do
    local class Point
        public y: string
    end
    local _inner: number = Point
end

local _after: string = Point
