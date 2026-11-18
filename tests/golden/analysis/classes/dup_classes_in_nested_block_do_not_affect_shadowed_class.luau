--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true
local class Point
    public x: number
end

do
    local class Point
        public y: string
    end
    local _first: number = Point

    local class Point
        public z: boolean
    end
    local _second: number = Point
end

local _after: string = Point
