--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

class Foo
end

local x = Foo {}

class Bar extends x
end
