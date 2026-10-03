--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true
open class A extends B
end

open class B extends C
end

open class C extends A
end
