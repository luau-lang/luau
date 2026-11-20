--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true
type function a()
    class A end
end

type function b()
    class B end
    return nil
end

type function c()
    local class C end 
end

function _d()
    class D end
end