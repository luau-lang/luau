--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true,LuauExportValueSyntax5=true

-- Used by tests that want to use a class that's declared in a different file

export open class Base
    function print_me(self)
        print('Hello World!')
    end
end
