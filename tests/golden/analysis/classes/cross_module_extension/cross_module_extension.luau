--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true,LuauExportValueSyntax5=true,LuauExportValueTypecheck2=true

const M = require("@self/exports_a_class")

class Derived extends M.Base
    function print_me_twice(self)
        self:print_me()
        self:print_me()
    end
end

const d = Derived {}
d:print_me_twice()
