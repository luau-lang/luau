-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This tests that the lua_*upval() APIs work correctly even with debug info disabled
local foo = 5
function clo_test()
    -- so `foo` gets captured as an upval
    print(foo)
    -- yield so we can look at clo_test's upvalues
    coroutine.yield()
end

clo_test()

return 'OK'
