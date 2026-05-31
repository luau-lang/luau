-- verify type-only exports can coexist with explicit return
local m = require("./export_type_with_return")

assert(m.value == 42, "explicit return should work with type-only exports")

local p: m.Point = { x = 1, y = 2 }
assert(p.x == 1, "type should be exported")

return true
