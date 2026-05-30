local m = require("./export_forward_rebind")

assert(m.recursive(5) == 120, `recursive(5) should be 120`)
assert(m.mut() == "function-stat", `mut() should be function-stat`)

local ok, err = pcall(function() m.rebindMut() end)
assert(not ok, "rebindMut should throw on frozen export")
assert(m.mut() == "function-stat", `mut() should still be function-stat after failed rebind`)

return true
