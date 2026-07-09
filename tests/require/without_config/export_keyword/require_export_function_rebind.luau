local m = require("./export_function_rebind")

pcall(function() m.rebind(5) end)

assert(m.foo() == "original", `direct call got {m.foo()}`)
assert(m.call_foo() == "original", `inner call got {m.call_foo()}`)

return true
