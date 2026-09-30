-- verify exported functions can be required and called
local m = require("./export_function")

assert(m.add(1, 2) == 3, "add function should work")
assert(m.add(10, 20) == 30, "add function should work with larger numbers")
assert(m.greet("World") == "Hello, World", "greet function should work")
assert(m.noop() == nil, "noop function should return nil")

return true
