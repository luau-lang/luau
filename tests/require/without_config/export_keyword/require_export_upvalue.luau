-- verify exported functions used as upvalues work correctly
local m = require("./export_upvalue")

assert(m.outer() == 42, "outer should return 42")

local add5 = m.makeAdder(5)
assert(add5(3) == 8, "makeAdder(5)(3) should be 8")
assert(add5(10) == 15, "makeAdder(5)(10) should be 15")

return true
