local a = require("./cyclic_a")
local b = require("./cyclic_b")

assert(type(a) == "table", "expected table from cyclic_a")
assert(type(b) == "table", "expected table from cyclic_b")
assert(a.value == "a_value", "expected a.value == 'a_value'")
assert(b.value == "b_value", "expected b.value == 'b_value'")
assert(a.bRef == b, "expected a.bRef == b (same table reference)")
assert(b.aRef == a, "expected b.aRef == a (same table reference)")
-- Safe: both modules are fully loaded by now.
assert(a.getB() == "b_value", "expected a.getB() == 'b_value'")
assert(b.getA() == "a_value", "expected b.getA() == 'a_value'")

return true
