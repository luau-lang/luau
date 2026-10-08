-- verify nested table behavior when required
local m = require("./export_nested_table")

assert(m.triangle.name == "triangle", "table contents should be accessible")
assert(m.triangle.sides == 3, "table contents should be accessible")
assert(m.triangle.draw() == "drawing triangle", "methods on table should work")

return true
