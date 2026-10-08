-- verify exported values can be required
local m = require("./export_value")

assert(m.version == "1.0.0", "version should be exported")
assert(m.name == "test module", "name should be exported")
assert(m.count == 42, "count should be exported")

return true
