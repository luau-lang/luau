-- verify mixed exports work correctly
local m = require("./export_mixed")

assert(m.version == "2.0.0", "version should be exported")
assert(m.defaultLabel == "default value", "const value should be exported")

local config: m.Config = m.createConfig(true, 30)
assert(config.debug == true, "config.debug should work")
assert(config.timeout == 30, "config.timeout should work")

return true
