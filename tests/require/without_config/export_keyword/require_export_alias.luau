local m = require("./export_alias")

assert(m.alias == 42, `alias should be 42, got {m.alias}`)

return true
