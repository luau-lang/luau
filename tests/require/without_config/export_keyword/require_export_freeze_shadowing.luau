local m = require("./export_freeze_shadowing")

assert(m.value == 42, "exported value should still be present")
assert(m.which == nil, "local table shadowing should not affect export finalization")

return true
