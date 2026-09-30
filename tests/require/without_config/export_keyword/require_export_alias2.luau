local m = require("./export_alias2")

assert(m.a == 1, `a should be 1, got {m.a}`)
assert(m.b == 1, `b should be 1, got {m.b}`)

return true
