local m = require("./export_multi_swap")

assert(m.a == 2, `a should be 2, got {m.a}`)
assert(m.b == 1, `b should be 1, got {m.b}`)
assert(m.c == 11, `c should be 11, got {m.c}`)

return true
