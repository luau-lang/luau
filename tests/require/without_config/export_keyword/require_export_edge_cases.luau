local m = require("./export_edge_cases")

assert(m.x == 6, `x should be 6, got {m.x}`)
assert(m.s == "ab", `s should be ab, got {m.s}`)
assert(m.n == 42, `n should be 42, got {m.n}`)
assert(m.step == 3, `step should be 3, got {m.step}`)
assert(m.sum == 6, `sum should be 6, got {m.sum}`)
assert(m.prod == 24, `prod should be 24, got {m.prod}`)
assert(m.t.x == 2, `t.x should be 2, got {m.t.x}`)
assert(m.a == 2, `a should be 2, got {m.a}`)
assert(m.closure == 1, `closure should be 1, got {m.closure}`)
assert(m.fw() == "forward", `fw() should return forward`)

return true
