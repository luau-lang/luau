local m = require("./export_trap")

assert(m.a == m.readA(), `{m.a} not equal {m.readA()}`)
assert(m.b == m.readB(), `{m.b} not equal {m.readB()}`)

return true
