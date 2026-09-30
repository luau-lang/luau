local m = require("./export_compound")

assert(m.num == 8, `num should be 8, got {m.num}`)
assert(m.str == "hi there", `str should be 'hi there', got {m.str}`)
assert(m.tbl.count == 1, `tbl.count should be 1, got {m.tbl.count}`)

return true
