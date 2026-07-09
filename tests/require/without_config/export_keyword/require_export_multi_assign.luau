local m = require("./export_multi_assign")

local ok, err = pcall(function()
	m.update()
end)

assert(not ok, "expected update to throw on frozen export")
assert(string.find(err, "attempt to modify a readonly table"), "expected readonly table error but got: " .. tostring(err))
assert(m.readA() == 0, `readA() was {m.readA()}`)
assert(m.readB() == 0, `readB() was {m.readB()}`)

return true
