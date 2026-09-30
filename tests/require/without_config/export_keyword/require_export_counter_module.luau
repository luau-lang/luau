local m = require("./export_counter_module")

-- we are starting with zeroes
assert(m.counter == 0)
assert(m.get() == 0)

local success, _ = pcall(function() m.set(5) end)
assert(success == false)
local success, _ = pcall(function() m.add(5) end)
assert(success == false)


assert(m.counter == 0, "should be frozen and zero, got " .. tostring(m.counter))
assert(m.get() == 0, "should be frozen and zero, got " .. tostring(m.get()))

return true
