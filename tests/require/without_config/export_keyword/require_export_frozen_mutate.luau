local m = require("./export_frozen_mutate")

assert(m.counter == 10)
assert(m.text == "hello")

local ok1, err1 = pcall(function() m.setCounter(99) end)
assert(not ok1, "setCounter should throw on frozen export")

local ok2, err2 = pcall(function() m.addCounter(5) end)
assert(not ok2, "addCounter should throw on frozen export")

local ok3, err3 = pcall(function() m.concatText("world") end)
assert(not ok3, "concatText should throw on frozen export")

assert(m.counter == 10, `counter should still be 10, got {m.counter}`)
assert(m.text == "hello", `text should still be hello, got {m.text}`)

return true
