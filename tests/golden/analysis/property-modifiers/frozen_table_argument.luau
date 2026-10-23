type T = { id: number }
local function f(t: T) return t end
f(table.freeze({ id = 5 }))
