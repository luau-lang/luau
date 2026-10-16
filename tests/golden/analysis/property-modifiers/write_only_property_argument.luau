type T = { count: number }
local function f(t: T) end
local wo: { write count: number } = { count = 5 }
f(wo)
