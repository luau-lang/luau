type T = { [string]: number }
local function f(t: T) end
local ro: { read [string]: number } = { score = 5 }
f(ro)
