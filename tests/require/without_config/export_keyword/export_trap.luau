export local a = 0
export local b = 0

local trap = table.freeze({})

pcall(function()
    a, trap.x, b = 1, 2, 3
end)

export function readA() return a end
export function readB() return b end
