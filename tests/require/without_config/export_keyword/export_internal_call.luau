-- calling exported functions internally
export function double(n: number): number
    return n * 2
end

local result = double(5)
assert(result == 10, "exported function should be callable internally")

export function internalCall()
    local x = double(3)
    return x + 1
end
