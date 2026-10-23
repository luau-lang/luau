local function make<T>(): T
    return 0 :: any
end

local value: number = make<<number>>() + 5
assert(value == 5)
