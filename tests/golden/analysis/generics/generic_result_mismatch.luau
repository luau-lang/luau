local function identity<T>(value: T): T
    return value
end

local value: number = identity("wrong")
print(value)
