local function identity<T>(value: T): T
    return value
end

local value = identity<<number, string>>(1)
print(value)
