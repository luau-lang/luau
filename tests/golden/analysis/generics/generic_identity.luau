local function identity<T>(value: T): T
    return value
end

local numberValue: number = identity(42)
local stringValue: string = identity("hello")
assert(numberValue == 42 and stringValue == "hello")
