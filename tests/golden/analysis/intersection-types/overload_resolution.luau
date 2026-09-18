type Overloaded = ((number) -> number) & ((string) -> string)
local identity = (function(value: any): any
    return value
end) :: Overloaded

local numberValue: number = identity(42)
local stringValue: string = identity("hello")
assert(numberValue == 42 and stringValue == "hello")
