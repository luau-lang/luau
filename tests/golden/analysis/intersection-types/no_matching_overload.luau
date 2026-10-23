type Overloaded = ((number) -> number) & ((string) -> string)

local function _call(identity: Overloaded)
    return identity(true)
end
