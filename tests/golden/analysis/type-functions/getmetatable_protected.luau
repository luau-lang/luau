local metatable = { __metatable = "protected" }
local value = setmetatable({ x = 1 }, metatable)
type ProtectedMetatable = getmetatable<typeof(value)>

local function _ok(result: ProtectedMetatable): string
    return result
end
