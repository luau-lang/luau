local metatable = { __index = { w = 1 } }
local obj = setmetatable({ x = 1, y = 2, z = 3 }, metatable)
type RawKeys = rawkeyof<typeof(obj)>

local function _ok(idx: RawKeys): "x" | "y" | "z"
    return idx
end

local function _err(idx: RawKeys): "x" | "y"
    return idx
end
