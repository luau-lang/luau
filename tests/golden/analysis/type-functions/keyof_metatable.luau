local metatable = { __index = { w = 1 } }
local obj = setmetatable({ x = 1, y = 2, z = 3 }, metatable)
type KeysOfMyObject = keyof<typeof(obj)>

local function _ok(idx: KeysOfMyObject): "w" | "x" | "y" | "z"
    return idx
end

local function _err(idx: KeysOfMyObject): "x" | "y" | "z"
    return idx
end
