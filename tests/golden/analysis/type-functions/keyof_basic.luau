type MyObject = { x: number, y: number, z: number }
type KeysOfMyObject = keyof<MyObject>

local function _ok(idx: KeysOfMyObject): "x" | "y" | "z"
    return idx
end

local function _err(idx: KeysOfMyObject): "x" | "y"
    return idx
end
