type MyObject = { x: number, y: number, z: number }
type KeysOfMyObject = keyof<MyObject | boolean>

local function _err(idx: KeysOfMyObject): "x" | "y" | "z"
    return idx
end
