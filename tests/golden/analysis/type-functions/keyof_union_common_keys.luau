type First = { x: number, y: number, z: number }
type Second = { w: number, y: number, z: number }
type CommonKeys = keyof<First | Second>

local function _err(idx: CommonKeys): "z"
    return idx
end
