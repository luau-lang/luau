-- https://github.com/luau-lang/luau/issues/1646

local function _check()
    type nset = (component_1, component_1) -> () &
    (component_2, component_2) -> ()
    local nset: nset = function(_a: any, _b: any) end
    type component_1 = string
    type component_2 = number

    local c1: string = "value"
    nset(c1, 1)
end
