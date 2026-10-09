-- https://github.com/luau-lang/luau/issues/1645

local function _f<t>(world: World<t>)
end

export type World<t> = {
    value: World<t>,
}

local function _g<t>(world: World<t>)
end
