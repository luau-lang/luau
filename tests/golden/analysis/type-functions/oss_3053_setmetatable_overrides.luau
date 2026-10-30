type object0 = {}
type object1 = setmetatable<{} & object0, object0>
type object2 = setmetatable<{} & object1, object1>
type object3 = setmetatable<{} & object2, object2>
type object3_1 = setmetatable<{}, object2>

local function _f()
    local x : object3 = nil :: any
    local o5 = getmetatable(x)

    local y : object2 = nil :: any
    local o4 = getmetatable(y)
    local o3 = getmetatable(o4)
    local o2 = getmetatable(o3)

    local _: _luau_print<typeof(o5)>
    local _: _luau_print<typeof(o4)>
    local _: _luau_print<typeof(o3)>
    local _: _luau_print<typeof(o2)>
end