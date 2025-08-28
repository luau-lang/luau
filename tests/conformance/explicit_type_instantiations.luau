-- Tests to ensure explicit type instantiations don't change runtime behavior
local function identity<T>(x: T): T
    return x
end

assert(identity<<number>>(1) == 1)

local function multipleReturns<T>(x: T): (T, T)
    return x, x
end

local a, b = multipleReturns<<number>>(1)
assert(a == 1 and b == 1)

local function typePacks<T...>(...: T...): T...
    return ...
end

local a, b = typePacks<<(string, number)>>(1, "a")
assert(a == 1 and b == "a")

local t = {}
function t:method<T>(x: T): T
    assert(self == t)
    return x
end

assert(t:method(1) == 1)

function t:methodTypePacks<T...>(...: T...): T...
    assert(self == t)
    return ...
end

local a, b = t:methodTypePacks<<(string, number)>>(1, "a")
assert(a == 1 and b == "a")

return "OK"
