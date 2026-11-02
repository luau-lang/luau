local function two(): (number, number)
    return 1, 2
end
local function three(): (number, number, number)
    return 1, 2, 3
end
local function none(): ()
end
local function strings(): ...string
    return "a"
end
local function numberThenStrings(): (number, ...string)
    return 1, "b"
end

-- a call in the last argument position passes all of its values
math.clamp(three())
math.clamp(1, two())

-- an empty pack doesn't add an argument
math.abs(3, none())

local function _forward(...: any)
    -- while ... can have <3 values, we don't want to complain when it has >=3 for nonstrict
    math.clamp(...)

    -- while ... can have values, we don't want to complain when it has 0 for nonstrict
    math.abs(3, ...)
end

local function _generic<T...>(...: T...)
    math.abs(...)
end

-- while ...string has a value here, from types alone the pack could be empty
math.abs(strings())
math.clamp(numberThenStrings())
