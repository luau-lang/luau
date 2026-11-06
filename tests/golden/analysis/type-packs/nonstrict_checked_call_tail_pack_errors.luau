local function two(): (number, number)
    return 1, 2
end
local function three(): (number, number, number)
    return 1, 2, 3
end
local function none(): ()
end

-- an empty pack passes no arguments
math.abs(none())

-- a group truncates a call to a single value
math.abs((none()))
math.clamp((three()))

-- every value of a call in the last argument position is passed
math.abs(3, two())
