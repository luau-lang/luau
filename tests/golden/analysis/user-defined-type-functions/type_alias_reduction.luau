type Property<T> = rawget<T, "a">

type function propertytype(value)
    return Property(value)
end

local first: propertytype<{ a: number }> = 2
local second: propertytype<{ a: string }> = "value"

assert(first == 2 and second == "value")
