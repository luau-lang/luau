local base = { Foo = "text", Bar = true }
local first = setmetatable({ Foo = 8 }, { __index = base })
local second = setmetatable({ Bar = 5 }, { __index = base })

type FirstFoo = index<typeof(first), "Foo">
type SecondFoo = index<typeof(second), "Foo">
type SecondValues = index<typeof(second), "Foo" | "Bar">

local function _first(value: FirstFoo): number
    return value
end

local function _second(value: SecondFoo): string
    return value
end

local function _values(value: SecondValues): string | number
    return value
end

type Missing = index<typeof(first), "Car">
