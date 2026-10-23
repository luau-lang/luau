type Optional<T> = T?

type function makeoptional(value)
    return Optional(value)
end

local first: makeoptional<{ a: number }> = { a = 2 }
local second: makeoptional<{ b: number }> = { b = 2 }

assert(first ~= nil and second ~= nil)
