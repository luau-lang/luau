local t1: { f: ((x: number) -> number)? } = {}

function t1.f(x)
    print(x + 5)
    return x .. "asd"
end

t1.f = function(x)
    print(x + 5)
    return x .. "asd"
end

local t2 = { f = nil :: ((x: number) -> number)? }

function t2.f(x: string): string
    return x .. "asd"
end

t2.f = function(x)
    print(x + 5)
    return x .. "asd"
end