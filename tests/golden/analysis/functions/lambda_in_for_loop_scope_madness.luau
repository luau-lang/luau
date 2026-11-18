local function call1(f: () -> number): { number }
    return { f() }
end

local function outer1(): string
    for _, v in call1(function() return 1 end) do
        return tostring(v)
    end
    return "hello"
end

local function call2(f: () -> number): number
    return f()
end

local function outer2(): string
    for i=1,call2(function() return 10 end) do
        return tostring(i)
    end
    return "world"
end

print(outer1())
print(outer2())