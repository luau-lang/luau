local function values(): (number, string)
    return 42, "hello"
end

local count: number, message: boolean = values()
assert(count == 42 and message)
