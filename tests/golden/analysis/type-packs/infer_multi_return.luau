local function values()
    return 42, "hello"
end

local count: number, message: string = values()
assert(count == 42 and message == "hello")
