-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

-- Generate an error (i.e. throw an exception) inside a tag method which is indirectly called via pcall.
-- This test is meant to detect a regression in handling errors inside a tag method

local status, result

local testtable = {}
setmetatable(testtable, {
    __index = function()
        error("Error")
    end
})

status, result = pcall(function()
    testtable.missingmethod()
end)
assert(status == false)
assert(string.find(result, ":11: Error") ~= nil)

--
local testtable2 = {}
setmetatable(testtable2, {
    __index = function()
        local status, result = pcall(function()
            error("Error")
        end)
        assert(status == false)
        assert(string.find(result, ":26: Error") ~= nil)
        return nil
    end
})

local m2 = testtable2.missingmethod

status, result = pcall(function()
    testtable2.missingmethod()
end)
assert(status == false)
assert(string.find(result, ":37: attempt to call a nil value") ~= nil)

--
local testtable3 = {}
setmetatable(testtable3, { __index = function()
    local status, result = pcall(error, "Error")
    assert(status == false)
    assert(result == "Error")
    return nil
end })

local m3 = testtable3.missingmethod

status, result = pcall(function()
    testtable3.missingmethod()
end)
assert(status == false)
assert(string.find(result, ":54: attempt to call a nil value") ~= nil)

--
local testtable4 = {}
setmetatable(testtable4, {
    __index = function()
        local status, result = pcall(error)
        assert(status == false)
        assert(result == nil)
        return nil
    end
})

local m4 = testtable4.missingmember

status, result = pcall(function()
    testtable4.missingmethod()
end)
assert(status == false)
assert(string.find(result, ":73: attempt to call a nil value") ~= nil)

return('OK')
