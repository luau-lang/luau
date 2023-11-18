-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
-- This file is based on Lua 5.x tests -- https://github.com/lua/lua/tree/master/testes

-- Generate an error (i.e. throw an exception) inside a tag method which is indirectly
-- called via pcall.
-- This test is meant to detect a regression in handling errors inside a tag method

local testtable = {}
setmetatable(testtable, { __index = function() error("Error") end })

pcall(function()
	testtable.missingmethod()
end)

--
local testtable2 = {}
setmetatable(testtable2, { __index = function() pcall(function() error("Error") end) end })

local m2 = testtable2.missingmethod

pcall(function()
	testtable2.missingmethod()
end)

--
local testtable3 = {}
setmetatable(testtable3, { __index = function() pcall(error, "Error") end })

local m3 = testtable3.missingmethod

pcall(function()
	testtable3.missingmethod()
end)

--
local testtable4 = {}
setmetatable(testtable4, { __index = function() pcall(error) end })

local m4 = testtable4.missingmember

return('OK')
