local a = require("./cyclic_locked_mt_a") -- cyclic; a's placeholder is temporarily invalidated

-- __metatable hides the real error metatable
assert(
    getmetatable(a) == "The metatable is locked",
    "expected getmetatable to return 'The metatable is locked', got: " .. tostring(getmetatable(a))
)

-- __metatable blocks setmetatable from Lua code
local ok, err = pcall(function() setmetatable(a, {}) end)
assert(not ok, "expected setmetatable to error on protected placeholder")
assert(
    err:find("cannot change a protected metatable") ~= nil,
    "expected 'cannot change a protected metatable', got: " .. tostring(err)
)

return {}
