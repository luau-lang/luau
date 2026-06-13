local M = ...
local b = require("./cyclic_b")
M.value = "a_value"
M.b = b
-- Safe to read b.value here because b is fully loaded by call time.
function M.getB()
    return b.value
end
return M
