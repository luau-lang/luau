local M = ...
local a = require("./cyclic_a") -- short-circuits; returns cyclic_a's require table
M.value = "b_value"
M.a = a -- store reference without accessing a's fields (they aren't set yet)
-- Safe to read a.value here because a is fully loaded by call time.
function M.getA()
    return a.value
end
return M
