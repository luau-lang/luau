local M = ...
setmetatable(M, {__index = function(t, k) return "fallback_" .. k end})
local b = require("./cyclic_prev_mt_b")
return M
