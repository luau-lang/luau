local a = require("./cyclic_prev_mt_a")

-- After both modules load, a's original metatable (__index fallback) should be active.
assert(a.unset == "fallback_unset", "expected __index fallback after metatable restore, got: " .. tostring(a.unset))

return true
