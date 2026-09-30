local m = require("./export_post_return_mutation_error")
local ok, err = pcall(function()
    m.increment()
end)
assert(not ok, "expected increment to throw after module returns")
assert(string.find(err, "attempt to modify a readonly table"), "expected readonly table error but got: " .. tostring(err))
return true
