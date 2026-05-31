-- verify returning custom value with exports causes error
-- should fail because value/function exports cannot coexist with explicit return
local success, err = pcall(function()
    local m = require("./export_with_return_error")
end)

assert(not success, "should fail when returning custom value with exports")

return true
