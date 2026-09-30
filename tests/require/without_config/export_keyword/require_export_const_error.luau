-- verify reassigning exported value causes error
local success, err = pcall(function()
    local m = require("./export_const_error")
end)

assert(not success, "should fail when reassigning exported value")

return true
