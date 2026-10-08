-- verify export table is frozen
local m = require("./export_value")

local success, err = pcall(function()
    m.newKey = "should fail"
end)

assert(not success, "should fail when trying to add to frozen table")

local success2, err2 = pcall(function()
    m.count = 200
end)

assert(not success2, "should fail when trying to modify exported value")

return true
