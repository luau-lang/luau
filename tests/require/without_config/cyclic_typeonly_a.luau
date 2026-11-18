export type T = number
local B = require("./cyclic_typeonly_b")

if os.clock() >= 0 then
    return 5
end
