local A = require("./cyclic_access_a")
local _ = A.Tree -- A is still loading; its placeholder is locked
return {}
