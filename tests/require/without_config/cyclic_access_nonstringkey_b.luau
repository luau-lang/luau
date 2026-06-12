local A = require("./cyclic_access_nonstringkey_a")
local key = {} -- table key; not convertible to string via lua_tostring
local _ = A[key] -- A is still loading; access with non-string key triggers CyclicDependencyError
return {}
