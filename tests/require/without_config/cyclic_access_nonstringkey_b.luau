local A = require("./cyclic_access_nonstringkey_a")
local key = {} -- table key; not convertible to string via lua_tostring
local _ = A[key] -- A's placeholder is locked; triggers CyclicDependencyError with 'unknown' key
export local dummy = true
