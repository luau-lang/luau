-- verify internal calls work when module is required
local m = require("./export_internal_call")

assert(m.double(5) == 10, "double should work")
assert(m.internalCall() == 7, "internalCall should call double internally and return 7")

return true
