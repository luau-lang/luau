-- verify mutually recursive functions work
local m = require("./export_mutual_recursion")

assert(m.a() == 43, "a() should call b() and return 43")
assert(m.b() == 42, "b() should return 42")

return true
