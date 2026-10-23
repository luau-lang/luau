--!golden ok
--!golden runtime.output: /multifile helper loaded/

local helper = require("@self/helper")

assert(helper.answer == 42)
print(helper.message)
