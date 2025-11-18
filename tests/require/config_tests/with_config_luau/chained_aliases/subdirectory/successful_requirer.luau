local result = {}

table.insert(result, require("@dep"))
table.insert(result, require("@outerdep"))
table.insert(result, require("@outerdir/outer_dependency"))

return result
