local result = require("./dependency")
result[#result+1] = "required into requirer.luau"
return result
