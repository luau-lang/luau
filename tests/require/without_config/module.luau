local result = require("./dependency")
result[#result+1] = "required into module"
return result
