local result = require("./nested_inits")
result[#result+1] = "required into module"
return result
