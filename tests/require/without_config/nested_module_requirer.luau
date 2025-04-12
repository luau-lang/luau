local result = require("./nested")
result[#result+1] = "required into module"
return result
