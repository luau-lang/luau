local result = require("./ambiguous/file/dependency")
result[#result+1] = "required into module"
return result
