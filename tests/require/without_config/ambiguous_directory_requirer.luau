local result = require("./ambiguous/directory/dependency")
result[#result+1] = "required into module"
return result
