local result = proxyrequire("./dependency", "@"..debug.info(1, "s"))
result[#result+1] = "required into proxy_requirer"
return result
