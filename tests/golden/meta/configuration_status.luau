--!golden status: fail, flags-on.status: ok
--!golden flags-on.runtime.output: /(?m)^export syntax enabled$/

export local message = "export syntax enabled"
print(message)
