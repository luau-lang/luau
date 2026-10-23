--!golden status: ok
--!golden runtime.output: /common: alpha,beta\/gamma/
--!golden flags-on.runtime.output: /(?m)^configuration matrix$/
--!golden flags-off.runtime.output: /(?i)CONFIGURATION MATRIX/

print("common: alpha,beta/gamma")
print("configuration matrix")
