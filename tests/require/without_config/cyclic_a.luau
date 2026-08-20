local b = require("./cyclic_b")
export local value = "a_value"
export local bRef = b
-- Safe to read b.value here because b is fully loaded by call time.
export function getB()
    return b.value
end
