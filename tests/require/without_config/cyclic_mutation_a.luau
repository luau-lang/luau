local B = require("./cyclic_mutation_b")
B.foo = "bar" -- B is still loading; its placeholder is locked
return {}
