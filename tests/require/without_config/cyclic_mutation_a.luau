local B = require("./cyclic_mutation_b")
B.foo = "bar" -- B is still loading; its export table has CyclicDependencyError
return {}
