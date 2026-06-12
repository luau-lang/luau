local A = require("./cyclic_access_a")
local _ = A.Tree -- A is still loading; its export table has CyclicDependencyError
return {}
