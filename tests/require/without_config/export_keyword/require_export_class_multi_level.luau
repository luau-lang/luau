local module = require("./export_class_multi_level")

-- All three classes should be exported
assert(module.LivingThing ~= nil, "expected LivingThing to be exported")
assert(module.Animal ~= nil, "expected Animal to be exported")
assert(module.Cat ~= nil, "expected Cat to be exported")

-- Grandchild should inherit through both levels
local c = module.Cat {alive = true, species = "Felis catus", name = "Whiskers"}
assert(class.isinstance(c, module.Cat), "expected Cat isinstance")
assert(c:isAlive() == true, "expected grandparent method to work")
assert(c:describe() == "a Felis catus", "expected parent method to work")
assert(c:greet() == "Hi, I'm Whiskers, a Felis catus", "expected child method calling parent method")

-- Middle class should work independently
local a = module.Animal {alive = false, species = "Dodo"}
assert(class.isinstance(a, module.Animal), "expected Animal isinstance")
assert(a:isAlive() == false, "expected grandparent method on middle class")
assert(a:describe() == "a Dodo", "expected own method on middle class")

return true
