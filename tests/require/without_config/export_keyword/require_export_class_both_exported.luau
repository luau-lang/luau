local module = require("./export_class_both_exported")

-- Both Animal and Cat should be accessible
assert(module.Animal ~= nil, "expected Animal to be exported")
assert(module.Cat ~= nil, "expected Cat to be exported")

-- Create instances of both
local a = module.Animal {species = "Canis lupus"}
assert(a.species == "Canis lupus", "expected a.species")
assert(a:live() == "I am alive", "expected Animal.live to work")

local c = module.Cat {species = "Felis catus", breed = "Siamese"}
assert(c.species == "Felis catus", "expected c.species inherited from Animal")
assert(c.breed == "Siamese", "expected c.breed")
assert(c:live() == "I am alive", "expected inherited method to work")
assert(c:describe() == "I am a Siamese Felis catus", "expected child method to work")

return true
