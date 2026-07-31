local module = require("./export_class_child_without_parent")

local myCat = module.Cat {species = "Felis catus", breed = "Siamese"}

assert(class.isinstance(myCat, module.Cat), "expected myCat to be an instance of module.Cat")
assert(myCat.species == "Felis catus", "expected myCat.species to be 'Felis catus'")
assert(myCat.breed == "Siamese", "expected myCat.breed to be 'Siamese'")
assert(myCat:live() == "I am alive", "expected inherited method to work")
assert(myCat:describe() == "I am a Siamese Felis catus", "expected child method to work")

-- Animal should not be accessible from the module since it's not exported
assert(module.Animal == nil, "expected Animal to not be exported")

return true
