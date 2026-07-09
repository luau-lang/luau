local module = require("./export_class")

local myPoint = module.Point {x = 1, y = 2}

assert(class.isinstance(myPoint, module.Point), "expected myPoint to be an instance of module.Point")
assert(myPoint:getX() == 1, "expected myPoint.x to be 1")
assert(myPoint:getY() == 2, "expected myPoint.y to be 2")

myPoint:setX(3)

assert(myPoint:getX() == 3, "expected myPoint.x to be 3")
assert(myPoint:getY() == 2, "expected myPoint.y to be 2")

local myPoint2 = module.Point {x = 1, y = 2}

assert(class.isinstance(myPoint2, module.Point), "expected myPoint2 to be an instance of module.Point")
assert(myPoint2:getX() == 1, "expected myPoint2.x to be 1")
assert(myPoint2:getY() == 2, "expected myPoint2.y to be 2")

myPoint2:setY(3)

assert(myPoint:dot(myPoint2) == 9, "expected myPoint.dot(myPoint2) to be 9")

local myFactoryFactory = module.FactoryFactory.make()
assert(class.isinstance(myFactoryFactory, module.FactoryFactory), "expected myFactoryFactory to be an instance of module.FactoryFactory")

return true
