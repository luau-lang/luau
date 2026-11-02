type Dog = { tag: "Dog", howls: boolean }
type Cat = { tag: "Cat", meows: boolean }
type Animal = Dog | Cat
local a : Dog = { tag = "Dog", howls = true }
local b : Animal = { tag = "Cat", meows = true }
local c : Animal = a
c = b
