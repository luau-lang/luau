type Dog = { tag: "Dog", howls: boolean }
type Cat = { tag: "Cat", meows: boolean }
type Animal = Dog | Cat
local a : Animal = { tag = "Cat", howls = true }
