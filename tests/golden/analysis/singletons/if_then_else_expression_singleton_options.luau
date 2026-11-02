type Cat = { tag: 'cat', catfood: string }
type Dog = { tag: 'dog', dogfood: string }
type Animal = Cat | Dog

local a: Animal = if true then { tag = 'cat', catfood = 'something' } else { tag = 'dog', dogfood = 'other' }
