--!golden fail
--!golden strict.output: /TypeError: Expected this to be 'number', but got 'string'/

local value: number = "text"
print(value)
