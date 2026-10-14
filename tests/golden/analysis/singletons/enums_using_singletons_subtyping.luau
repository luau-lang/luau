type MyEnum1 = "foo" | "bar"
type MyEnum2 = MyEnum1 | "baz"
local a : MyEnum1 = "foo"
local b : MyEnum2 = a
local c : string = b
