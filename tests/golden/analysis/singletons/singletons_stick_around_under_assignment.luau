type Foo = {
    kind: "Foo",
}

local foo = ({kind = "Foo"} :: any) :: Foo

print(foo.kind == "Bar") -- type of equality refines to `false`
local kind = foo.kind
print(kind == "Bar") -- type of equality refines to `false`
