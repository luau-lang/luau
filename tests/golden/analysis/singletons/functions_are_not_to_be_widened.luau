local function foo(my_enum: "A" | "B") end

local _fooType: _luau_print<typeof(foo)> = foo
