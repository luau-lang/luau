type A = ("A") -> string
type B = ("B") -> number

local function foo(f: A & B)
    return f("A"), f("B")
end

local _fooType: _luau_print<typeof(foo)> = foo
