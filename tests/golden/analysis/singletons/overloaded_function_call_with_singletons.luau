function f(a, b) end
local g : ((true, string) -> ()) & ((false, number) -> ()) = (f::any)
g(true, "foo")
g(false, 37)
