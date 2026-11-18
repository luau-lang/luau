function f(g: ((true, string) -> ()) & ((false, number) -> ()))
    g(true, 37)
end
