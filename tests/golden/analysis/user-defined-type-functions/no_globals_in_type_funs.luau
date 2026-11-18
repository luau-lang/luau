--!golden fflags=LuauBanGlobalsInTypeFunctions=true
type function f() end
type function g()
    function f()
    end
end

Foobar = "yo"
type function huh()
    print(Foobar)
end

