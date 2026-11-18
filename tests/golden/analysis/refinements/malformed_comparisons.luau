local function foo(x)
    if x < 0 then
        print("less than zero")
    elseif x > "huh" then
        print("more than 'huh'")
    end
end

local _: _luau_print<typeof(foo)>