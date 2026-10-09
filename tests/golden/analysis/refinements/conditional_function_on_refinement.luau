local function printwhenstring(x)
    if typeof(x) == "string" then
        print(string.upper(x))
    end
end

local _: _luau_print<typeof(printwhenstring)>

printwhenstring(42)
printwhenstring("heyheyhey")