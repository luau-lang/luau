local a: string = "hi"
if a == "hi" or a == "bye" then
    local _aType: _luau_print<typeof(a)> = a
    local x = #a
end
