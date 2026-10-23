type function errors_if_string(argument)
    if argument:is("string") then
        error("We are in a math class! not english")
    end
    return argument
end

local function _show(value: errors_if_string<string>): nil
    return value
end
