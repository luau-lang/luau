type function optionalize(value: type)
    if value:is("nil") then
        return value
    else
        error("oh no")
    end
end

type function recover(value)
    assert(not pcall(optionalize, value))
    assert(not xpcall(optionalize, function(problem)
        return problem
    end, value))
    return types.unionof(value, types.singleton(nil))
end

local value: recover<number> = 5
assert(value == 5)
