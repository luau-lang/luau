type function first()
    return "hi"
end

type function second()
    return types.singleton(first())
end

local function _show(value: second<>): nil
    return value
end
