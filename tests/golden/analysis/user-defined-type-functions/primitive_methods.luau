type function getnil()
    local value = types.singleton(nil)
    if value:is("nil") then
        return value
    end
    return types.string
end

type function getnumber()
    local value = types.number
    if value:is("number") then
        return value
    end
    return types.string
end

type function getboolean()
    local value = types.boolean
    if value:is("boolean") then
        return value
    end
    return types.string
end

local function _nil(value: getnil<>): nil
    return value
end

local function _number(value: getnumber<>): number
    return value
end

local function _boolean(value: getboolean<>): boolean
    return value
end
