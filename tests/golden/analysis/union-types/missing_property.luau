type Value = { common: number, name: string } | { common: number }

local function _name(value: Value)
    return value.name
end
