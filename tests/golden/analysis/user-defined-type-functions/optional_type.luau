type function optionalnumber()
    return types.optional(types.number)
end

local function _show(value: optionalnumber<>): never
    return value
end
