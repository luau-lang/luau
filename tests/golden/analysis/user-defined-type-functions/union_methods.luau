type function getunion()
    local value = types.unionof(types.string, types.number, types.boolean)
    if value:is("union") then
        local components = {}
        for _, component in value:components() do
            table.insert(components, component)
        end
        return types.unionof(table.unpack(components))
    end
    return types.number
end

local function _show(value: getunion<>): never
    return value
end
