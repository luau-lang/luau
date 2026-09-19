type function getintersection()
    local first = types.newtable(nil, nil, nil)
    first:setproperty(types.singleton("boolean"), types.boolean)
    first:setproperty(types.singleton("number"), types.number)

    local second = types.newtable(nil, nil, nil)
    second:setproperty(types.singleton("boolean"), types.boolean)
    second:setproperty(types.singleton("string"), types.string)

    local value = types.intersectionof(first, second)
    if value:is("intersection") then
        local components = {}
        for _, component in value:components() do
            table.insert(components, component)
        end
        return types.intersectionof(table.unpack(components))
    end
    return types.string
end

local function _show(value: getintersection<>): never
    return value
end
