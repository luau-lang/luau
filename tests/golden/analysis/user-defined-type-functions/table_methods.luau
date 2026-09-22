type function gettable()
    local indexer = {
        index = types.number,
        readresult = types.boolean,
        writeresult = types.boolean,
    }
    local value = types.newtable(nil, indexer, nil)
    value:setproperty(types.singleton("string"), types.number)
    value:setproperty(types.singleton("number"), types.string)
    value:setproperty(types.singleton("string"), nil)

    local result = types.newtable(nil, nil, nil)
    for key, property in value:properties() do
        result:setreadproperty(key, property.read)
        result:setwriteproperty(key, property.write)
    end
    if result:is("table") then
        result:setindexer(types.boolean, types.string)
        return result
    end
    return types.number
end

local function _show(value: gettable<>): never
    return value
end
