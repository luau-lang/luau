type function optionify(tabletype)
    if not tabletype:is("table") then
        error("Argument is not a table")
    end
    for key, property in tabletype:properties() do
        tabletype:setproperty(key, types.unionof(property.read, types.singleton(nil)))
    end
    return tabletype
end

type Person = {
    name: string,
    age: number,
    alive: boolean,
}

local function _show(value: optionify<Person>): nil
    return value
end
