type function getfunction()
    local value = types.newfunction(nil, nil)
    value:setparameters({ types.string, types.number }, nil)
    value:setreturns(nil, types.boolean)
    if value:is("function") then
        local parameters: { type } = {}
        local head = value:parameters().head
        if head then
            for _, parameter in head do
                table.insert(parameters, parameter)
            end
        end
        return types.newfunction({ head = parameters }, value:returns())
    end
    return types.number
end

local function _show(value: getfunction<>): never
    return value
end
