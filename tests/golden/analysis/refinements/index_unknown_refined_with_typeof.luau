local function readFirst(object: unknown)
    if typeof(object) ~= "table" then
        return nil
    end

    return object[1]
end

return readFirst
