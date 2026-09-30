local function readFirst(object: unknown)
    if type(object) ~= "table" then
        return nil
    end

    return object[1]
end

return readFirst
