local function make<T>(): T
    return nil :: any
end

local function _add()
    return make<<string>>() + 5
end
