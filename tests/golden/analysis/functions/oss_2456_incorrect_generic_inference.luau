type Object = {
    Name: string,
}

type Foo = {
    IsFoo: true,
}

local function duplicate<T>(obj: Object & T): Object & T
    return nil :: any
end

local function doFooThing(obj: Object & Foo)
end

local x: Object & Foo = ...
local y = duplicate(x)
doFooThing(y)