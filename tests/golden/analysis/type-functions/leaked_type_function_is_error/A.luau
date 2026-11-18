--!nonstrict
local function unknownValue(): unknown
    return nil
end

local function tag(thing)
    thing.label = "tag-" .. unknownValue()
    return thing
end

export type T = typeof(tag)

return { tag = tag }
