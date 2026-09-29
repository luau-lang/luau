type MyObject = { a: string }
type Missing = rawget<MyObject, "b">

local function _ok(value: Missing): nil
    return value
end
