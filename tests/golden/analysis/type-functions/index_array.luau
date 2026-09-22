local values = { "hello", 1, true }
type Element = index<typeof(values), number>

local function _ok(value: Element): string | number | boolean
    return value
end
