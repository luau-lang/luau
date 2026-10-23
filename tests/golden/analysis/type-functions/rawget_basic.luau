type MyObject = { a: string, b: number, c: boolean }
type A = rawget<MyObject, "a">
type All = rawget<MyObject, keyof<MyObject>>

local function _ok(value: A): string
    return value
end

local function _all(value: All): string | number | boolean
    return value
end

local function _err(value: A): boolean
    return value
end
