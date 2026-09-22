type MyObject = { a: string, b: number, c: boolean }
type A = index<MyObject, "a">
type All = index<MyObject, keyof<MyObject>>

local function _ok(value: A): string
    return value
end

local function _all(value: All): string | number | boolean
    return value
end

local function _err(value: A): boolean
    return value
end
