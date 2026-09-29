type First = { a: string, b: number, c: boolean }
type Second = { a: number }
type A = index<First | Second, "a">

local function _ok(value: A): string | number
    return value
end

type MissingFromSecond = index<First | Second, "b">
