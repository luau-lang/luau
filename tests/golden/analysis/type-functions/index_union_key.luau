type MyObject = { a: string, b: number, c: boolean }
type Present = index<MyObject, "a" | "b">

local function _ok(value: Present): string | number
    return value
end

type PartlyMissing = index<MyObject, "a" | "d">
