export type T = "foo" | "bar" | "toto"

local getOpposite: { [number]: T } = {
    ["foo"] = "bar",
    ["bar"] = "toto",
    ["toto"] = "foo"
}
