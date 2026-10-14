export type T = "foo" | "bar" | "toto"

local object: T = "foo"

local getOpposite: {[T]: T} = {
    ["foo"] = "bar",
    ["bar"] = "toto",
    ["toto"] = "foo"
}

local function hello()
    local x = getOpposite[object]

    if x then
        object = x
    end
end
