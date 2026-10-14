type T = {
    ["$$bar"] : string,
}
local t: T =  {
    ["$$bar"] = "hi",
}
t["$$bar"] = 5
