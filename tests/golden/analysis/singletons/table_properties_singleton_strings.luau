type T = {
    ["foo"] : number,
    ["$$bar"] : string,
    baz : boolean
}
local t: T =  {
    ["foo"] = 37,
    ["$$bar"] = "hi",
    baz = true
}
local a: number = t.foo
local b: string = t["$$bar"]
local c: boolean = t.baz
t.foo = 5
t["$$bar"] = "lo"
t.baz = false
