type test = "A"
type test2 = "A"|"B"|"C"

local t: { [any]: test } = { A = "A" }

local t2: { [any]: test2 } = {
    A = "A",
    B = "B",
    C = "C"
}
