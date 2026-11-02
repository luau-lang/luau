type Info = { name: string }
local function report(payload: Info) end
report(table.freeze({ name = "frozen" }))
