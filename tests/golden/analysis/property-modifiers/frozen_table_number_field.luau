type Info = { id: number }
local function report(payload: Info) end
report(table.freeze({ id = 1 }))
