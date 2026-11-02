type Info = { id: number }
local signal = {}
function signal.Fire(self, payload: Info) end
signal:Fire(table.freeze({ id = 1 }))
