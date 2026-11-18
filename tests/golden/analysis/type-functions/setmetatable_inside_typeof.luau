--!golden fflags=LuauUnconditionallyVisitTypeAliasParams
local typeDefs: any

type T = typeDefs.sharedCharacter<typeof(setmetatable({}:: typeDefs.client | typeDefs.server, {}))>

return {}