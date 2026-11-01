--!golden status: fail
--!golden fflags=DebugLuauAnalyzeDumpsCFG=true
local f = nil
local x = 1
local y = f(x)
