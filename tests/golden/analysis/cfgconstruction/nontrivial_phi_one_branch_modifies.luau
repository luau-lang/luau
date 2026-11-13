--!golden ok
--!golden fflags=DebugLuauAnalyzeDumpsCFG=true
local x = 1
if true then
    x = 2
end
local w = x
