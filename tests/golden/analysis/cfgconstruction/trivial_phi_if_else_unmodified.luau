--!golden ok
--!golden fflags=DebugLuauAnalyzeDumpsCFG=true
local x = 1
if true then
    local y = 2
else
    local z = 3
end
local w = x
