--!golden fflags=DebugLuauAnalyzeDumpsCFG=true
local x = 1
while true do
    if x > 0 then
        break
    end
    x = x + 1
end
local w = x
