--!golden fflags=DebugLuauAnalyzeDumpsCFG=true
local x = 0
for i = 1, 10 do
    if x > 0 then
        break
    end
    x = x + 1
end
local w = x
