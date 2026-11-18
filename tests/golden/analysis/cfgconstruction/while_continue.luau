--!golden fflags=DebugLuauAnalyzeDumpsCFG=true
local x = 1
while x > 0 do
    if x > 0 then
        x = 0
        continue
    end
    x = x + 1
end
local w = x
