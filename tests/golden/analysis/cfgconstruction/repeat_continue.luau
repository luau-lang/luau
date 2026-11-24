--!golden fflags=DebugLuauAnalyzeDumpsCFG=true
local x = 1
repeat
    if x > 0 then
        continue
    end
    x = x + 1
until x
local w = x
