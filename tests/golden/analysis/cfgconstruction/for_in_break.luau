--!golden status: ok
--!golden fflags=DebugLuauAnalyzeDumpsCFG=true
local x = 0
local t = {}
for k, v in t do
    if x > 0 then
        break
    end
    x = x + 1
end
local w = x
