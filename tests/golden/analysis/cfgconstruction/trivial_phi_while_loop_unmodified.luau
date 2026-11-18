--!golden ok
--!golden fflags=DebugLuauAnalyzeDumpsCFG=true
local x = 1
local iterations = 0
while iterations < 1 do
    local y = x
    iterations = iterations + 1
end
