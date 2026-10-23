--!golden fflags=DebugLuauAnalyzeDumpsCFG=true
local t = 8
if true then
    t = 9
else
    t = "hello"
end
local y = t