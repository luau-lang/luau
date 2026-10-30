--!golden fflags=DebugLuauWhileLocalSyntax=true,DebugLuauWhileLocalAnalysis=true

local once = true
while local x: { y: number } = once and { y = "oops" } or nil do
    once = false
end
