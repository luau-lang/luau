--!golden fflags=DebugLuauWhileLocalSyntax=true,DebugLuauWhileLocalAnalysis=true

local function f(v: string?)
    -- the annotation does not narrow the condition's value: `v` is still `string?` here
    while local x: string = v do
    end
end

return f
