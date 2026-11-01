--!golden fflags=DebugLuauWhileLocalSyntax=true,DebugLuauWhileLocalAnalysis=true

local function f(v: number?)
    while local x = v do
        -- arithmetic requires `x` to have narrowed from `number?` to `number`
        print(x + 1)
    end
end

return f
