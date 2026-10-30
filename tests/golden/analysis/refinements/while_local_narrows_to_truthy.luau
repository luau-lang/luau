--!golden fflags=DebugLuauWhileLocalSyntax=true,DebugLuauWhileLocalAnalysis=true

local function f(v: string?)
    while local x = v do
        -- `x` must have narrowed from `string?` to `string` to satisfy this annotation
        local s: string = x
        print(s)
    end
end

return f
