--!golden fflags=DebugLuauWhileLocalSyntax=true,DebugLuauWhileLocalAnalysis=true

local function f(v: number?)
    while local x: number? = v do
        -- `x` is annotated `number?` but is still narrowed by `truthy` to `number` in the body
        local n: number = x
        print(n)
    end
end

return f
