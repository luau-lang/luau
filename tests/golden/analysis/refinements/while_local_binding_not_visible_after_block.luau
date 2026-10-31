--!golden fflags=DebugLuauWhileLocalSyntax=true,DebugLuauWhileLocalAnalysis=true

local n = 0
while local x = n < 3 do
    n += 1
    -- `x` is narrowed to `true` in the body, so this would fail if it were `boolean`
    local t: true = x
    print(t)
end
print(x)
