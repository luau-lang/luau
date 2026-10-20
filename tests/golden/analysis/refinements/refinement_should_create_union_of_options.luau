-- NOTE: we currently output a *hideous* type for `foobar`. We could do better
-- but at least the type is correct.
local function foobar(x)
    if typeof(x) == "string" then
        string.len(x)
    else
        math.abs(x)
    end
end

foobar("hmmm") -- Should be OK
foobar(42) -- Should also be OK
foobar(true) -- Should never be OK!