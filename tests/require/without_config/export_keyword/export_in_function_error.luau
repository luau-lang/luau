-- export inside function body should error
local function test()
    export local inside = "function"
end

test()
