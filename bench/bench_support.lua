-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
local bench = {}

bench.runs = 20
bench.extraRuns = 4

function bench.runCode(f, description)
    -- Under Callgrind, run the test only once and measure just the execution cost
    if callgrind and callgrind("running") then
        if collectgarbage then collectgarbage() end

        callgrind("zero")
        f() -- unfortunately we can't easily separate setup cost from runtime cost in f unless it calls callgrind()
        callgrind("dump", description)
        return
    end

    local timeTable = {}

    for i = 1,bench.runs + bench.extraRuns do
        -- try to run GC if it's available
        if collectgarbage then
            pcall(function()
                collectgarbage()
            end)
        end

        local ts0 = os.clock()

        local result = f()

        local ts1 = os.clock()

        -- If test case doesn't return a duration (if only a part of code is measured) we will measure full execution time here
        if not result then
            result = ts1 - ts0
        end

        table.insert(timeTable, result)
    end

    table.sort(timeTable)

    for i = 1,bench.extraRuns do
        table.remove(timeTable, #timeTable)
    end

    -- Output test name followed by each result
    local report = "|><|"..description

    for _,v in ipairs(timeTable) do
        report = report .. "|><|" .. (v * 1000)
    end

    report = report .. "||_||"

    print(report)
end

-- This function acts a bit like a Unix "fork" operation
-- When it is first called it clones `scriptInstance` and starts executing
-- the cloned script parented to an Actor.  When the cloned script calls "runScriptCodeUnderActor"
-- it will run 'f' and print out the provided 'description'.
--
-- The function returns 'true' if it was invoked from a script running under an Actor
-- and 'false' otherwise.
--
-- Example usage:
--   local function prequire(name) local success, result = pcall(require, name); return success and result end
--   local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")
--   function testFunc()
--      ...
--   end
--   bench.runScriptCodeUnderActor(script, testFunc, "test function")
function bench.runScriptCodeUnderActor(scriptInstance, f, description)
    if scriptInstance:GetActor() then
        -- If this function was called from an Actor script, just run the function provided using runCode
        bench.runCode(f, description)
        return true
    else
        -- If this function was not called from an Actor script, clone the script and place it under
        -- Actor instance.

        -- Create an Actor to run the script under
        local actor = Instance.new("Actor")
        -- Clone this script (i.e. the bench_support module) and place it under the Actor where
        -- the script script would expect it to be when using 'require'.
        local benchModule = script:Clone()
        benchModule.Parent = actor
        -- Clone the scriptInstance
        local actorScript = scriptInstance:Clone()
        -- Enable the script since `scriptInstance` may be started by roblox-cli without ever being enabled.
        actorScript.Disabled = false
        actorScript.Parent = actor
        -- Add the actor to the workspace which will start executing the cloned script.
        -- Note: the script needs to be placed under a instance that implements 'IScriptFilter'
        -- (which workspace does) or it will never start executing.
        actor.Parent = workspace
        return false
    end
end

return bench
