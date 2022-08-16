local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    local ts0 = os.clock()
    for i=1,1000000 do
        local up = 1
        local function f()
            up = 2
        end
    end
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "Upvalues: dead")