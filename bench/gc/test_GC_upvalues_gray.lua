local bench = script and require(script.Parent.bench_support) or require("bench_support")

function test()

    local function makeup()
        local up = 1
        local function f()
            up = 2
        end
        coroutine.yield()
    end

    local ts0 = os.clock()
    for i=1,100000 do
        local co = coroutine.create(makeup)
        coroutine.resume(co)
    end
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "Upvalues: gray")