local function prequire(name) local success, result = pcall(require, name); return if success then result else nil end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

function test()

    function test() coroutine.yield() return 1 end

    local ts0 = os.clock()
    for i=0,100000 do
        local co = coroutine.create(function() return pcall(test) end)
        coroutine.resume(co)
        coroutine.resume(co)
    end
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "Pcall: pcall yield")