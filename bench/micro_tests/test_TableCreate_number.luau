local function prequire(name) local success, result = pcall(require, name); return if success then result else nil end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

function test()

    local ts0 = os.clock()
    for i=1,100000 do table.create(100,0) end
    local ts1 = os.clock()

    return ts1-ts0
end

bench.runCode(test, "TableCreate: number")