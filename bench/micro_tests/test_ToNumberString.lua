local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

bench.runCode(function()
    for j=1,1e6 do
        tonumber("42")
        tonumber(42)
    end
end, "tonumber")

bench.runCode(function()
    for j=1,1e6 do
        tostring(nil)
        tostring("test")
        tostring(42)
    end
end, "tostring")

bench.runCode(function()
    for j=1,1e6 do
        tostring(j)
    end
end, "tostring-gc")
