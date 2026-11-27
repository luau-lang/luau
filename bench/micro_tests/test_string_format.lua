local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

bench.runCode(function()
    for j=1,1e6 do
        local _ = string.format("j=%s", j)
    end
end, "format: %s integer")

bench.runCode(function()
    for j=1,1e6 do
        local _ = string.format("j=%s", j + 0.25)
    end
end, "format: %s float")

bench.runCode(function()
    local ok = "hello!"
    for j=1,1e6 do
        local _ = string.format("j=%s", ok)
    end
end, "format: %s string")

bench.runCode(function()
    for j=1,1e6 do
        local _ = string.format("j=%d", j)
    end
end, "format: %d")

bench.runCode(function()
    for j=1,1e6 do
        local _ = string.format("j=%d", -j * 123457)
    end
end, "format: %d negative large")

bench.runCode(function()
    for j=1,1e6 do
        local _ = string.format("%s: %d/%d", "hp", j, 1000)
    end
end, "format: %s %d %d")

bench.runCode(function()
    for j=1,1e6 do
        local _ = string.format("j=%5d", j)
    end
end, "format: %5d")

bench.runCode(function()
    for j=1,1e6 do
        local _ = string.format("j=%5s", j)
    end
end, "format: %5s integer")

bench.runCode(function()
    for j=1,1e6 do
        local _ = string.format("j=%.2f", j)
    end
end, "format: %.2f")
