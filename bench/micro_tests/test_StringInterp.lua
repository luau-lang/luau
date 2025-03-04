local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

bench.runCode(function()
    for j=1,1e6 do
        local _ = "j=" .. tostring(j)
    end
end, "interp: tostring")

bench.runCode(function()
    for j=1,1e6 do
        local _ = "j=" .. j
    end
end, "interp: concat")

bench.runCode(function()
    for j=1,1e6 do
        local _ = string.format("j=%f", j)
    end
end, "interp: %f format")

bench.runCode(function()
    for j=1,1e6 do
        local _ = string.format("j=%d", j)
    end
end, "interp: %d format")

bench.runCode(function()
    for j=1,1e6 do
        local _ = string.format("j=%*", j)
    end
end, "interp: %* format")

bench.runCode(function()
    for j=1,1e6 do
        local _ = `j={j}`
    end
end, "interp: interp number")

bench.runCode(function()
    local ok = "hello!"
    for j=1,1e6 do
        local _ = string.format("j=%s", ok)
    end
end, "interp: %s format")

bench.runCode(function()
	local ok = "hello!"
    for j=1,1e6 do
        local _ = `j={ok}`
    end
end, "interp: interp string")