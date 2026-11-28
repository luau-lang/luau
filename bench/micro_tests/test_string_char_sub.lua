local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

local bytes = {}
for i = 0, 255 do
    bytes[i + 1] = string.char(i)
end
local s = table.concat(bytes)

bench.runCode(function()
    local last
    for _ = 1, 4000 do
        for c = 0, 255 do
            last = string.char(c)
        end
    end
    assert(last == bytes[256])
end, "string.char: one byte")

bench.runCode(function()
    local last
    for _ = 1, 4000 do
        for i = 1, #s do
            last = s:sub(i, i)
        end
    end
    assert(last == bytes[256])
end, "string.sub: one byte")
