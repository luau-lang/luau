local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

local words = {"the", "quick", "brown", "fox", "jumps", "over", "lazy", "dog", "hello", "world"}
local parts = {}
for i = 1, 200 do
	parts[i] = words[i % #words + 1]
end
local prose = table.concat(parts, " ")

local mostly = string.rep("the quick brown fox jumps over the lazy dog \xC3\xB1 ", 25)
local cjk = string.rep("\xE6\x97\xA5\xE6\x9C\xAC\xE8\xAA\x9E\xE3\x81\xAE\xE3\x83\x86\xE3\x82\xAD\xE3\x82\xB9\xE3\x83\x88", 50)
local short = "hello"

bench.runCode(function()
	local n
	for i = 1, 10000 do
		n = utf8.len(prose)
	end
	assert(n == #prose)
end, "utf8.len: ascii")

bench.runCode(function()
	local n
	for i = 1, 10000 do
		n = utf8.len(mostly)
	end
	assert(n == 46 * 25)
end, "utf8.len: mostly ascii")

bench.runCode(function()
	local n
	for i = 1, 10000 do
		n = utf8.len(cjk)
	end
	assert(n == 8 * 50)
end, "utf8.len: cjk")

bench.runCode(function()
	local n
	for i = 1, 1000000 do
		n = utf8.len(short)
	end
	assert(n == 5)
end, "utf8.len: short")
