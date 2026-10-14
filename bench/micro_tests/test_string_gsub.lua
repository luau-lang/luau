local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

local words = {"the", "quick", "brown", "fox", "jumps", "over", "lazy", "dog", "hello", "world"}
local parts = {}
for i = 1, 200 do
	parts[i] = words[i % #words + 1]
end
local prose = table.concat(parts, " ")

local lines = {}
for i = 1, 200 do
	lines[i] = string.rep("x", 40) .. i
end
local text = table.concat(lines, "\n")

local repeated = string.rep("a", 1000)

bench.runCode(function()
	local r
	for i = 1, 10000 do
		r = string.gsub(prose, " ", "")
	end
	assert(#r > 0)
end, "string.gsub: remove spaces")

bench.runCode(function()
	local r
	for i = 1, 1000 do
		r = string.gsub(text, "\n", " ")
	end
	assert(#r == #text)
end, "string.gsub: replace newlines")

bench.runCode(function()
	local r
	for i = 1, 10000 do
		r = string.gsub(prose, "world", "earth")
	end
	assert(#r == #prose)
end, "string.gsub: replace word")

bench.runCode(function()
	local r
	for i = 1, 10000 do
		r = string.gsub(prose, "zz", "y")
	end
	assert(r == prose)
end, "string.gsub: no match")

bench.runCode(function()
	local r
	for i = 1, 10000 do
		r = string.gsub(prose, "fox", string.upper)
	end
	assert(#r == #prose)
end, "string.gsub: function replacement")

bench.runCode(function()
	local r
	for i = 1, 1000000 do
		r = string.gsub("hello_world", "_", " ")
	end
	assert(r == "hello world")
end, "string.gsub: short string")

bench.runCode(function()
	local r
	for i = 1, 10000 do
		r = string.gsub(repeated, "a", "b")
	end
	assert(#r == #repeated)
end, "string.gsub: every character matches")

bench.runCode(function()
	local r
	for i = 1, 10000 do
		r = string.gsub(repeated, "ab", "")
	end
	assert(r == repeated)
end, "string.gsub: separator prefix repeats")
