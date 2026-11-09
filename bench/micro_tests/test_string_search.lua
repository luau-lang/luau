local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

local words = {"the", "quick", "brown", "fox", "jumps", "over", "lazy", "dog", "hello", "world"}
local parts = {}
for i = 1, 2000 do
	parts[i] = words[i % #words + 1]
end
local prose = table.concat(parts, " ")

local zeros = string.rep("0", 100000)
local aaa = string.rep("a", 100000)
local needle = string.rep("a", 500) .. "b" .. string.rep("a", 499)

bench.runCode(function()
	local r
	for i = 1, 10000 do
		r = string.find(prose, "lazy cat", 1, true)
	end
	assert(r == nil)
end, "string.find: prose miss")

bench.runCode(function()
	local r
	for i = 1, 1000 do
		r = string.gsub(prose, "fox jumps", "cat")
	end
	assert(#r > 0)
end, "string.gsub: prose")

bench.runCode(function()
	local r
	for i = 1, 1000 do
		r = string.split(prose, "dog hello")
	end
	assert(#r > 1)
end, "string.split: prose")

bench.runCode(function()
	local r
	for i = 1, 100 do
		r = string.find(zeros, string.rep("0", 63) .. "1", 1, true)
	end
	assert(r == nil)
end, "string.find: zero padding")

bench.runCode(function()
	local r
	for i = 1, 10 do
		r = string.find(aaa, needle, 1, true)
	end
	assert(r == nil)
end, "string.find: repetitive")

bench.runCode(function()
	local r
	for i = 1, 10 do
		r = string.gsub(aaa, needle, "")
	end
	assert(#r == #aaa)
end, "string.gsub: repetitive")

bench.runCode(function()
	local r
	for i = 1, 10 do
		r = string.split(aaa, needle)
	end
	assert(#r == 1)
end, "string.split: repetitive")

local big = string.rep("a", 200000)

bench.runCode(function()
	local r = string.find(big, string.rep("a", 4999) .. "b", 1, true)
	assert(r == nil)
end, "string.find: long needle")

bench.runCode(function()
	local r = string.gsub(big, string.rep("a", 2500) .. "b" .. string.rep("a", 2499), "")
	assert(#r == #big)
end, "string.gsub: long needle")
