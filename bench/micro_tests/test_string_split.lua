local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

local csv = "alpha,beta,gamma,delta,epsilon,zeta,eta,theta"

local lines = {}
for i = 1, 2000 do
	lines[i] = string.rep("x", 40 + i % 40) .. i
end
local text = table.concat(lines, "\n")
local textMulti = table.concat(lines, "\r\n")

local repeated = string.rep("a", 100000)

bench.runCode(function()
	local r
	for i = 1, 200000 do
		r = string.split(csv, ",")
	end
	assert(#r == 8)
end, "string.split: short csv")

bench.runCode(function()
	local r
	for i = 1, 100 do
		r = string.split(text, "\n")
	end
	assert(#r == 2000)
end, "string.split: long lines")

bench.runCode(function()
	local r
	for i = 1, 100 do
		r = string.split(textMulti, "\r\n")
	end
	assert(#r == 2000)
end, "string.split: long lines, 2-char separator")

bench.runCode(function()
	local r
	for i = 1, 20000 do
		r = string.split(csv, "")
	end
	assert(#r == #csv)
end, "string.split: empty separator")

bench.runCode(function()
	local r
	for i = 1, 100 do
		r = string.split(repeated, "aab")
	end
	assert(#r == 1)
end, "string.split: separator prefix repeats")

bench.runCode(function()
	local r
	for i = 1, 100 do
		r = string.split(repeated, "aaba")
	end
	assert(#r == 1)
end, "string.split: separator prefix and suffix repeat")
