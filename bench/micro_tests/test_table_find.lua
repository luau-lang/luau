local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

local small = {}
for i = 1, 10 do
	small[i] = i
end

local names = {}
for i = 1, 100 do
	names[i] = "Player" .. i
end

local objects = {}
for i = 1, 100 do
	objects[i] = { id = i }
end

local numbers = {}
for i = 1, 1000 do
	numbers[i] = i
end

local eqmt = { __eq = function(a, b) return a.id == b.id end }
local eqobjects = {}
for i = 1, 100 do
	eqobjects[i] = setmetatable({ id = i }, eqmt)
end

local empty = {}

bench.runCode(function()
	local r
	for i = 1, 1000000 do
		r = table.find(small, 7)
	end
	assert(r == 7)
end, "table.find: 10 numbers")

bench.runCode(function()
	local r
	for i = 1, 100000 do
		r = table.find(names, "Player90")
	end
	assert(r == 90)
end, "table.find: 100 strings")

bench.runCode(function()
	local r
	local target = objects[90]
	for i = 1, 100000 do
		r = table.find(objects, target)
	end
	assert(r == 90)
end, "table.find: 100 tables")

bench.runCode(function()
	local r
	for i = 1, 10000 do
		r = table.find(numbers, 0)
	end
	assert(r == nil)
end, "table.find: 1000 numbers, no match")

bench.runCode(function()
	local r
	for i = 1, 1000000 do
		r = table.find(names, "Player1")
	end
	assert(r == 1)
end, "table.find: first element")

bench.runCode(function()
	local r
	local target = setmetatable({ id = 90 }, eqmt)
	for i = 1, 10000 do
		r = table.find(eqobjects, target)
	end
	assert(r == 90)
end, "table.find: 100 tables with __eq")

bench.runCode(function()
	local r
	for i = 1, 1000000 do
		r = table.find(empty, 1)
	end
	assert(r == nil)
end, "table.find: empty table")
