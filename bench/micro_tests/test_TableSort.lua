local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

local arr_months = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}

local arr_num = {}
for i=1,100 do table.insert(arr_num, math.sin(i)) end

local arr_numk = {}
for i=1,10000 do table.insert(arr_numk, math.sin(i)) end

function test(arr)
    local t = table.create(#arr)

    for i=1,1e6/#arr do
        table.move(arr, 1, #arr, 1, t)
        table.sort(t)
    end
end

bench.runCode(function() test(arr_months) end, "table.sort: 12 strings")
bench.runCode(function() test(arr_num) end, "table.sort: 100 numbers")
bench.runCode(function() test(arr_numk) end, "table.sort: 10k numbers")
