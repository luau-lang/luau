local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

-- searches at every position plus for one missing value, to average the lookup time
local function findSweep(values, missing)
    local needles = table.create(#values + 1)
    for i, v in values do
        needles[i] = v
    end
    needles[#values + 1] = missing

    for i, v in needles do
        assert(table.find(values, v) == (if i <= #values then i else nil))
    end

    local result
    for _=1,10000 do
        for _, v in needles do
            result = table.find(values, v)
        end
    end
    assert(result == nil)
end

local numberValues = table.create(20)
local stringValues = table.create(20)
local vectorValues = table.create(20)
local tableValues = table.create(20)
local userdataValues = table.create(20)

for i=1,20 do
    numberValues[i] = i
    stringValues[i] = "value" .. i
    vectorValues[i] = vector.create(i, i + 1, i + 2)
    tableValues[i] = {}
    userdataValues[i] = newproxy()
end

bench.runCode(function()
    findSweep(numberValues, 0)
end, "TableFind: numbers")

bench.runCode(function()
    findSweep(stringValues, "missing")
end, "TableFind: strings")

bench.runCode(function()
    findSweep(vectorValues, vector.create(0, 0, 0))
end, "TableFind: vectors")

bench.runCode(function()
    findSweep(tableValues, {})
end, "TableFind: tables")

bench.runCode(function()
    findSweep(userdataValues, newproxy())
end, "TableFind: userdata")
