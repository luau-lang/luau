local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

local function setRepeated(values, key)
    for i=1,1000000 do
        values[key] = i
    end
    assert(values[key] == 1000000)
end

local numberKeys = table.create(20)
local roundKeys = table.create(20)
local stringKeys = table.create(20)
local vectorKeys = table.create(20)
local tableKeys = table.create(20)
local userdataKeys = table.create(20)

local numberValues = {}
local roundValues = {}
local stringValues = {}
local vectorValues = {}
local tableValues = {}
local userdataValues = {}

for i=1,20 do
    numberKeys[i] = i + 0.5
    roundKeys[i] = i * 1000
    stringKeys[i] = string.format("key%d", i)
    vectorKeys[i] = vector.create(i, i + 1, i + 2)
    tableKeys[i] = {}
    userdataKeys[i] = newproxy()

    numberValues[numberKeys[i]] = 0
    roundValues[roundKeys[i]] = 0
    stringValues[stringKeys[i]] = 0
    vectorValues[vectorKeys[i]] = 0
    tableValues[tableKeys[i]] = 0
    userdataValues[userdataKeys[i]] = 0
end

local numberKey = numberKeys[15]
local roundKey = roundKeys[15]
local stringKey = stringKeys[15]
local vectorKey = vectorKeys[15]
local tableKey = tableKeys[15]
local userdataKey = userdataKeys[15]

bench.runCode(function()
    setRepeated(numberValues, numberKey)
end, "TableSet: non-integer number key")

bench.runCode(function()
    setRepeated(roundValues, roundKey)
end, "TableSet: integer number key")

bench.runCode(function()
    setRepeated(stringValues, stringKey)
end, "TableSet: string key")

bench.runCode(function()
    setRepeated(vectorValues, vectorKey)
end, "TableSet: vector key")

bench.runCode(function()
    setRepeated(tableValues, tableKey)
end, "TableSet: table key")

bench.runCode(function()
    setRepeated(userdataValues, userdataKey)
end, "TableSet: userdata key")
