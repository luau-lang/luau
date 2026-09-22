local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

local function lookupRepeated(values, key)
    local result = 0
    for _=1,1000000 do
        result += values[key]
    end
    assert(result == 42000000)
end

local numberKeys = table.create(20)
local stringKeys = table.create(20)
local vectorKeys = table.create(20)
local tableKeys = table.create(20)
local userdataKeys = table.create(20)

local numberLookup = {}
local stringLookup = {}
local vectorLookup = {}
local tableLookup = {}
local userdataLookup = {}

for i=1,20 do
    numberKeys[i] = i + 0.5
    stringKeys[i] = string.format("key%d", i)
    vectorKeys[i] = vector.create(i, i + 1, i + 2)
    tableKeys[i] = {}
    userdataKeys[i] = newproxy()

    numberLookup[numberKeys[i]] = 42
    stringLookup[stringKeys[i]] = 42
    vectorLookup[vectorKeys[i]] = 42
    tableLookup[tableKeys[i]] = 42
    userdataLookup[userdataKeys[i]] = 42
end

local numberKey = numberKeys[15]
local stringKey = stringKeys[15]
local vectorKey = vectorKeys[15]
local tableKey = tableKeys[15]
local userdataKey = userdataKeys[15]

bench.runCode(function()
    lookupRepeated(numberLookup, numberKey)
end, "TableLookup: non-integer number key")

bench.runCode(function()
    lookupRepeated(stringLookup, stringKey)
end, "TableLookup: string key")

bench.runCode(function()
    lookupRepeated(vectorLookup, vectorKey)
end, "TableLookup: vector key")

bench.runCode(function()
    lookupRepeated(tableLookup, tableKey)
end, "TableLookup: table key")

bench.runCode(function()
    lookupRepeated(userdataLookup, userdataKey)
end, "TableLookup: userdata key")
