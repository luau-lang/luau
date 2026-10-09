local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

local iterations = 100000

local stringValues = {a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7, h = 8}
local numberValues = {1, 2, 3, 4, 5, 6, 7, 8}
local stringNewindexValues = {a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0}
local numberNewindexValues = {0, 0, 0, 0, 0, 0, 0, 0}
local stringNewindexFuncValues = {a = 0, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0}

local function indexStringValue(_, key) return stringValues[key]end
local function newindexStringValue(_, key, value) stringNewindexFuncValues[key] = value end

local tableStr = setmetatable({}, table.freeze({ __index = stringValues }))
local tableStrFunc = setmetatable({}, table.freeze({ __index = indexStringValue }))
local tableNum = setmetatable({}, table.freeze({ __index = numberValues }))
local tableStrNewindex = setmetatable({}, table.freeze({ __newindex = stringNewindexValues }))
local tableStrNewindexFunc = setmetatable({}, table.freeze({ __newindex = newindexStringValue }))
local tableNumNewindex = setmetatable({}, table.freeze({ __newindex = numberNewindexValues }))

bench.runCode(function()
    local result = 0
    for _=1,iterations do
        result += tableStr.a
        result += tableStr.b
        result += tableStr.c
        result += tableStr.d
        result += tableStr.e
        result += tableStr.f
        result += tableStr.g
        result += tableStr.h
    end
    assert(result == iterations * 36)
end, "MetatableLookup: table string __index")

bench.runCode(function()
    local result = 0
    for _=1,iterations do
        result += tableStrFunc.a
        result += tableStrFunc.b
        result += tableStrFunc.c
        result += tableStrFunc.d
        result += tableStrFunc.e
        result += tableStrFunc.f
        result += tableStrFunc.g
        result += tableStrFunc.h
    end
    assert(result == iterations * 36)
end, "MetatableLookup: table string function __index")

bench.runCode(function()
    local result = 0
    for _=1,iterations do
        result += tableNum[1]
        result += tableNum[2]
        result += tableNum[3]
        result += tableNum[4]
        result += tableNum[5]
        result += tableNum[6]
        result += tableNum[7]
        result += tableNum[8]
    end
    assert(result == iterations * 36)
end, "MetatableLookup: table number __index")

bench.runCode(function()
    for _=1,iterations do
        tableStrNewindex.a = 1
        tableStrNewindex.b = 2
        tableStrNewindex.c = 3
        tableStrNewindex.d = 4
        tableStrNewindex.e = 5
        tableStrNewindex.f = 6
        tableStrNewindex.g = 7
        tableStrNewindex.h = 8
    end
    assert(stringNewindexValues.a == 1 and stringNewindexValues.h == 8)
end, "MetatableLookup: table string __newindex")

bench.runCode(function()
    for _=1,iterations do
        tableStrNewindexFunc.a = 1
        tableStrNewindexFunc.b = 2
        tableStrNewindexFunc.c = 3
        tableStrNewindexFunc.d = 4
        tableStrNewindexFunc.e = 5
        tableStrNewindexFunc.f = 6
        tableStrNewindexFunc.g = 7
        tableStrNewindexFunc.h = 8
    end
    assert(stringNewindexFuncValues.a == 1 and stringNewindexFuncValues.h == 8)
end, "MetatableLookup: table string function __newindex")

bench.runCode(function()
    for _=1,iterations do
        tableNumNewindex[1] = 1
        tableNumNewindex[2] = 2
        tableNumNewindex[3] = 3
        tableNumNewindex[4] = 4
        tableNumNewindex[5] = 5
        tableNumNewindex[6] = 6
        tableNumNewindex[7] = 7
        tableNumNewindex[8] = 8
    end
    assert(numberNewindexValues[1] == 1 and numberNewindexValues[8] == 8)
end, "MetatableLookup: table number __newindex")

local function add(_, rhs) return 10 + rhs end
local function sub(_, rhs) return 10 - rhs end
local function mul(_, rhs) return 10 * rhs end
local function div(_, rhs) return 10 / rhs end

local tableArithmetic = setmetatable({}, table.freeze({ __add = add, __sub = sub, __mul = mul, __div = div }))

bench.runCode(function()
    local result = 0
    for _=1,iterations do
        result += (tableArithmetic + 1)
            + (tableArithmetic - 1)
            + (tableArithmetic * 2)
            + (tableArithmetic / 2)
            + (tableArithmetic + 2)
            + (tableArithmetic - 2)
            + (tableArithmetic * 3)
            + (tableArithmetic / 4)
    end
    assert(result == iterations * 97.5)
end, "MetatableLookup: table arithmetic")

local proxyStr = newproxy(true)
local proxyStrMt = getmetatable(proxyStr)
proxyStrMt.__index = stringValues
table.freeze(proxyStrMt)

local proxyStrFunc = newproxy(true)
local proxyStrFuncMt = getmetatable(proxyStrFunc)
proxyStrFuncMt.__index = indexStringValue
table.freeze(proxyStrFuncMt)

local proxyNum = newproxy(true)
local proxyNumMt = getmetatable(proxyNum)
proxyNumMt.__index = numberValues
table.freeze(proxyNumMt)

local proxyStrNewindex = newproxy(true)
local proxyStrNewindexMt = getmetatable(proxyStrNewindex)
proxyStrNewindexMt.__newindex = stringNewindexValues
table.freeze(proxyStrNewindexMt)

local proxyStrNewindexFunc = newproxy(true)
local proxyStrNewindexFuncMt = getmetatable(proxyStrNewindexFunc)
proxyStrNewindexFuncMt.__newindex = newindexStringValue
table.freeze(proxyStrNewindexFuncMt)

local proxyNumNewindex = newproxy(true)
local proxyNumNewindexMt = getmetatable(proxyNumNewindex)
proxyNumNewindexMt.__newindex = numberNewindexValues
table.freeze(proxyNumNewindexMt)

bench.runCode(function()
    local result = 0
    for _=1,iterations do
        result += proxyStr.a
        result += proxyStr.b
        result += proxyStr.c
        result += proxyStr.d
        result += proxyStr.e
        result += proxyStr.f
        result += proxyStr.g
        result += proxyStr.h
    end
    assert(result == iterations * 36)
end, "MetatableLookup: proxy string __index")

bench.runCode(function()
    local result = 0
    for _=1,iterations do
        result += proxyStrFunc.a
        result += proxyStrFunc.b
        result += proxyStrFunc.c
        result += proxyStrFunc.d
        result += proxyStrFunc.e
        result += proxyStrFunc.f
        result += proxyStrFunc.g
        result += proxyStrFunc.h
    end
    assert(result == iterations * 36)
end, "MetatableLookup: proxy string function __index")

bench.runCode(function()
    local result = 0
    for _=1,iterations do
        result += proxyNum[1]
        result += proxyNum[2]
        result += proxyNum[3]
        result += proxyNum[4]
        result += proxyNum[5]
        result += proxyNum[6]
        result += proxyNum[7]
        result += proxyNum[8]
    end
    assert(result == iterations * 36)
end, "MetatableLookup: proxy number __index")

bench.runCode(function()
    for _=1,iterations do
        proxyStrNewindex.a = 1
        proxyStrNewindex.b = 2
        proxyStrNewindex.c = 3
        proxyStrNewindex.d = 4
        proxyStrNewindex.e = 5
        proxyStrNewindex.f = 6
        proxyStrNewindex.g = 7
        proxyStrNewindex.h = 8
    end
    assert(stringNewindexValues.a == 1 and stringNewindexValues.h == 8)
end, "MetatableLookup: proxy string __newindex")

bench.runCode(function()
    for _=1,iterations do
        proxyStrNewindexFunc.a = 1
        proxyStrNewindexFunc.b = 2
        proxyStrNewindexFunc.c = 3
        proxyStrNewindexFunc.d = 4
        proxyStrNewindexFunc.e = 5
        proxyStrNewindexFunc.f = 6
        proxyStrNewindexFunc.g = 7
        proxyStrNewindexFunc.h = 8
    end
    assert(stringNewindexFuncValues.a == 1 and stringNewindexFuncValues.h == 8)
end, "MetatableLookup: proxy string function __newindex")

bench.runCode(function()
    for _=1,iterations do
        proxyNumNewindex[1] = 1
        proxyNumNewindex[2] = 2
        proxyNumNewindex[3] = 3
        proxyNumNewindex[4] = 4
        proxyNumNewindex[5] = 5
        proxyNumNewindex[6] = 6
        proxyNumNewindex[7] = 7
        proxyNumNewindex[8] = 8
    end
    assert(numberNewindexValues[1] == 1 and numberNewindexValues[8] == 8)
end, "MetatableLookup: proxy number __newindex")

local proxyArithmetic = newproxy(true)
local proxyArithmeticMt = getmetatable(proxyArithmetic)
proxyArithmeticMt.__add = add
proxyArithmeticMt.__sub = sub
proxyArithmeticMt.__mul = mul
proxyArithmeticMt.__div = div
table.freeze(proxyArithmeticMt)

bench.runCode(function()
    local result = 0
    for _=1,iterations do
        result += (proxyArithmetic + 1)
            + (proxyArithmetic - 1)
            + (proxyArithmetic * 2)
            + (proxyArithmetic / 2)
            + (proxyArithmetic + 2)
            + (proxyArithmetic - 2)
            + (proxyArithmetic * 3)
            + (proxyArithmetic / 4)
    end
    assert(result == iterations * 97.5)
end, "MetatableLookup: proxy arithmetic")

local proxyNamecall = newproxy(true)
local proxyNamecallMt = getmetatable(proxyNamecall)
proxyNamecallMt.__namecall = function()
    return 1
end
table.freeze(proxyNamecallMt)

bench.runCode(function()
    local result = 0
    for _=1,iterations do
        result += proxyNamecall:a()
        result += proxyNamecall:b()
        result += proxyNamecall:c()
        result += proxyNamecall:d()
        result += proxyNamecall:e()
        result += proxyNamecall:f()
        result += proxyNamecall:g()
        result += proxyNamecall:h()
    end
    assert(result == iterations * 8)
end, "MetatableLookup: proxy __namecall")
