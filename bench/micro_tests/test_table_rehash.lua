local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

local iterations = 100000

local function prepareTables(create)
    local tables = table.create(iterations)

    for i=1,iterations do
        tables[i] = create()
    end

    return tables
end

bench.runCode(function()
    local tables = prepareTables(function()
        return table.create(20, 1)
    end)

    local start = os.clock()
    for i=1,iterations do
        local t = tables[i]
        t.a = 1
        t.b = 2
        t.c = 3
        t.d = 4
        t.e = 5
        t.f = 6
        t.g = 7
        t.h = 8
    end
    local elapsed = os.clock() - start

    assert(tables[iterations].h == 8)
    return elapsed
end, "TableRehash: array then string keys")

bench.runCode(function()
    local tables = prepareTables(function()
        return {}
    end)

    local start = os.clock()
    for i=1,iterations do
        local t = tables[i]
        t.a = 1
        t.b = 2
        t.c = 3
        t.d = 4
        t.e = 5
        t.f = 6
        t.g = 7
        t.h = 8
    end
    local elapsed = os.clock() - start

    assert(tables[iterations].h == 8)
    return elapsed
end, "TableRehash: empty then string keys")

bench.runCode(function()
    local tables = prepareTables(function()
        return {}
    end)

    local start = os.clock()
    for i=1,iterations do
        local t = tables[i]
        t[1] = 1
        t[2] = 2
        t[3] = 3
        t[4] = 4
        t[5] = 5
        t[6] = 6
        t[7] = 7
        t[8] = 8
    end
    local elapsed = os.clock() - start

    assert(tables[iterations][8] == 8)
    return elapsed
end, "TableRehash: ordered number keys")

bench.runCode(function()
    local tables = prepareTables(function()
        return {}
    end)

    local start = os.clock()
    for i=1,iterations do
        local t = tables[i]
        t[5] = 5
        t[6] = 6
        t[1] = 1
        t[2] = 2
        t[7] = 7
        t[8] = 8
        t[3] = 3
        t[4] = 4
    end
    local elapsed = os.clock() - start

    assert(tables[iterations][4] == 4)
    return elapsed
end, "TableRehash: shuffled number keys")

bench.runCode(function()
    local tables = prepareTables(function()
        return {a=1, b=2, c=3, d=4, e=5, f=6, g=7, h=8}
    end)

    local start = os.clock()
    for i=1,iterations do
        local t = tables[i]
        t[1] = 1
        t[2] = 2
        t[3] = 3
        t[4] = 4
        t[5] = 5
        t[6] = 6
        t[7] = 7
        t[8] = 8
    end
    local elapsed = os.clock() - start

    assert(tables[iterations].a == 1 and tables[iterations][8] == 8)
    return elapsed
end, "TableRehash: string then number keys")
