local function prequire(name) local success, result = pcall(require, name); if success then return result end return nil end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

function fma(a: vector, b: vector, c: vector)
    return a * b + c
end

function approx(a: vector): vector
    local r = vector.create(1, 1, 1)
    local aa = a
    r += aa * 0.123
    aa *= a
    r += aa * 0.123
    aa *= a
    r += aa * 0.123
    aa *= a
    r += aa * 0.123
    aa *= a
    r += aa * 0.123
    aa *= a
    r += aa * 0.123
    return r
end

function test()
    local A = vector.create(1, 2, 3)
    local B = vector.create(4, 5, 6)
    local C = vector.create(7, 8, 9)
    local fma = fma
    local approx = approx

    for i=1,100000 do
        fma(A, B, C)

        approx(A)
    end
end

bench.runCode(test, "vector-math")
