local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

local function mmul(matrix1, matrix2)
    local shapeRows = #matrix1
    local shapeColumns = #matrix2[1]
    local result = table.create(shapeRows)
    for i = 1, shapeRows do
        result[i] = table.create(shapeColumns)
        for j = 1, shapeColumns do
            local sum = 0
            for k = 1, shapeColumns do
                sum = sum + matrix1[i][k] * matrix2[k][j]
            end
            result[i][j] = sum
        end
    end
    return result
end

function test()
    local n = 100

    local mat = table.create(n)
    for i = 1, n do
        local t = table.create(n)
        for k = 1, n do
            t[k] = math.random()
        end
        mat[i] = t
    end

    local startTime = os.clock()

    local result = mmul(mat, mat)

    return os.clock() - startTime
end

bench.runCode(test, "matrixmult")
