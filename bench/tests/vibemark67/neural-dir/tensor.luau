-- Tensor/matrix operations for neural network

local Tensor = {}
Tensor.__index = Tensor

export type Tensor = typeof(setmetatable({} :: {
    data: {number},
    rows: number,
    cols: number,
}, Tensor))

function Tensor.new(rows: number, cols: number): Tensor
    return setmetatable({
        data = table.create(rows * cols, 0),
        rows = rows,
        cols = cols,
    }, Tensor)
end

function Tensor.fromData(rows: number, cols: number, data: {number}): Tensor
    local t = Tensor.new(rows, cols)
    table.move(data, 1, rows * cols, 1, t.data)
    return t
end

function Tensor.get(self: Tensor, row: number, col: number): number
    return self.data[(row - 1) * self.cols + col]
end

function Tensor.set(self: Tensor, row: number, col: number, value: number)
    self.data[(row - 1) * self.cols + col] = value
end

function Tensor.fill(self: Tensor, value: number)
    for i = 1, self.rows * self.cols do
        self.data[i] = value
    end
end

function Tensor.clone(self: Tensor): Tensor
    local t = Tensor.new(self.rows, self.cols)
    table.move(self.data, 1, self.rows * self.cols, 1, t.data)
    return t
end

function Tensor.add(a: Tensor, b: Tensor): Tensor
    local result = Tensor.new(a.rows, a.cols)
    for i = 1, a.rows * a.cols do
        result.data[i] = a.data[i] + b.data[i]
    end
    return result
end

function Tensor.sub(a: Tensor, b: Tensor): Tensor
    local result = Tensor.new(a.rows, a.cols)
    for i = 1, a.rows * a.cols do
        result.data[i] = a.data[i] - b.data[i]
    end
    return result
end

function Tensor.mulScalar(self: Tensor, s: number): Tensor
    local result = Tensor.new(self.rows, self.cols)
    for i = 1, self.rows * self.cols do
        result.data[i] = self.data[i] * s
    end
    return result
end

function Tensor.hadamard(a: Tensor, b: Tensor): Tensor
    local result = Tensor.new(a.rows, a.cols)
    for i = 1, a.rows * a.cols do
        result.data[i] = a.data[i] * b.data[i]
    end
    return result
end

function Tensor.matmul(a: Tensor, b: Tensor): Tensor
    local result = Tensor.new(a.rows, b.cols)
    for i = 1, a.rows do
        for k = 1, a.cols do
            local aik = a.data[(i - 1) * a.cols + k]
            if aik ~= 0 then
                local rowOffset = (i - 1) * b.cols
                local kOffset = (k - 1) * b.cols
                for j = 1, b.cols do
                    result.data[rowOffset + j] += aik * b.data[kOffset + j]
                end
            end
        end
    end
    return result
end

function Tensor.transpose(self: Tensor): Tensor
    local result = Tensor.new(self.cols, self.rows)
    for i = 1, self.rows do
        for j = 1, self.cols do
            result.data[(j - 1) * self.rows + i] = self.data[(i - 1) * self.cols + j]
        end
    end
    return result
end

function Tensor.addBias(self: Tensor, bias: Tensor): Tensor
    local result = self:clone()
    for i = 1, self.rows do
        local offset = (i - 1) * self.cols
        for j = 1, self.cols do
            result.data[offset + j] += bias.data[j]
        end
    end
    return result
end

function Tensor.sumRows(self: Tensor): Tensor
    local result = Tensor.new(1, self.cols)
    for i = 1, self.rows do
        local offset = (i - 1) * self.cols
        for j = 1, self.cols do
            result.data[j] += self.data[offset + j]
        end
    end
    return result
end

function Tensor.apply(self: Tensor, fn: (number) -> number): Tensor
    local result = Tensor.new(self.rows, self.cols)
    for i = 1, self.rows * self.cols do
        result.data[i] = fn(self.data[i])
    end
    return result
end

function Tensor.randomNormal(rows: number, cols: number, seed: number, scale: number): (Tensor, number)
    local t = Tensor.new(rows, cols)
    for i = 1, rows * cols do
        seed = bit32.band(seed * 1103515245 + 12345, 0x7FFFFFFF)
        local u1 = (seed % 10000 + 1) / 10001
        seed = bit32.band(seed * 1103515245 + 12345, 0x7FFFFFFF)
        local u2 = (seed % 10000 + 1) / 10001
        local z = math.sqrt(-2 * math.log(u1)) * math.cos(2 * math.pi * u2)
        t.data[i] = z * scale
    end
    return t, seed
end

function Tensor.meanSquaredError(predicted: Tensor, target: Tensor): number
    local sum = 0
    local n = predicted.rows * predicted.cols
    for i = 1, n do
        local diff = predicted.data[i] - target.data[i]
        sum += diff * diff
    end
    return sum / n
end

return {
    Tensor = Tensor,
}
