-- Activation functions and their derivatives

local tensorMod = require("./tensor")
local Tensor = tensorMod.Tensor

local function relu(x: number): number
    return if x > 0 then x else 0
end

local function reluDeriv(x: number): number
    return if x > 0 then 1 else 0
end

local function sigmoid(x: number): number
    if x > 20 then return 1 end
    if x < -20 then return 0 end
    return 1 / (1 + math.exp(-x))
end

local function sigmoidDeriv(x: number): number
    local s = sigmoid(x)
    return s * (1 - s)
end

local function tanh_(x: number): number
    if x > 20 then return 1 end
    if x < -20 then return -1 end
    local e2x = math.exp(2 * x)
    return (e2x - 1) / (e2x + 1)
end

local function tanhDeriv(x: number): number
    local t = tanh_(x)
    return 1 - t * t
end

local function leakyRelu(x: number): number
    return if x > 0 then x else 0.01 * x
end

local function leakyReluDeriv(x: number): number
    return if x > 0 then 1 else 0.01
end

local function applyActivation(t: typeof(Tensor.new(1,1)), name: string): typeof(Tensor.new(1,1))
    if name == "relu" then
        return t:apply(relu)
    elseif name == "sigmoid" then
        return t:apply(sigmoid)
    elseif name == "tanh" then
        return t:apply(tanh_)
    elseif name == "leaky_relu" then
        return t:apply(leakyRelu)
    end
    return t:clone()
end

local function applyActivationDeriv(t: typeof(Tensor.new(1,1)), name: string): typeof(Tensor.new(1,1))
    if name == "relu" then
        return t:apply(reluDeriv)
    elseif name == "sigmoid" then
        return t:apply(sigmoidDeriv)
    elseif name == "tanh" then
        return t:apply(tanhDeriv)
    elseif name == "leaky_relu" then
        return t:apply(leakyReluDeriv)
    end
    local result = Tensor.new(t.rows, t.cols)
    result:fill(1)
    return result
end

local function softmax(t: typeof(Tensor.new(1,1))): typeof(Tensor.new(1,1))
    local result = Tensor.new(t.rows, t.cols)
    for i = 1, t.rows do
        local offset = (i - 1) * t.cols
        local maxVal = t.data[offset + 1]
        for j = 2, t.cols do
            if t.data[offset + j] > maxVal then
                maxVal = t.data[offset + j]
            end
        end
        local sum = 0
        for j = 1, t.cols do
            local e = math.exp(t.data[offset + j] - maxVal)
            result.data[offset + j] = e
            sum += e
        end
        for j = 1, t.cols do
            result.data[offset + j] /= sum
        end
    end
    return result
end

return {
    applyActivation = applyActivation,
    applyActivationDeriv = applyActivationDeriv,
    softmax = softmax,
    relu = relu,
    sigmoid = sigmoid,
}
