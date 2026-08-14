-- Neural network layers: Dense and Conv1D

local tensorMod = require("./tensor")
local Tensor = tensorMod.Tensor

export type DenseLayer = {
    weights: typeof(Tensor.new(1,1)),
    bias: typeof(Tensor.new(1,1)),
    dWeights: typeof(Tensor.new(1,1)),
    dBias: typeof(Tensor.new(1,1)),
    input: typeof(Tensor.new(1,1))?,
    mW: typeof(Tensor.new(1,1)),
    vW: typeof(Tensor.new(1,1)),
    mB: typeof(Tensor.new(1,1)),
    vB: typeof(Tensor.new(1,1)),
    inputSize: number,
    outputSize: number,
}

export type Conv1DLayer = {
    filters: {typeof(Tensor.new(1,1))},
    biases: {number},
    dFilters: {typeof(Tensor.new(1,1))},
    dBiases: {number},
    input: typeof(Tensor.new(1,1))?,
    mF: {typeof(Tensor.new(1,1))},
    vF: {typeof(Tensor.new(1,1))},
    mB: {number},
    vB: {number},
    numFilters: number,
    kernelSize: number,
    inputChannels: number,
}

local function createDense(inputSize: number, outputSize: number, seed: number): (DenseLayer, number)
    local scale = math.sqrt(2.0 / inputSize)
    local weights, newSeed = Tensor.randomNormal(inputSize, outputSize, seed, scale)
    local bias = Tensor.new(1, outputSize)
    return {
        weights = weights,
        bias = bias,
        dWeights = Tensor.new(inputSize, outputSize),
        dBias = Tensor.new(1, outputSize),
        input = nil,
        mW = Tensor.new(inputSize, outputSize),
        vW = Tensor.new(inputSize, outputSize),
        mB = Tensor.new(1, outputSize),
        vB = Tensor.new(1, outputSize),
        inputSize = inputSize,
        outputSize = outputSize,
    }, newSeed
end

local function createConv1D(inputChannels: number, numFilters: number, kernelSize: number, seed: number): (Conv1DLayer, number)
    local scale = math.sqrt(2.0 / (inputChannels * kernelSize))
    local filters = {}
    local mF = {}
    local vF = {}
    for f = 1, numFilters do
        local w
        w, seed = Tensor.randomNormal(inputChannels, kernelSize, seed, scale)
        filters[f] = w
        mF[f] = Tensor.new(inputChannels, kernelSize)
        vF[f] = Tensor.new(inputChannels, kernelSize)
    end
    local biases = table.create(numFilters, 0)
    local dBiases = table.create(numFilters, 0)
    local mB = table.create(numFilters, 0)
    local vB = table.create(numFilters, 0)
    local dFilters = {}
    for f = 1, numFilters do
        dFilters[f] = Tensor.new(inputChannels, kernelSize)
    end
    return {
        filters = filters,
        biases = biases,
        dFilters = dFilters,
        dBiases = dBiases,
        input = nil,
        mF = mF,
        vF = vF,
        mB = mB,
        vB = vB,
        numFilters = numFilters,
        kernelSize = kernelSize,
        inputChannels = inputChannels,
    }, seed
end

local function denseForward(layer: DenseLayer, input: typeof(Tensor.new(1,1))): typeof(Tensor.new(1,1))
    layer.input = input
    local output = Tensor.matmul(input, layer.weights)
    return output:addBias(layer.bias)
end

local function denseBackward(layer: DenseLayer, gradOutput: typeof(Tensor.new(1,1))): typeof(Tensor.new(1,1))
    local input = layer.input :: typeof(Tensor.new(1,1))
    layer.dWeights = Tensor.matmul(input:transpose(), gradOutput)
    layer.dBias = gradOutput:sumRows()
    return Tensor.matmul(gradOutput, layer.weights:transpose())
end

local function conv1DForward(layer: Conv1DLayer, input: typeof(Tensor.new(1,1))): typeof(Tensor.new(1,1))
    layer.input = input
    local batchSize = input.rows
    local inputLen = math.floor(input.cols / layer.inputChannels)
    local outputLen = inputLen - layer.kernelSize + 1
    local output = Tensor.new(batchSize, outputLen * layer.numFilters)

    for b = 1, batchSize do
        for f = 1, layer.numFilters do
            local filter = layer.filters[f]
            for i = 1, outputLen do
                local sum = layer.biases[f]
                for c = 1, layer.inputChannels do
                    for k = 1, layer.kernelSize do
                        local inputIdx = (b - 1) * input.cols + (i + k - 2) * layer.inputChannels + c
                        local filterIdx = (c - 1) * layer.kernelSize + k
                        sum += input.data[inputIdx] * filter.data[filterIdx]
                    end
                end
                local outIdx = (b - 1) * output.cols + (i - 1) * layer.numFilters + f
                output.data[outIdx] = sum
            end
        end
    end
    return output
end

local function conv1DBackward(layer: Conv1DLayer, gradOutput: typeof(Tensor.new(1,1))): typeof(Tensor.new(1,1))
    local input = layer.input :: typeof(Tensor.new(1,1))
    local batchSize = input.rows
    local inputLen = math.floor(input.cols / layer.inputChannels)
    local outputLen = inputLen - layer.kernelSize + 1
    local gradInput = Tensor.new(batchSize, input.cols)

    for f = 1, layer.numFilters do
        layer.dFilters[f]:fill(0)
        layer.dBiases[f] = 0
    end

    for b = 1, batchSize do
        for f = 1, layer.numFilters do
            local filter = layer.filters[f]
            for i = 1, outputLen do
                local gradIdx = (b - 1) * gradOutput.cols + (i - 1) * layer.numFilters + f
                local g = gradOutput.data[gradIdx]
                layer.dBiases[f] += g
                for c = 1, layer.inputChannels do
                    for k = 1, layer.kernelSize do
                        local inputIdx = (b - 1) * input.cols + (i + k - 2) * layer.inputChannels + c
                        local filterIdx = (c - 1) * layer.kernelSize + k
                        layer.dFilters[f].data[filterIdx] += g * input.data[inputIdx]
                        gradInput.data[inputIdx] += g * filter.data[filterIdx]
                    end
                end
            end
        end
    end
    return gradInput
end

return {
    createDense = createDense,
    createConv1D = createConv1D,
    denseForward = denseForward,
    denseBackward = denseBackward,
    conv1DForward = conv1DForward,
    conv1DBackward = conv1DBackward,
}
