local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

local tensorMod = require("./neural-dir/tensor")
local layers = require("./neural-dir/layers")
local activations = require("./neural-dir/activations")
local optimizer = require("./neural-dir/optimizer")

function test()

-- Neural network benchmark: train a multi-layer network on procedurally generated data

local Tensor = tensorMod.Tensor

local BATCH_SIZE = 32
local INPUT_SIZE = 64
local HIDDEN1_SIZE = 128
local HIDDEN2_SIZE = 64
local OUTPUT_SIZE = 10
local NUM_EPOCHS = 5
local NUM_BATCHES = 5

local seed = 42424242

local function generateBatch(batchSize: number, inputSize: number, outputSize: number): (typeof(Tensor.new(1,1)), typeof(Tensor.new(1,1)), number)
    local input
    input, seed = Tensor.randomNormal(batchSize, inputSize, seed, 1.0)
    local targets = Tensor.new(batchSize, outputSize)
    for i = 1, batchSize do
        local sum = 0
        local offset = (i - 1) * inputSize
        for j = 1, inputSize do
            sum += input.data[offset + j]
        end
        local classIdx = (math.floor(math.abs(sum * 100)) % outputSize) + 1
        targets.data[(i - 1) * outputSize + classIdx] = 1
    end
    return input, targets, seed
end

local dense1
dense1, seed = layers.createDense(INPUT_SIZE, HIDDEN1_SIZE, seed)
local dense2
dense2, seed = layers.createDense(HIDDEN1_SIZE, HIDDEN2_SIZE, seed)
local dense3
dense3, seed = layers.createDense(HIDDEN2_SIZE, OUTPUT_SIZE, seed)

local adam1 = optimizer.createAdam(0.001)
local adam2 = optimizer.createAdam(0.001)
local adam3 = optimizer.createAdam(0.001)

local totalLoss = 0
local totalBatches = 0

for epoch = 1, NUM_EPOCHS do
    local epochLoss = 0

    for batch = 1, NUM_BATCHES do
        local input, targets
        input, targets, seed = generateBatch(BATCH_SIZE, INPUT_SIZE, OUTPUT_SIZE)

        -- Forward pass
        local z1 = layers.denseForward(dense1, input)
        local a1 = activations.applyActivation(z1, "relu")

        local z2 = layers.denseForward(dense2, a1)
        local a2 = activations.applyActivation(z2, "leaky_relu")

        local z3 = layers.denseForward(dense3, a2)
        local output = activations.softmax(z3)

        -- Loss (cross-entropy approximated by MSE for simplicity)
        local loss = Tensor.meanSquaredError(output, targets)
        epochLoss += loss

        -- Backward pass
        local gradOutput = Tensor.sub(output, targets):mulScalar(2.0 / (BATCH_SIZE * OUTPUT_SIZE))

        local gradZ3 = gradOutput
        local gradA2 = layers.denseBackward(dense3, gradZ3)
        local gradZ2 = Tensor.hadamard(gradA2, activations.applyActivationDeriv(z2, "leaky_relu"))
        local gradA1 = layers.denseBackward(dense2, gradZ2)
        local gradZ1 = Tensor.hadamard(gradA1, activations.applyActivationDeriv(z1, "relu"))
        layers.denseBackward(dense1, gradZ1)

        -- Adam updates
        optimizer.adamUpdate(adam1, dense1.weights, dense1.dWeights, dense1.mW, dense1.vW)
        optimizer.adamUpdate(adam1, dense1.bias, dense1.dBias, dense1.mB, dense1.vB)
        optimizer.adamUpdate(adam2, dense2.weights, dense2.dWeights, dense2.mW, dense2.vW)
        optimizer.adamUpdate(adam2, dense2.bias, dense2.dBias, dense2.mB, dense2.vB)
        optimizer.adamUpdate(adam3, dense3.weights, dense3.dWeights, dense3.mW, dense3.vW)
        optimizer.adamUpdate(adam3, dense3.bias, dense3.dBias, dense3.mB, dense3.vB)

        totalBatches += 1
    end

    totalLoss += epochLoss / NUM_BATCHES
end

local avgLoss = totalLoss / NUM_EPOCHS
print(string.format("Neural benchmark complete: %d epochs, %d batches, avg_loss=%.17g", NUM_EPOCHS, totalBatches, avgLoss))

if avgLoss ~= 0.099400851977591437 then
    error("Bad result")
end

end

bench.runCode(test, "neural")
