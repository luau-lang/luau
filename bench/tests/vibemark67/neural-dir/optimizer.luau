-- Adam optimizer

local tensorMod = require("./tensor")
local Tensor = tensorMod.Tensor

export type AdamState = {
    lr: number,
    beta1: number,
    beta2: number,
    epsilon: number,
    t: number,
}

local function createAdam(lr: number?): AdamState
    return {
        lr = lr or 0.001,
        beta1 = 0.9,
        beta2 = 0.999,
        epsilon = 1e-8,
        t = 0,
    }
end

local function adamUpdate(
    state: AdamState,
    param: typeof(Tensor.new(1,1)),
    grad: typeof(Tensor.new(1,1)),
    m: typeof(Tensor.new(1,1)),
    v: typeof(Tensor.new(1,1))
)
    state.t += 1
    local t = state.t
    local beta1 = state.beta1
    local beta2 = state.beta2
    local lr = state.lr
    local eps = state.epsilon

    local bc1 = 1 - math.pow(beta1, t)
    local bc2 = 1 - math.pow(beta2, t)

    for i = 1, #param.data do
        local g = grad.data[i]
        m.data[i] = beta1 * m.data[i] + (1 - beta1) * g
        v.data[i] = beta2 * v.data[i] + (1 - beta2) * g * g
        local mHat = m.data[i] / bc1
        local vHat = v.data[i] / bc2
        param.data[i] -= lr * mHat / (math.sqrt(vHat) + eps)
    end
end

local function adamUpdateScalars(
    state: AdamState,
    params: {number},
    grads: {number},
    m: {number},
    v: {number}
)
    state.t += 1
    local t = state.t
    local beta1 = state.beta1
    local beta2 = state.beta2
    local lr = state.lr
    local eps = state.epsilon

    local bc1 = 1 - math.pow(beta1, t)
    local bc2 = 1 - math.pow(beta2, t)

    for i = 1, #params do
        local g = grads[i]
        m[i] = beta1 * m[i] + (1 - beta1) * g
        v[i] = beta2 * v[i] + (1 - beta2) * g * g
        local mHat = m[i] / bc1
        local vHat = v[i] / bc2
        params[i] -= lr * mHat / (math.sqrt(vHat) + eps)
    end
end

return {
    createAdam = createAdam,
    adamUpdate = adamUpdate,
    adamUpdateScalars = adamUpdateScalars,
}
