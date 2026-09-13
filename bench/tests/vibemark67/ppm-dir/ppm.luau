-- PPM compressor and decompressor

local arithmetic = require("./arithmetic")
local context = require("./context")

local function compress(data: {number}): {number}
    local model = context.createModel(context.MAX_ORDER)
    local encoder = arithmetic.Encoder.new()
    local history: {number} = {}

    for _, symbol in data do
        local encoded = false
        for order = math.min(context.MAX_ORDER, #history), 0, -1 do
            local node = context.getContext(model, history, order)
            if node and node.total > 0 then
                local cumLow, cumHigh, total, escaped =
                    context.getSymbolProbabilityWithEscape(node, symbol + 1)
                encoder:encode(cumLow, cumHigh, total)
                if not escaped then
                    encoded = true
                    break
                end
            else
                local total = context.ALPHABET_SIZE
                local cumLow = symbol
                local cumHigh = symbol + 1
                encoder:encode(cumLow, cumHigh, total)
                encoded = true
                break
            end
        end

        if not encoded then
            local total = context.ALPHABET_SIZE
            encoder:encode(symbol, symbol + 1, total)
        end

        context.updateModel(model, history, symbol + 1)
        table.insert(history, symbol + 1)
        if #history > context.MAX_ORDER then
            table.remove(history, 1)
        end
    end

    local eofSymbol = 256
    for order = math.min(context.MAX_ORDER, #history), 0, -1 do
        local node = context.getContext(model, history, order)
        if node and node.total > 0 then
            local cumLow, cumHigh, total, escaped =
                context.getSymbolProbabilityWithEscape(node, eofSymbol + 1)
            encoder:encode(cumLow, cumHigh, total)
            if not escaped then break end
        else
            encoder:encode(eofSymbol, eofSymbol + 1, context.ALPHABET_SIZE)
            break
        end
    end

    encoder:finish()
    return encoder:getBytes()
end

local function decompress(compressed: {number}, originalSize: number): {number}
    local model = context.createModel(context.MAX_ORDER)
    local decoder = arithmetic.Decoder.new(compressed)
    local history: {number} = {}
    local output: {number} = table.create(originalSize, 0)
    local outPos = 1

    while outPos <= originalSize do
        local symbol = -1

        for order = math.min(context.MAX_ORDER, #history), 0, -1 do
            local node = context.getContext(model, history, order)
            if node and node.total > 0 then
                local total = context.getTotalWithEscape(node)
                local target = decoder:getTarget(total)
                local sym, cumLow, cumHigh, escaped =
                    context.decodeSymbolFromContext(node, target, total)
                decoder:decode(cumLow, cumHigh, total)
                if not escaped then
                    symbol = sym - 1
                    break
                end
            else
                local total = context.ALPHABET_SIZE
                local target = decoder:getTarget(total)
                decoder:decode(target, target + 1, total)
                symbol = target
                break
            end
        end

        if symbol == -1 then
            local total = context.ALPHABET_SIZE
            local target = decoder:getTarget(total)
            decoder:decode(target, target + 1, total)
            symbol = target
        end

        if symbol >= 256 then break end

        output[outPos] = symbol
        outPos += 1

        context.updateModel(model, history, symbol + 1)
        table.insert(history, symbol + 1)
        if #history > context.MAX_ORDER then
            table.remove(history, 1)
        end
    end

    return output
end

return {
    compress = compress,
    decompress = decompress,
}
