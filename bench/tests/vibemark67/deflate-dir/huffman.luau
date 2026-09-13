-- Huffman coding for DEFLATE

local bitstream = require("./bitstream")

export type HuffTree = {
    codes: {number},
    lengths: {number},
    maxLen: number,
}

local function buildFromLengths(lengths: {number}, maxSymbol: number): HuffTree
    local maxLen = 0
    for i = 1, maxSymbol do
        if lengths[i] and lengths[i] > maxLen then
            maxLen = lengths[i]
        end
    end
    if maxLen == 0 then maxLen = 1 end

    local blCount = table.create(maxLen + 1, 0)
    for i = 1, maxSymbol do
        local len = lengths[i] or 0
        if len > 0 then
            blCount[len + 1] = (blCount[len + 1] or 0) + 1
        end
    end

    local nextCode = table.create(maxLen + 2, 0)
    local code = 0
    for bits = 1, maxLen do
        code = bit32.lshift(code + (blCount[bits] or 0), 1)
        nextCode[bits + 1] = code
    end

    local codes = table.create(maxSymbol, 0)
    for i = 1, maxSymbol do
        local len = lengths[i] or 0
        if len > 0 then
            codes[i] = nextCode[len + 1]
            nextCode[len + 1] += 1
        end
    end

    return {
        codes = codes,
        lengths = lengths,
        maxLen = maxLen,
    }
end

local function writeCode(writer: bitstream.BitWriter, tree: HuffTree, symbol: number)
    local len = tree.lengths[symbol] or 0
    local code = tree.codes[symbol] or 0
    local reversed = 0
    for i = 1, len do
        reversed = bit32.bor(bit32.lshift(reversed, 1), bit32.band(code, 1))
        code = bit32.rshift(code, 1)
    end
    writer:writeBits(reversed, len)
end

local function buildDecodeTable(tree: HuffTree, maxSymbol: number): {{symbol: number, length: number}}
    local tableSize = bit32.lshift(1, tree.maxLen)
    local decodeTable: {{symbol: number, length: number}} = table.create(tableSize)

    for sym = 1, maxSymbol do
        local len = tree.lengths[sym] or 0
        if len > 0 then
            local code = tree.codes[sym]
            local reversed = 0
            for i = 1, len do
                reversed = bit32.bor(bit32.lshift(reversed, 1), bit32.band(code, 1))
                code = bit32.rshift(code, 1)
            end
            local fill = bit32.lshift(1, tree.maxLen - len)
            for i = 0, fill - 1 do
                local idx = bit32.bor(reversed, bit32.lshift(i, len))
                decodeTable[idx + 1] = {symbol = sym, length = len}
            end
        end
    end

    return decodeTable
end

local function buildLookup(lengths: {number}, codes: {number}, maxSymbol: number, maxLen: number): {number}
    local tableSize = bit32.lshift(1, maxLen)
    local lookup = table.create(tableSize, 0)
    for sym = 1, maxSymbol do
        local len = lengths[sym] or 0
        if len > 0 then
            local code = codes[sym]
            local reversed = 0
            for i = 1, len do
                reversed = bit32.bor(bit32.lshift(reversed, 1), bit32.band(code, 1))
                code = bit32.rshift(code, 1)
            end
            local fill = bit32.lshift(1, maxLen - len)
            for i = 0, fill - 1 do
                local idx = bit32.bor(reversed, bit32.lshift(i, len)) + 1
                lookup[idx] = bit32.bor(bit32.lshift(sym, 8), len)
            end
        end
    end
    return lookup
end

local function decodeSymbol(reader: bitstream.BitReader, lookup: {number}, maxLen: number): number
    while reader.bitCount < maxLen do
        if reader.pos > #reader.data then
            reader.bitCount += 8
        else
            reader.bitBuf = bit32.bor(reader.bitBuf, bit32.lshift(reader.data[reader.pos], reader.bitCount))
            reader.pos += 1
            reader.bitCount += 8
        end
    end
    local peekBits = bit32.band(reader.bitBuf, bit32.lshift(1, maxLen) - 1)
    local entry = lookup[peekBits + 1]
    local sym = bit32.rshift(entry, 8)
    local len = bit32.band(entry, 0xFF)
    reader.bitBuf = bit32.rshift(reader.bitBuf, len)
    reader.bitCount -= len
    return sym
end

local function buildFixedLitTree(): HuffTree
    local lengths = table.create(288, 0)
    for i = 1, 144 do lengths[i] = 8 end
    for i = 145, 256 do lengths[i] = 9 end
    for i = 257, 280 do lengths[i] = 7 end
    for i = 281, 288 do lengths[i] = 8 end
    return buildFromLengths(lengths, 288)
end

local function buildFixedDistTree(): HuffTree
    local lengths = table.create(32, 5)
    return buildFromLengths(lengths, 32)
end

return {
    buildFromLengths = buildFromLengths,
    writeCode = writeCode,
    buildDecodeTable = buildDecodeTable,
    buildLookup = buildLookup,
    decodeSymbol = decodeSymbol,
    buildFixedLitTree = buildFixedLitTree,
    buildFixedDistTree = buildFixedDistTree,
}
