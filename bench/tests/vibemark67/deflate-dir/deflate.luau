-- DEFLATE compressor and decompressor

local bitstream = require("./bitstream")
local huffman = require("./huffman")
local lz77 = require("./lz77")

local lengthBase = {
    3,4,5,6,7,8,9,10,11,13,15,17,19,23,27,31,35,43,51,59,67,83,99,115,131,163,195,227,258,
}
local lengthExtra = {
    0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,0,
}
local distBase = {
    1,2,3,4,5,7,9,13,17,25,33,49,65,97,129,193,257,385,513,769,
    1025,1537,2049,3073,4097,6145,8193,12289,16385,24577,
}
local distExtra = {
    0,0,0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,
}

local function lengthToCode(length: number): (number, number, number)
    for i = 1, #lengthBase do
        if i == #lengthBase or lengthBase[i + 1] > length then
            local code = 257 + i
            local extra = lengthExtra[i]
            local extraBits = length - lengthBase[i]
            return code, extra, extraBits
        end
    end
    return 286, 0, 0
end

local function distToCode(dist: number): (number, number, number)
    for i = 1, #distBase do
        if i == #distBase or distBase[i + 1] > dist then
            local code = i
            local extra = distExtra[i]
            local extraBits = dist - distBase[i]
            return code, extra, extraBits
        end
    end
    return 30, 0, 0
end

local function compress(data: {number}): {number}
    local writer = bitstream.BitWriter.new()
    local tokens = lz77.compress(data)
    local litTree = huffman.buildFixedLitTree()
    local distTree = huffman.buildFixedDistTree()

    writer:writeBits(1, 1) -- final block
    writer:writeBits(1, 2) -- fixed Huffman

    for _, token in tokens do
        if token.literal then
            huffman.writeCode(writer, litTree, token.literal + 1)
        else
            local match = token.match :: lz77.Match
            local lenCode, lenExtra, lenExtraBits = lengthToCode(match.length)
            huffman.writeCode(writer, litTree, lenCode)
            if lenExtra > 0 then
                writer:writeBits(lenExtraBits, lenExtra)
            end
            local dCode, dExtra, dExtraBits = distToCode(match.distance)
            huffman.writeCode(writer, distTree, dCode)
            if dExtra > 0 then
                writer:writeBits(dExtraBits, dExtra)
            end
        end
    end

    huffman.writeCode(writer, litTree, 257) -- end of block (symbol 256 is at index 257)
    return writer:getBytes()
end

local function decompress(compressed: {number}, originalSize: number): {number}
    local reader = bitstream.BitReader.new(compressed)
    local output: {number} = table.create(originalSize, 0)
    local outPos = 1

    local bfinal = reader:readBits(1)
    local btype = reader:readBits(2)

    local litTree = huffman.buildFixedLitTree()
    local distTree = huffman.buildFixedDistTree()
    local litLookup = huffman.buildLookup(litTree.lengths, litTree.codes, 288, litTree.maxLen)
    local distLookup = huffman.buildLookup(distTree.lengths, distTree.codes, 32, distTree.maxLen)

    while true do
        local sym = huffman.decodeSymbol(reader, litLookup, litTree.maxLen)
        if sym == 257 then
            break
        elseif sym <= 256 then
            output[outPos] = sym - 1
            outPos += 1
        else
            local lenIdx = sym - 257
            local length = lengthBase[lenIdx]
            local extra = lengthExtra[lenIdx]
            if extra > 0 then
                length += reader:readBits(extra)
            end

            local distSym = huffman.decodeSymbol(reader, distLookup, distTree.maxLen)
            local dist = distBase[distSym]
            local dExtra = distExtra[distSym]
            if dExtra > 0 then
                dist += reader:readBits(dExtra)
            end

            for i = 1, length do
                output[outPos] = output[outPos - dist]
                outPos += 1
            end
        end
    end

    return output
end

return {
    compress = compress,
    decompress = decompress,
}
