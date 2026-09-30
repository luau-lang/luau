-- Bit-level I/O for DEFLATE compression

local BitWriter = {}
BitWriter.__index = BitWriter

export type BitWriter = typeof(setmetatable({} :: {
    buffer: {number},
    bitBuf: number,
    bitCount: number,
}, BitWriter))

function BitWriter.new(): BitWriter
    return setmetatable({
        buffer = {},
        bitBuf = 0,
        bitCount = 0,
    }, BitWriter)
end

function BitWriter.writeBits(self: BitWriter, value: number, numBits: number)
    self.bitBuf = bit32.bor(self.bitBuf, bit32.lshift(value, self.bitCount))
    self.bitCount += numBits
    while self.bitCount >= 8 do
        table.insert(self.buffer, bit32.band(self.bitBuf, 0xFF))
        self.bitBuf = bit32.rshift(self.bitBuf, 8)
        self.bitCount -= 8
    end
end

function BitWriter.writeByte(self: BitWriter, byte: number)
    self:writeBits(byte, 8)
end

function BitWriter.flush(self: BitWriter)
    if self.bitCount > 0 then
        table.insert(self.buffer, bit32.band(self.bitBuf, 0xFF))
        self.bitBuf = 0
        self.bitCount = 0
    end
end

function BitWriter.getBytes(self: BitWriter): {number}
    self:flush()
    return self.buffer
end

function BitWriter.size(self: BitWriter): number
    return #self.buffer
end

local BitReader = {}
BitReader.__index = BitReader

export type BitReader = typeof(setmetatable({} :: {
    data: {number},
    pos: number,
    bitBuf: number,
    bitCount: number,
}, BitReader))

function BitReader.new(data: {number}): BitReader
    return setmetatable({
        data = data,
        pos = 1,
        bitBuf = 0,
        bitCount = 0,
    }, BitReader)
end

function BitReader.readBits(self: BitReader, numBits: number): number
    while self.bitCount < numBits do
        if self.pos > #self.data then
            self.bitCount += 8
        else
            self.bitBuf = bit32.bor(self.bitBuf, bit32.lshift(self.data[self.pos], self.bitCount))
            self.pos += 1
            self.bitCount += 8
        end
    end
    local value = bit32.band(self.bitBuf, bit32.lshift(1, numBits) - 1)
    self.bitBuf = bit32.rshift(self.bitBuf, numBits)
    self.bitCount -= numBits
    return value
end

function BitReader.readByte(self: BitReader): number
    return self:readBits(8)
end

return {
    BitWriter = BitWriter,
    BitReader = BitReader,
}
