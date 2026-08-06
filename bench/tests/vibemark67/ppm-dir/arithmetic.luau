-- Arithmetic coder for PPM compression

local PRECISION = 30
local TOP_VALUE = bit32.lshift(1, PRECISION) - 1
local FIRST_QUARTER = bit32.lshift(1, PRECISION - 2)
local HALF = bit32.lshift(1, PRECISION - 1)
local THIRD_QUARTER = FIRST_QUARTER + HALF

local Encoder = {}
Encoder.__index = Encoder

export type Encoder = typeof(setmetatable({} :: {
    low: number,
    high: number,
    pending: number,
    output: {number},
    bitBuf: number,
    bitCount: number,
}, Encoder))

function Encoder.new(): Encoder
    return setmetatable({
        low = 0,
        high = TOP_VALUE,
        pending = 0,
        output = {},
        bitBuf = 0,
        bitCount = 0,
    }, Encoder)
end

local function encoderOutputBit(self: Encoder, bit: number)
    self.bitBuf = bit32.bor(self.bitBuf, bit32.lshift(bit, self.bitCount))
    self.bitCount += 1
    if self.bitCount == 8 then
        table.insert(self.output, self.bitBuf)
        self.bitBuf = 0
        self.bitCount = 0
    end
end

function Encoder.encode(self: Encoder, cumLow: number, cumHigh: number, total: number)
    local range = self.high - self.low + 1
    self.high = self.low + math.floor(range * cumHigh / total) - 1
    self.low = self.low + math.floor(range * cumLow / total)

    while true do
        if self.high < HALF then
            encoderOutputBit(self, 0)
            for i = 1, self.pending do
                encoderOutputBit(self, 1)
            end
            self.pending = 0
        elseif self.low >= HALF then
            encoderOutputBit(self, 1)
            for i = 1, self.pending do
                encoderOutputBit(self, 0)
            end
            self.pending = 0
            self.low -= HALF
            self.high -= HALF
        elseif self.low >= FIRST_QUARTER and self.high < THIRD_QUARTER then
            self.pending += 1
            self.low -= FIRST_QUARTER
            self.high -= FIRST_QUARTER
        else
            break
        end
        self.low = bit32.band(self.low * 2, TOP_VALUE)
        self.high = bit32.band(self.high * 2 + 1, TOP_VALUE)
    end
end

function Encoder.finish(self: Encoder)
    self.pending += 1
    if self.low < FIRST_QUARTER then
        encoderOutputBit(self, 0)
        for i = 1, self.pending do
            encoderOutputBit(self, 1)
        end
    else
        encoderOutputBit(self, 1)
        for i = 1, self.pending do
            encoderOutputBit(self, 0)
        end
    end
    if self.bitCount > 0 then
        table.insert(self.output, self.bitBuf)
    end
end

function Encoder.getBytes(self: Encoder): {number}
    return self.output
end

local Decoder = {}
Decoder.__index = Decoder

export type Decoder = typeof(setmetatable({} :: {
    low: number,
    high: number,
    value: number,
    input: {number},
    bytePos: number,
    bitPos: number,
}, Decoder))

function Decoder.new(data: {number}): Decoder
    local d = setmetatable({
        low = 0,
        high = TOP_VALUE,
        value = 0,
        input = data,
        bytePos = 1,
        bitPos = 0,
    }, Decoder)
    for i = 1, PRECISION do
        d.value = bit32.band(d.value * 2 + d:readBit(), TOP_VALUE)
    end
    return d
end

function Decoder.readBit(self: Decoder): number
    if self.bytePos > #self.input then return 0 end
    local bit = bit32.band(bit32.rshift(self.input[self.bytePos], self.bitPos), 1)
    self.bitPos += 1
    if self.bitPos == 8 then
        self.bitPos = 0
        self.bytePos += 1
    end
    return bit
end

function Decoder.getTarget(self: Decoder, total: number): number
    local range = self.high - self.low + 1
    local offset = self.value - self.low
    return math.floor(((offset + 1) * total - 1) / range)
end

function Decoder.decode(self: Decoder, cumLow: number, cumHigh: number, total: number)
    local range = self.high - self.low + 1
    self.high = self.low + math.floor(range * cumHigh / total) - 1
    self.low = self.low + math.floor(range * cumLow / total)

    while true do
        if self.high < HALF then
            -- nothing
        elseif self.low >= HALF then
            self.value -= HALF
            self.low -= HALF
            self.high -= HALF
        elseif self.low >= FIRST_QUARTER and self.high < THIRD_QUARTER then
            self.value -= FIRST_QUARTER
            self.low -= FIRST_QUARTER
            self.high -= FIRST_QUARTER
        else
            break
        end
        self.low = bit32.band(self.low * 2, TOP_VALUE)
        self.high = bit32.band(self.high * 2 + 1, TOP_VALUE)
        self.value = bit32.band(self.value * 2 + self:readBit(), TOP_VALUE)
    end
end

return {
    Encoder = Encoder,
    Decoder = Decoder,
}
