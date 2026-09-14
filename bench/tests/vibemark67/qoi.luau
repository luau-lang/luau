local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

local QOI_MAGIC = 0x716f6966
local QOI_HEADER_SIZE = 14
local QOI_PADDING_SIZE = 8

local QOI_OP_INDEX = 0x00
local QOI_OP_DIFF = 0x40
local QOI_OP_LUMA = 0x80
local QOI_OP_RUN = 0xc0
local QOI_OP_RGB = 0xfe
local QOI_OP_RGBA = 0xff
local QOI_MASK_2 = 0xc0

local WIDTH = 407
local HEIGHT = 286
local PIXEL_COUNT = WIDTH * HEIGHT
local PIXEL_BYTES = PIXEL_COUNT * 4
local MAX_ENCODED_BYTES = QOI_HEADER_SIZE + PIXEL_COUNT * 5 + QOI_PADDING_SIZE
local ITERATIONS = 6

local function packRgba(r: number, g: number, b: number, a: number): number
    return r + g * 256 + b * 65536 + a * 16777216
end

local function pixelHash(r: number, g: number, b: number, a: number): number
    return (r * 3 + g * 5 + b * 7 + a * 11) % 64
end

local function clearIndex(index: buffer)
    for offset = 0, 63 * 4, 4 do
        buffer.writeu32(index, offset, 0)
    end
end

local function encodeQoi(source: buffer, width: number, height: number, output: buffer, index: buffer): number
    local pixelCount = width * height
    assert(buffer.len(source) >= pixelCount * 4)
    assert(buffer.len(output) >= QOI_HEADER_SIZE + pixelCount * 5 + QOI_PADDING_SIZE)
    assert(buffer.len(index) >= 64 * 4)

    clearIndex(index)

    buffer.writeu32(output, 0, bit32.byteswap(QOI_MAGIC))
    buffer.writeu32(output, 4, bit32.byteswap(width))
    buffer.writeu32(output, 8, bit32.byteswap(height))
    buffer.writeu8(output, 12, 4)
    buffer.writeu8(output, 13, 0)

    local outputOffset = QOI_HEADER_SIZE
    local previous = packRgba(0, 0, 0, 255)
    local run = 0

    for pixelIndex = 0, pixelCount - 1 do
        local pixel = buffer.readu32(source, pixelIndex * 4)

        if pixel == previous then
            run += 1

            if run == 62 or pixelIndex == pixelCount - 1 then
                buffer.writeu8(output, outputOffset, QOI_OP_RUN + run - 1)
                outputOffset += 1
                run = 0
            end
        else
            if run > 0 then
                buffer.writeu8(output, outputOffset, QOI_OP_RUN + run - 1)
                outputOffset += 1
                run = 0
            end

            local r = bit32.extract(pixel, 0, 8)
            local g = bit32.extract(pixel, 8, 8)
            local b = bit32.extract(pixel, 16, 8)
            local a = bit32.extract(pixel, 24, 8)
            local hashOffset = pixelHash(r, g, b, a) * 4

            if buffer.readu32(index, hashOffset) == pixel then
                buffer.writeu8(output, outputOffset, QOI_OP_INDEX + hashOffset / 4)
                outputOffset += 1
            else
                buffer.writeu32(index, hashOffset, pixel)

                local previousR = bit32.extract(previous, 0, 8)
                local previousG = bit32.extract(previous, 8, 8)
                local previousB = bit32.extract(previous, 16, 8)
                local previousA = bit32.extract(previous, 24, 8)

                if a == previousA then
                    local dr = r - previousR
                    local dg = g - previousG
                    local db = b - previousB

                    if dr >= -2 and dr <= 1 and dg >= -2 and dg <= 1 and db >= -2 and db <= 1 then
                        local chunk = QOI_OP_DIFF + (dr + 2) * 16 + (dg + 2) * 4 + db + 2
                        buffer.writeu8(output, outputOffset, chunk)
                        outputOffset += 1
                    else
                        local drdg = dr - dg
                        local dbdg = db - dg

                        if dg >= -32 and dg <= 31 and drdg >= -8 and drdg <= 7 and dbdg >= -8 and dbdg <= 7 then
                            buffer.writeu8(output, outputOffset, QOI_OP_LUMA + dg + 32)
                            buffer.writeu8(output, outputOffset + 1, (drdg + 8) * 16 + dbdg + 8)
                            outputOffset += 2
                        else
                            buffer.writeu8(output, outputOffset, QOI_OP_RGB)
                            buffer.writeu8(output, outputOffset + 1, r)
                            buffer.writeu8(output, outputOffset + 2, g)
                            buffer.writeu8(output, outputOffset + 3, b)
                            outputOffset += 4
                        end
                    end
                else
                    buffer.writeu8(output, outputOffset, QOI_OP_RGBA)
                    buffer.writeu32(output, outputOffset + 1, pixel)
                    outputOffset += 5
                end
            end

            previous = pixel
        end
    end

    buffer.fill(output, outputOffset, 0, QOI_PADDING_SIZE - 1)
    buffer.writeu8(output, outputOffset + QOI_PADDING_SIZE - 1, 1)
    return outputOffset + QOI_PADDING_SIZE
end

local function decodeQoi(source: buffer, sourceSize: number, output: buffer, index: buffer): (number, number, number, number)
    assert(sourceSize >= QOI_HEADER_SIZE + QOI_PADDING_SIZE)
    assert(sourceSize <= buffer.len(source))
    assert(bit32.byteswap(buffer.readu32(source, 0)) == QOI_MAGIC)

    local width = bit32.byteswap(buffer.readu32(source, 4))
    local height = bit32.byteswap(buffer.readu32(source, 8))
    local channels = buffer.readu8(source, 12)
    local colorspace = buffer.readu8(source, 13)
    local pixelCount = width * height

    assert(width > 0 and height > 0)
    assert(channels == 3 or channels == 4)
    assert(colorspace == 0 or colorspace == 1)
    assert(buffer.len(output) >= pixelCount * 4)
    assert(buffer.len(index) >= 64 * 4)

    local dataEnd = sourceSize - QOI_PADDING_SIZE
    for i = 0, QOI_PADDING_SIZE - 2 do
        assert(buffer.readu8(source, dataEnd + i) == 0)
    end
    assert(buffer.readu8(source, sourceSize - 1) == 1)

    clearIndex(index)

    local inputOffset = QOI_HEADER_SIZE
    local previous = packRgba(0, 0, 0, 255)
    local run = 0

    for pixelIndex = 0, pixelCount - 1 do
        local pixel = previous

        if run > 0 then
            run -= 1
        else
            assert(inputOffset < dataEnd)
            local tag = buffer.readu8(source, inputOffset)
            inputOffset += 1

            if tag == QOI_OP_RGB then
                assert(inputOffset + 3 <= dataEnd)
                local r = buffer.readu8(source, inputOffset)
                local g = buffer.readu8(source, inputOffset + 1)
                local b = buffer.readu8(source, inputOffset + 2)
                local a = bit32.extract(previous, 24, 8)
                inputOffset += 3
                pixel = packRgba(r, g, b, a)
            elseif tag == QOI_OP_RGBA then
                assert(inputOffset + 4 <= dataEnd)
                pixel = buffer.readu32(source, inputOffset)
                inputOffset += 4
            else
                local tagType = bit32.band(tag, QOI_MASK_2)

                if tagType == QOI_OP_INDEX then
                    pixel = buffer.readu32(index, bit32.band(tag, 0x3f) * 4)
                elseif tagType == QOI_OP_DIFF then
                    local r = bit32.extract(previous, 0, 8)
                    local g = bit32.extract(previous, 8, 8)
                    local b = bit32.extract(previous, 16, 8)
                    local a = bit32.extract(previous, 24, 8)
                    r = bit32.band(r + bit32.extract(tag, 4, 2) - 2, 0xff)
                    g = bit32.band(g + bit32.extract(tag, 2, 2) - 2, 0xff)
                    b = bit32.band(b + bit32.extract(tag, 0, 2) - 2, 0xff)
                    pixel = packRgba(r, g, b, a)
                elseif tagType == QOI_OP_LUMA then
                    assert(inputOffset < dataEnd)
                    local second = buffer.readu8(source, inputOffset)
                    inputOffset += 1

                    local dg = bit32.band(tag, 0x3f) - 32
                    local drdg = bit32.extract(second, 4, 4) - 8
                    local dbdg = bit32.extract(second, 0, 4) - 8
                    local r = bit32.extract(previous, 0, 8)
                    local g = bit32.extract(previous, 8, 8)
                    local b = bit32.extract(previous, 16, 8)
                    local a = bit32.extract(previous, 24, 8)
                    r = bit32.band(r + dg + drdg, 0xff)
                    g = bit32.band(g + dg, 0xff)
                    b = bit32.band(b + dg + dbdg, 0xff)
                    pixel = packRgba(r, g, b, a)
                else
                    run = bit32.band(tag, 0x3f)
                end
            end
        end

        buffer.writeu32(output, pixelIndex * 4, pixel)

        local r = bit32.extract(pixel, 0, 8)
        local g = bit32.extract(pixel, 8, 8)
        local b = bit32.extract(pixel, 16, 8)
        local a = bit32.extract(pixel, 24, 8)
        buffer.writeu32(index, pixelHash(r, g, b, a) * 4, pixel)
        previous = pixel
    end

    assert(inputOffset == dataEnd)
    return width, height, channels, colorspace
end

local function makeSource(): buffer
    local source = buffer.create(PIXEL_BYTES)
    local state = 0x12345678

    for y = 0, HEIGHT - 1 do
        for x = 0, WIDTH - 1 do
            local r, g, b, a

            if y < 32 then
                local band = math.floor(x / 48)
                r = band * 29 % 256
                g = (y * 7 + band * 13) % 256
                b = (y * 11 + band * 17) % 256
                a = 255
            elseif y < 64 then
                local value = (x + (y - 32) * WIDTH) % 256
                r, g, b, a = value, value, value, 255
            elseif y < 96 then
                local value = (x + (y - 64) * WIDTH) % 256
                r, g, b, a = value * 2 % 256, value, 80, 255
            elseif y < 144 then
                if x % 2 == 0 then
                    r, g, b, a = 10, 20, 30, 255
                else
                    r, g, b, a = 100, 100, 100, 255
                end
            elseif y < 224 then
                state = bit32.band(state * 1664525 + 1013904223, 0xffffffff)
                r = bit32.extract(state, 0, 8)
                g = bit32.extract(state, 8, 8)
                b = bit32.extract(state, 16, 8)
                a = 255
            else
                r = (x * 31 + y * 3) % 256
                g = (x * 17 + y * 5) % 256
                b = (x * 7 + y * 11) % 256
                a = math.floor(x / 4) * 19 % 256
            end

            buffer.writeu32(source, (y * WIDTH + x) * 4, packRgba(r, g, b, a))
        end
    end

    return source
end

local function assertEqualPixels(expected: buffer, actual: buffer)
    assert(buffer.len(expected) == buffer.len(actual))

    for offset = 0, buffer.len(expected) - 4, 4 do
        assert(buffer.readu32(expected, offset) == buffer.readu32(actual, offset))
    end
end

local function assertAllOpcodes(encoded: buffer, encodedSize: number)
    local counts = {0, 0, 0, 0, 0, 0}
    local offset = QOI_HEADER_SIZE
    local dataEnd = encodedSize - QOI_PADDING_SIZE

    while offset < dataEnd do
        local tag = buffer.readu8(encoded, offset)
        offset += 1

        if tag == QOI_OP_RGB then
            counts[5] += 1
            offset += 3
        elseif tag == QOI_OP_RGBA then
            counts[6] += 1
            offset += 4
        else
            local tagType = bit32.band(tag, QOI_MASK_2)
            if tagType == QOI_OP_INDEX then
                counts[1] += 1
            elseif tagType == QOI_OP_DIFF then
                counts[2] += 1
            elseif tagType == QOI_OP_LUMA then
                counts[3] += 1
                offset += 1
            else
                counts[4] += 1
            end
        end
    end

    assert(offset == dataEnd)
    for _, count in counts do
        assert(count > 0)
    end
end

local source = makeSource()
local encoded = buffer.create(MAX_ENCODED_BYTES)
local decoded = buffer.create(PIXEL_BYTES)
local encodeIndex = buffer.create(64 * 4)
local decodeIndex = buffer.create(64 * 4)

-- Verify correctness and opcode coverage once, outside the timed samples.
local encodedSize = encodeQoi(source, WIDTH, HEIGHT, encoded, encodeIndex)
local decodedWidth, decodedHeight, decodedChannels, decodedColorspace = decodeQoi(encoded, encodedSize, decoded, decodeIndex)
assert(decodedWidth == WIDTH and decodedHeight == HEIGHT)
assert(decodedChannels == 4 and decodedColorspace == 0)
assertEqualPixels(source, decoded)
assertAllOpcodes(encoded, encodedSize)

local sink = 0

local function preallocatedRoundTrips()
    local checksum = 0

    for _ = 1, ITERATIONS do
        local size = encodeQoi(source, WIDTH, HEIGHT, encoded, encodeIndex)
        local width, height = decodeQoi(encoded, size, decoded, decodeIndex)
        checksum += size + width + height + buffer.readu32(decoded, PIXEL_BYTES - 4)
    end

    sink = checksum
end

local function allocatingRoundTrips()
    local checksum = 0

    for _ = 1, ITERATIONS do
        local iterationEncoded = buffer.create(MAX_ENCODED_BYTES)
        local iterationDecoded = buffer.create(PIXEL_BYTES)
        local iterationEncodeIndex = buffer.create(64 * 4)
        local iterationDecodeIndex = buffer.create(64 * 4)
        local size = encodeQoi(source, WIDTH, HEIGHT, iterationEncoded, iterationEncodeIndex)
        local width, height = decodeQoi(iterationEncoded, size, iterationDecoded, iterationDecodeIndex)
        checksum += size + width + height + buffer.readu32(iterationDecoded, PIXEL_BYTES - 4)
    end

    sink = checksum
end

bench.runCode(preallocatedRoundTrips, "qoi-preallocated")
bench.runCode(allocatingRoundTrips, "qoi-allocate")

assert(sink ~= 0)
