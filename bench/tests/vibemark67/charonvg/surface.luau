--!native
local Color = require("./color")

local Surface = {}
Surface.__index = Surface

local MAX_DIM = 32768

function Surface.create(width, height)
    assert(width > 0 and width <= MAX_DIM, "invalid width")
    assert(height > 0 and height <= MAX_DIM, "invalid height")
    width = math.floor(width)
    height = math.floor(height)
    local stride = width * 4
    local data = buffer.create(stride * height)
    return setmetatable({
        _width = width,
        _height = height,
        _stride = stride,
        _data = data,
    }, Surface)
end

function Surface:getWidth()
    return self._width
end

function Surface:getHeight()
    return self._height
end

function Surface:getStride()
    return self._stride
end

function Surface:getData()
    return self._data
end

function Surface:clear(color)
    local size = self._stride * self._height
    if not color then
        buffer.fill(self._data, 0, 0, size)
        return
    end
    local a = math.floor(color.a * 255 + 0.5)
    local r = math.floor(color.r * a + 0.5)
    local g = math.floor(color.g * a + 0.5)
    local b = math.floor(color.b * a + 0.5)
    -- Fast path: if all bytes are the same value, use buffer.fill
    if a == r and r == g and g == b then
        buffer.fill(self._data, 0, a, size)
        return
    end
    local pixel = bit32.bor(bit32.lshift(a, 24), bit32.lshift(r, 16), bit32.lshift(g, 8), b)
    local data = self._data
    -- Write first pixel, then copy in doubling chunks
    buffer.writeu32(data, 0, pixel)
    local written = 4
    while written < size do
        local chunk = written
        if chunk > size - written then
            chunk = size - written
        end
        buffer.copy(data, written, data, 0, chunk)
        written = written + chunk
    end
end

function Surface:getPixel(x, y)
    if x < 0 or x >= self._width or y < 0 or y >= self._height then
        return 0, 0, 0, 0
    end
    local offset = y * self._stride + x * 4
    local pixel = buffer.readu32(self._data, offset)
    local a = bit32.rshift(pixel, 24)
    local r = bit32.band(bit32.rshift(pixel, 16), 0xFF)
    local g = bit32.band(bit32.rshift(pixel, 8), 0xFF)
    local b = bit32.band(pixel, 0xFF)
    -- Unpremultiply
    if a == 0 then
        return 0, 0, 0, 0
    elseif a == 255 then
        return r, g, b, a
    else
        r = math.floor(r * 255 / a + 0.5)
        g = math.floor(g * 255 / a + 0.5)
        b = math.floor(b * 255 / a + 0.5)
        return r, g, b, a
    end
end

function Surface:setPixel(x, y, r, g, b, a)
    if x < 0 or x >= self._width or y < 0 or y >= self._height then
        return
    end
    -- Premultiply
    local pr = math.floor(r * a / 255 + 0.5)
    local pg = math.floor(g * a / 255 + 0.5)
    local pb = math.floor(b * a / 255 + 0.5)
    local pixel = bit32.bor(bit32.lshift(a, 24), bit32.lshift(pr, 16), bit32.lshift(pg, 8), pb)
    local offset = y * self._stride + x * 4
    buffer.writeu32(self._data, offset, pixel)
end

function Surface.convertARGBtoRGBA(dst, src, width, height, stride)
    for y = 0, height - 1 do
        local rowOff = y * stride
        for x = 0, width - 1 do
            local off = rowOff + x * 4
            local pixel = buffer.readu32(src, off)
            local a = bit32.rshift(pixel, 24)
            local r = bit32.band(bit32.rshift(pixel, 16), 0xFF)
            local g = bit32.band(bit32.rshift(pixel, 8), 0xFF)
            local b = bit32.band(pixel, 0xFF)
            -- Unpremultiply
            if a > 0 and a < 255 then
                r = math.min(255, math.floor(r * 255 / a + 0.5))
                g = math.min(255, math.floor(g * 255 / a + 0.5))
                b = math.min(255, math.floor(b * 255 / a + 0.5))
            end
            -- Write as RGBA byte order
            buffer.writeu8(dst, off, r)
            buffer.writeu8(dst, off + 1, g)
            buffer.writeu8(dst, off + 2, b)
            buffer.writeu8(dst, off + 3, a)
        end
    end
end

function Surface:getRGBA()
    local total = self._width * self._height
    local src = self._data
    local dst = buffer.create(total * 4)
    local size = total * 4

    local lastIn0 = -1
    local lastIn1 = -2
    local lastOut0 = 0
    local lastOut1 = 0

    local pairsEnd = size - 7
    local i = 0
    while i < pairsEnd do
        local p0 = buffer.readu32(src, i)
        local p1 = buffer.readu32(src, i + 4)
        if p0 == lastIn0 and p1 == lastIn1 then
            buffer.writeu32(dst, i, lastOut0)
            buffer.writeu32(dst, i + 4, lastOut1)
        else
            local a = bit32.rshift(p0, 24)
            local r = bit32.band(bit32.rshift(p0, 16), 0xFF)
            local g = bit32.band(bit32.rshift(p0, 8), 0xFF)
            local b = bit32.band(p0, 0xFF)
            if a > 0 and a < 255 then
                local inv = 255 / a
                r = math.floor(r * inv + 0.5)
                g = math.floor(g * inv + 0.5)
                b = math.floor(b * inv + 0.5)
                if r > 255 then
                    r = 255
                end
                if g > 255 then
                    g = 255
                end
                if b > 255 then
                    b = 255
                end
            end
            lastOut0 = bit32.bor(r, bit32.lshift(g, 8), bit32.lshift(b, 16), bit32.lshift(a, 24))
            buffer.writeu32(dst, i, lastOut0)

            a = bit32.rshift(p1, 24)
            r = bit32.band(bit32.rshift(p1, 16), 0xFF)
            g = bit32.band(bit32.rshift(p1, 8), 0xFF)
            b = bit32.band(p1, 0xFF)
            if a > 0 and a < 255 then
                local inv = 255 / a
                r = math.floor(r * inv + 0.5)
                g = math.floor(g * inv + 0.5)
                b = math.floor(b * inv + 0.5)
                if r > 255 then
                    r = 255
                end
                if g > 255 then
                    g = 255
                end
                if b > 255 then
                    b = 255
                end
            end
            lastOut1 = bit32.bor(r, bit32.lshift(g, 8), bit32.lshift(b, 16), bit32.lshift(a, 24))
            buffer.writeu32(dst, i + 4, lastOut1)

            lastIn0 = p0
            lastIn1 = p1
        end
        i = i + 8
    end

    -- Handle remainder
    while i < size do
        local pixel = buffer.readu32(src, i)
        local a = bit32.rshift(pixel, 24)
        local r = bit32.band(bit32.rshift(pixel, 16), 0xFF)
        local g = bit32.band(bit32.rshift(pixel, 8), 0xFF)
        local b = bit32.band(pixel, 0xFF)
        if a > 0 and a < 255 then
            local inv = 255 / a
            r = math.floor(r * inv + 0.5)
            g = math.floor(g * inv + 0.5)
            b = math.floor(b * inv + 0.5)
            if r > 255 then
                r = 255
            end
            if g > 255 then
                g = 255
            end
            if b > 255 then
                b = 255
            end
        end
        buffer.writeu32(dst, i, bit32.bor(r, bit32.lshift(g, 8), bit32.lshift(b, 16), bit32.lshift(a, 24)))
        i = i + 4
    end
    return dst
end

function Surface.convertRGBAtoARGB(dst, src, width, height, stride)
    for y = 0, height - 1 do
        local rowOff = y * stride
        for x = 0, width - 1 do
            local off = rowOff + x * 4
            local r = buffer.readu8(src, off)
            local g = buffer.readu8(src, off + 1)
            local b = buffer.readu8(src, off + 2)
            local a = buffer.readu8(src, off + 3)
            -- Premultiply
            if a < 255 then
                r = math.floor(r * a / 255 + 0.5)
                g = math.floor(g * a / 255 + 0.5)
                b = math.floor(b * a / 255 + 0.5)
            end
            local pixel = bit32.bor(bit32.lshift(a, 24), bit32.lshift(r, 16), bit32.lshift(g, 8), b)
            buffer.writeu32(dst, off, pixel)
        end
    end
end

return Surface
