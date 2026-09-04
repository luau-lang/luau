--!native
local Blend = {}


-- Premultiply a color {r,g,b,a} (0-1 floats) into a u32 ARGB pixel
local function colorToPixel(color)
    local a = math.floor(color.a * 255 + 0.5)
    local r = math.floor(color.r * a + 0.5)
    local g = math.floor(color.g * a + 0.5)
    local b = math.floor(color.b * a + 0.5)
    return bit32.bor(bit32.lshift(a, 24), bit32.lshift(r, 16), bit32.lshift(g, 8), b)
end

-- BYTE_MUL: multiply each channel of a premultiplied pixel by a 0-255 value
-- Uses the standard rounding trick: (x*a + 128) >> 8 approximates x*a/255
local function byteMul(pixel, a)
    if a == 255 then
        return pixel
    end
    if a == 0 then
        return 0
    end
    local rb = bit32.band(pixel, 0x00FF00FF)
    local ga = bit32.band(bit32.rshift(pixel, 8), 0x00FF00FF)
    rb = bit32.rshift(rb * a + 0x00800080, 8)
    rb = bit32.band(rb, 0x00FF00FF)
    ga = bit32.rshift(ga * a + 0x00800080, 8)
    ga = bit32.band(ga, 0x00FF00FF)
    return bit32.bor(rb, bit32.lshift(ga, 8))
end

-- Porter-Duff compositing operators
local function compositeClear()
    return 0
end

local function compositeSrc(dst, src)
    return src
end

local function compositeDst(dst, src)
    return dst
end

local function compositeSrcOver(dst, src)
    local srcAlpha = bit32.rshift(src, 24)
    if srcAlpha == 255 then
        return src
    end
    if srcAlpha == 0 then
        return dst
    end
    return src + byteMul(dst, 255 - srcAlpha)
end

local function compositeDstOver(dst, src)
    local dstAlpha = bit32.rshift(dst, 24)
    if dstAlpha == 255 then
        return dst
    end
    if dstAlpha == 0 then
        return src
    end
    return dst + byteMul(src, 255 - dstAlpha)
end

local function compositeSrcIn(dst, src)
    local dstAlpha = bit32.rshift(dst, 24)
    if dstAlpha == 0 then
        return 0
    end
    if dstAlpha == 255 then
        return src
    end
    return byteMul(src, dstAlpha)
end

local function compositeDstIn(dst, src)
    local srcAlpha = bit32.rshift(src, 24)
    if srcAlpha == 0 then
        return 0
    end
    if srcAlpha == 255 then
        return dst
    end
    return byteMul(dst, srcAlpha)
end

local function compositeSrcOut(dst, src)
    local dstAlpha = bit32.rshift(dst, 24)
    if dstAlpha == 0 then
        return src
    end
    if dstAlpha == 255 then
        return 0
    end
    return byteMul(src, 255 - dstAlpha)
end

local function compositeDstOut(dst, src)
    local srcAlpha = bit32.rshift(src, 24)
    if srcAlpha == 0 then
        return dst
    end
    if srcAlpha == 255 then
        return 0
    end
    return byteMul(dst, 255 - srcAlpha)
end

local function compositeSrcAtop(dst, src)
    local srcAlpha = bit32.rshift(src, 24)
    local dstAlpha = bit32.rshift(dst, 24)
    return byteMul(src, dstAlpha) + byteMul(dst, 255 - srcAlpha)
end

local function compositeDstAtop(dst, src)
    local srcAlpha = bit32.rshift(src, 24)
    local dstAlpha = bit32.rshift(dst, 24)
    return byteMul(dst, srcAlpha) + byteMul(src, 255 - dstAlpha)
end

local function compositeXor(dst, src)
    local srcAlpha = bit32.rshift(src, 24)
    local dstAlpha = bit32.rshift(dst, 24)
    return byteMul(src, 255 - dstAlpha) + byteMul(dst, 255 - srcAlpha)
end

local COMPOSITORS = {
    ["clear"] = compositeClear,
    ["src"] = compositeSrc,
    ["dst"] = compositeDst,
    ["src-over"] = compositeSrcOver,
    ["dst-over"] = compositeDstOver,
    ["src-in"] = compositeSrcIn,
    ["dst-in"] = compositeDstIn,
    ["src-out"] = compositeSrcOut,
    ["dst-out"] = compositeDstOut,
    ["src-atop"] = compositeSrcAtop,
    ["dst-atop"] = compositeDstAtop,
    ["xor"] = compositeXor,
}

local compositeSourceOver = compositeSrcOver

-- Blend solid color spans onto a surface
-- This is the minimal blend needed for filled shapes with src-over
function Blend.solidSrcOver(surface, spans, color, opacity)
    local pixel = colorToPixel(color)
    local data = surface:getData()
    local stride = surface:getStride()
    local width = surface:getWidth()
    local height = surface:getHeight()

    for i = 1, #spans do
        local span = spans[i]
        local sx = span.x
        local sy = span.y
        local len = span.len
        local coverage = span.coverage

        if sy >= 0 and sy < height then
            -- Apply opacity to coverage
            local constAlpha = coverage
            if opacity and opacity < 1 then
                constAlpha = math.floor(coverage * opacity + 0.5)
            end

            if constAlpha > 0 then
                local rowOffset = sy * stride
                local startX = math.max(0, sx)
                local endX = math.min(width, sx + len)

                if constAlpha == 255 and bit32.rshift(pixel, 24) == 255 then
                    for x = startX, endX - 1 do
                        buffer.writeu32(data, rowOffset + x * 4, pixel)
                    end
                else
                    local src = byteMul(pixel, constAlpha)
                    local srcAlpha = bit32.rshift(src, 24)
                    for x = startX, endX - 1 do
                        local offset = rowOffset + x * 4
                        local dst = buffer.readu32(data, offset)
                        buffer.writeu32(data, offset, src + byteMul(dst, 255 - srcAlpha))
                    end
                end
            end
        end
    end
end

-- Blend with operator "src" (replace destination)
function Blend.solidSrc(surface, spans, color, opacity)
    local pixel = colorToPixel(color)
    local data = surface:getData()
    local stride = surface:getStride()
    local width = surface:getWidth()
    local height = surface:getHeight()

    for i = 1, #spans do
        local span = spans[i]
        local sx = span.x
        local sy = span.y
        local len = span.len
        local coverage = span.coverage

        if sy >= 0 and sy < height then
            local constAlpha = coverage
            if opacity and opacity < 1 then
                constAlpha = math.floor(coverage * opacity + 0.5)
            end

            if constAlpha > 0 then
                local src = byteMul(pixel, constAlpha)
                local rowOffset = sy * stride
                local startX = math.max(0, sx)
                local endX = math.min(width, sx + len)

                if constAlpha == 255 then
                    for x = startX, endX - 1 do
                        buffer.writeu32(data, rowOffset + x * 4, pixel)
                    end
                else
                    local dstFactor = 255 - constAlpha
                    for x = startX, endX - 1 do
                        local offset = rowOffset + x * 4
                        local dst = buffer.readu32(data, offset)
                        local result = src + byteMul(dst, dstFactor)
                        buffer.writeu32(data, offset, result)
                    end
                end
            end
        end
    end
end

-- Paint-based blending (gradient/texture support)
function Blend.paintSrcOver(surface, spans, paint, opacity)
    if paint.type == "solid" then
        Blend.solidSrcOver(surface, spans, paint.color, opacity)
        return
    end

    local data = surface:getData()
    local stride = surface:getStride()
    local width = surface:getWidth()
    local height = surface:getHeight()

    local isLinear = paint.type == "linear"
    local isRadial = paint.type == "radial"
    if not isLinear and not isRadial then
        return
    end

    -- Pre-extract paint fields to avoid repeated table lookups in the inner loop
    local colorTable = paint.colorTable
    local spread = paint.spread
    local paintMatrix = paint.matrix
    local invMatrix = paintMatrix and paintMatrix:invert()

    -- Linear gradient constants
    local gx1, gy1, gdx, gdy, glen2
    if isLinear then
        gx1 = paint.x1
        gy1 = paint.y1
        gdx = paint.x2 - paint.x1
        gdy = paint.y2 - paint.y1
        glen2 = gdx * gdx + gdy * gdy
    end
    -- Radial gradient constants
    local rcx, rcy, rcr, rfx, rfy, rfr
    if isRadial then
        rcx = paint.cx
        rcy = paint.cy
        rcr = paint.cr
        rfx = paint.fx
        rfy = paint.fy
        rfr = paint.fr or 0
    end

    local COLOR_TABLE_SIZE = 1024
    local mathFloor = math.floor
    local mathSqrt = math.sqrt
    local mathAbs = math.abs

    for i = 1, #spans do
        local span = spans[i]
        local sx = span.x
        local sy = span.y
        local slen = span.len
        local coverage = span.coverage

        if sy >= 0 and sy < height then
            local constAlpha = coverage
            if opacity and opacity < 1 then
                constAlpha = math.floor(coverage * opacity + 0.5)
            end

            if constAlpha > 0 then
                local rowOffset = sy * stride
                local startX = math.max(0, sx)
                local endX = math.min(width, sx + slen)

                for x = startX, endX - 1 do
                    local px, py = x + 0.5, sy + 0.5
                    if invMatrix then
                        px, py = invMatrix:mapPoint(px, py)
                    end

                    local pos
                    if isLinear then
                        if glen2 < 1e-10 then
                            pos = 0
                        else
                            local t = ((px - gx1) * gdx + (py - gy1) * gdy) / glen2
                            pos = mathFloor(t * 1023 + 0.5)
                        end
                    else
                        local dx = px - rfx
                        local dy = py - rfy
                        local dr = rcr - rfr
                        local dcx = rcx - rfx
                        local dcy = rcy - rfy
                        local a = dcx * dcx + dcy * dcy - dr * dr
                        local b = dx * dcx + dy * dcy + rfr * dr
                        local c = dx * dx + dy * dy - rfr * rfr
                        local t
                        if mathAbs(a) < 1e-10 then
                            t = mathAbs(b) < 1e-10 and 0 or c / (2 * b)
                        else
                            local det = b * b - a * c
                            if det < 0 then
                                det = 0
                            end
                            local sd = mathSqrt(det)
                            t = a > 0 and (b + sd) / a or (b - sd) / a
                        end
                        if t < 0 then
                            t = 0
                        end
                        pos = mathFloor(t * 1023 + 0.5)
                    end

                    -- Spread clamping
                    if spread == "pad" then
                        if pos < 0 then
                            pos = 0
                        elseif pos >= COLOR_TABLE_SIZE then
                            pos = COLOR_TABLE_SIZE - 1
                        end
                    elseif spread == "repeat" then
                        pos = pos % COLOR_TABLE_SIZE
                        if pos < 0 then
                            pos = pos + COLOR_TABLE_SIZE
                        end
                    elseif spread == "reflect" then
                        pos = pos % (COLOR_TABLE_SIZE * 2)
                        if pos < 0 then
                            pos = pos + COLOR_TABLE_SIZE * 2
                        end
                        if pos >= COLOR_TABLE_SIZE then
                            pos = COLOR_TABLE_SIZE * 2 - 1 - pos
                        end
                    end

                    local src = colorTable[mathFloor(pos) + 1]
                    src = byteMul(src, constAlpha)
                    local srcAlpha = bit32.rshift(src, 24)
                    if srcAlpha > 0 then
                        local offset = rowOffset + x * 4
                        local dst = buffer.readu32(data, offset)
                        buffer.writeu32(data, offset, src + byteMul(dst, 255 - srcAlpha))
                    end
                end
            end
        end
    end
end

-- General solid blend with any operator
function Blend.solid(surface, spans, color, opacity, operator)
    local comp = COMPOSITORS[operator]
    if not comp or operator == "src-over" then
        return Blend.solidSrcOver(surface, spans, color, opacity)
    end
    if operator == "src" then
        return Blend.solidSrc(surface, spans, color, opacity)
    end

    local pixel = colorToPixel(color)
    local data = surface:getData()
    local stride = surface:getStride()
    local width = surface:getWidth()
    local height = surface:getHeight()

    for i = 1, #spans do
        local span = spans[i]
        local sx = span.x
        local sy = span.y
        local slen = span.len
        local coverage = span.coverage

        if sy >= 0 and sy < height then
            local constAlpha = coverage
            if opacity and opacity < 1 then
                constAlpha = math.floor(coverage * opacity + 0.5)
            end
            if constAlpha > 0 then
                local src = byteMul(pixel, constAlpha)
                local rowOffset = sy * stride
                local startX = math.max(0, sx)
                local endX = math.min(width, sx + slen)
                for x = startX, endX - 1 do
                    local offset = rowOffset + x * 4
                    local dst = buffer.readu32(data, offset)
                    buffer.writeu32(data, offset, comp(dst, src))
                end
            end
        end
    end
end

Blend.COMPOSITORS = COMPOSITORS
Blend.colorToPixel = colorToPixel
Blend.byteMul = byteMul

return Blend
