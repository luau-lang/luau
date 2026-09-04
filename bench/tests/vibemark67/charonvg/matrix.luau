--!native
local Matrix = {}
Matrix.__index = Matrix

local PI = math.pi
local DEG2RAD = PI / 180

local function makeMatrix(a, b, c, d, e, f)
    return setmetatable({ a, b, c, d, e, f }, Matrix)
end

function Matrix.create(a, b, c, d, e, f)
    return makeMatrix(a, b, c, d, e, f)
end

function Matrix.identity()
    return makeMatrix(1, 0, 0, 1, 0, 0)
end

function Matrix.translate(tx, ty)
    return makeMatrix(1, 0, 0, 1, tx, ty)
end

function Matrix.scale(sx, sy)
    return makeMatrix(sx, 0, 0, sy, 0, 0)
end

function Matrix.rotate(angle)
    local c = math.cos(angle)
    local s = math.sin(angle)
    return makeMatrix(c, s, -s, c, 0, 0)
end

function Matrix.shear(shx, shy)
    return makeMatrix(1, math.tan(shy), math.tan(shx), 1, 0, 0)
end

function Matrix:multiply(other)
    local la, lb, lc, ld, le, lf = self[1], self[2], self[3], self[4], self[5], self[6]
    local ra, rb, rc, rd, re, rf = other[1], other[2], other[3], other[4], other[5], other[6]
    return makeMatrix(
        la * ra + lb * rc,
        la * rb + lb * rd,
        lc * ra + ld * rc,
        lc * rb + ld * rd,
        le * ra + lf * rc + re,
        le * rb + lf * rd + rf
    )
end

function Matrix:invert()
    local a, b, c, d, e, f = self[1], self[2], self[3], self[4], self[5], self[6]
    local det = a * d - b * c
    if det == 0 then
        return nil
    end
    local inv_det = 1 / det
    return makeMatrix(
        d * inv_det,
        -b * inv_det,
        -c * inv_det,
        a * inv_det,
        (c * f - d * e) * inv_det,
        (b * e - a * f) * inv_det
    )
end

function Matrix:mapPoint(x, y)
    local a, b, c, d, e, f = self[1], self[2], self[3], self[4], self[5], self[6]
    return x * a + y * c + e, x * b + y * d + f
end

function Matrix:mapRect(x, y, w, h)
    local x0, y0 = self:mapPoint(x, y)
    local x1, y1 = self:mapPoint(x + w, y)
    local x2, y2 = self:mapPoint(x + w, y + h)
    local x3, y3 = self:mapPoint(x, y + h)

    local minX = math.min(x0, x1, x2, x3)
    local minY = math.min(y0, y1, y2, y3)
    local maxX = math.max(x0, x1, x2, x3)
    local maxY = math.max(y0, y1, y2, y3)

    return minX, minY, maxX - minX, maxY - minY
end

function Matrix:translated(tx, ty)
    return Matrix.translate(tx, ty):multiply(self)
end

function Matrix:scaled(sx, sy)
    return Matrix.scale(sx, sy):multiply(self)
end

function Matrix:rotated(angle)
    return Matrix.rotate(angle):multiply(self)
end

function Matrix:sheared(shx, shy)
    return Matrix.shear(shx, shy):multiply(self)
end

-- SVG transform string parsing

local function isWs(c)
    return c == 32 or c == 9 or c == 10 or c == 13
end

local function isNum(c)
    return c >= 48 and c <= 57
end

local function skipWs(str, pos)
    while pos <= #str and isWs(string.byte(str, pos)) do
        pos = pos + 1
    end
    return pos
end

local function skipWsOrComma(str, pos)
    local hadComma = false
    pos = skipWs(str, pos)
    if pos <= #str and string.byte(str, pos) == 44 then
        hadComma = true
        pos = pos + 1
        pos = skipWs(str, pos)
    end
    return pos, hadComma
end

local function parseNumber(str, pos)
    local len = #str
    local sign = 1

    if pos > len then
        return nil
    end
    local c = string.byte(str, pos)
    if c == 43 then
        pos = pos + 1
    elseif c == 45 then
        pos = pos + 1
        sign = -1
    end

    if pos > len then
        return nil
    end
    c = string.byte(str, pos)
    if c ~= 46 and not isNum(c) then
        return nil
    end

    local integer = 0
    while pos <= len do
        c = string.byte(str, pos)
        if not isNum(c) then
            break
        end
        integer = integer * 10 + (c - 48)
        pos = pos + 1
    end

    local fraction = 0
    if pos <= len and string.byte(str, pos) == 46 then
        pos = pos + 1
        if pos > len or not isNum(string.byte(str, pos)) then
            if integer == 0 and sign == 1 then
                return nil
            end
            return sign * integer, pos
        end
        local divisor = 1
        while pos <= len do
            c = string.byte(str, pos)
            if not isNum(c) then
                break
            end
            fraction = fraction * 10 + (c - 48)
            divisor = divisor * 10
            pos = pos + 1
        end
        fraction = fraction / divisor
    end

    local exponent = 0
    local expsign = 1
    if pos <= len then
        c = string.byte(str, pos)
        if c == 101 or c == 69 then
            pos = pos + 1
            if pos <= len then
                c = string.byte(str, pos)
                if c == 43 then
                    pos = pos + 1
                elseif c == 45 then
                    pos = pos + 1
                    expsign = -1
                end
            end
            if pos > len or not isNum(string.byte(str, pos)) then
                return nil
            end
            while pos <= len do
                c = string.byte(str, pos)
                if not isNum(c) then
                    break
                end
                exponent = exponent * 10 + (c - 48)
                pos = pos + 1
            end
        end
    end

    local value = sign * (integer + fraction)
    if exponent ~= 0 then
        value = value * (10 ^ (expsign * exponent))
    end
    return value, pos
end

local function parseMatrixParams(str, pos, required, optional)
    pos = skipWs(str, pos)
    if pos > #str or string.byte(str, pos) ~= 40 then
        return nil
    end
    pos = pos + 1
    pos = skipWs(str, pos)

    local values = {}
    local maxCount = required + optional
    local hadComma = false
    for i = 1, maxCount do
        local val, newPos = parseNumber(str, pos)
        if not val then
            break
        end
        values[i] = val
        pos = newPos
        pos, hadComma = skipWsOrComma(str, pos)
    end

    local count = #values
    if hadComma then
        return nil
    end
    if count ~= required and count ~= required + optional then
        return nil
    end

    pos = skipWs(str, pos)
    if pos > #str or string.byte(str, pos) ~= 41 then
        return nil
    end
    pos = pos + 1

    return values, pos, count
end

local function matchString(str, pos, target)
    local tlen = #target
    if pos + tlen - 1 > #str then
        return nil
    end
    if string.sub(str, pos, pos + tlen - 1) == target then
        return pos + tlen
    end
    return nil
end

function Matrix.parse(str)
    if type(str) ~= "string" or #str == 0 then
        return nil
    end
    local result = Matrix.identity()
    local pos = skipWs(str, 1)
    local len = #str
    local hadComma = false

    while pos <= len do
        local newPos
        newPos = matchString(str, pos, "matrix")
        if newPos then
            local values
            values, pos = parseMatrixParams(str, newPos, 6, 0)
            if not values then
                return nil
            end
            local m = makeMatrix(values[1], values[2], values[3], values[4], values[5], values[6])
            result = m:multiply(result)
        else
            newPos = matchString(str, pos, "translate")
            if newPos then
                local values, _, count
                values, pos, count = parseMatrixParams(str, newPos, 1, 1)
                if not values then
                    return nil
                end
                local tx, ty = values[1], count == 2 and values[2] or 0
                result = Matrix.translate(tx, ty):multiply(result)
            else
                newPos = matchString(str, pos, "scale")
                if newPos then
                    local values, _, count
                    values, pos, count = parseMatrixParams(str, newPos, 1, 1)
                    if not values then
                        return nil
                    end
                    local sx = values[1]
                    local sy = count == 2 and values[2] or sx
                    result = Matrix.scale(sx, sy):multiply(result)
                else
                    newPos = matchString(str, pos, "rotate")
                    if newPos then
                        local values, _, count
                        values, pos, count = parseMatrixParams(str, newPos, 1, 2)
                        if not values then
                            return nil
                        end
                        if count == 3 then
                            result = Matrix.translate(values[2], values[3]):multiply(result)
                        end
                        result = Matrix.rotate(values[1] * DEG2RAD):multiply(result)
                        if count == 3 then
                            result = Matrix.translate(-values[2], -values[3]):multiply(result)
                        end
                    else
                        newPos = matchString(str, pos, "skewX")
                        if newPos then
                            local values
                            values, pos = parseMatrixParams(str, newPos, 1, 0)
                            if not values then
                                return nil
                            end
                            result = Matrix.shear(values[1] * DEG2RAD, 0):multiply(result)
                        else
                            newPos = matchString(str, pos, "skewY")
                            if newPos then
                                local values
                                values, pos = parseMatrixParams(str, newPos, 1, 0)
                                if not values then
                                    return nil
                                end
                                result = Matrix.shear(0, values[1] * DEG2RAD):multiply(result)
                            else
                                return nil
                            end
                        end
                    end
                end
            end
        end

        pos, hadComma = skipWsOrComma(str, pos)
    end

    if hadComma then
        return nil
    end
    return result
end

return Matrix
