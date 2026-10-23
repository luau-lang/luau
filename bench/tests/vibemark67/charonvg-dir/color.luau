--!native
local Color = {}
Color.__index = Color

local function clamp(v, lo, hi)
    if v < lo then
        return lo
    end
    if v > hi then
        return hi
    end
    return v
end

local function round(v)
    return math.floor(v + 0.5)
end

local function makeColor(r, g, b, a)
    return setmetatable({ r = r, g = g, b = b, a = a }, Color)
end

function Color.rgb(r, g, b)
    return makeColor(clamp(r, 0, 1), clamp(g, 0, 1), clamp(b, 0, 1), 1)
end

function Color.rgba(r, g, b, a)
    return makeColor(clamp(r, 0, 1), clamp(g, 0, 1), clamp(b, 0, 1), clamp(a, 0, 1))
end

function Color.rgb8(r, g, b)
    return Color.rgba(r / 255, g / 255, b / 255, 1)
end

function Color.rgba8(r, g, b, a)
    return Color.rgba(r / 255, g / 255, b / 255, a / 255)
end

function Color.fromRGBA32(value)
    local r = bit32.rshift(bit32.band(value, 0xFF000000), 24)
    local g = bit32.rshift(bit32.band(value, 0x00FF0000), 16)
    local b = bit32.rshift(bit32.band(value, 0x0000FF00), 8)
    local a = bit32.band(value, 0x000000FF)
    return Color.rgba8(r, g, b, a)
end

function Color.fromARGB32(value)
    local a = bit32.rshift(bit32.band(value, 0xFF000000), 24)
    local r = bit32.rshift(bit32.band(value, 0x00FF0000), 16)
    local g = bit32.rshift(bit32.band(value, 0x0000FF00), 8)
    local b = bit32.band(value, 0x000000FF)
    return Color.rgba8(r, g, b, a)
end

local function hslComponent(h, s, l, n)
    local k = (n + h / 30) % 12
    local a = s * math.min(l, 1 - l)
    return l - a * math.max(-1, math.min(1, math.min(k - 3, 9 - k)))
end

function Color.hsl(h, s, l)
    return Color.hsla(h, s, l, 1)
end

function Color.hsla(h, s, l, a)
    h = h % 360
    if h < 0 then
        h = h + 360
    end
    s = clamp(s, 0, 1)
    l = clamp(l, 0, 1)
    local r = hslComponent(h, s, l, 0)
    local g = hslComponent(h, s, l, 8)
    local b = hslComponent(h, s, l, 4)
    return Color.rgba(r, g, b, a)
end

function Color:toRGBA32()
    local r = round(self.r * 255)
    local g = round(self.g * 255)
    local b = round(self.b * 255)
    local a = round(self.a * 255)
    return bit32.bor(bit32.lshift(r, 24), bit32.lshift(g, 16), bit32.lshift(b, 8), a)
end

function Color:toARGB32()
    local a = round(self.a * 255)
    local r = round(self.r * 255)
    local g = round(self.g * 255)
    local b = round(self.b * 255)
    return bit32.bor(bit32.lshift(a, 24), bit32.lshift(r, 16), bit32.lshift(g, 8), b)
end

-- CSS color string parsing

local NAMED_COLORS = {
    aliceblue = 0xF0F8FF,
    antiquewhite = 0xFAEBD7,
    aqua = 0x00FFFF,
    aquamarine = 0x7FFFD4,
    azure = 0xF0FFFF,
    beige = 0xF5F5DC,
    bisque = 0xFFE4C4,
    black = 0x000000,
    blanchedalmond = 0xFFEBCD,
    blue = 0x0000FF,
    blueviolet = 0x8A2BE2,
    brown = 0xA52A2A,
    burlywood = 0xDEB887,
    cadetblue = 0x5F9EA0,
    chartreuse = 0x7FFF00,
    chocolate = 0xD2691E,
    coral = 0xFF7F50,
    cornflowerblue = 0x6495ED,
    cornsilk = 0xFFF8DC,
    crimson = 0xDC143C,
    cyan = 0x00FFFF,
    darkblue = 0x00008B,
    darkcyan = 0x008B8B,
    darkgoldenrod = 0xB8860B,
    darkgray = 0xA9A9A9,
    darkgreen = 0x006400,
    darkgrey = 0xA9A9A9,
    darkkhaki = 0xBDB76B,
    darkmagenta = 0x8B008B,
    darkolivegreen = 0x556B2F,
    darkorange = 0xFF8C00,
    darkorchid = 0x9932CC,
    darkred = 0x8B0000,
    darksalmon = 0xE9967A,
    darkseagreen = 0x8FBC8F,
    darkslateblue = 0x483D8B,
    darkslategray = 0x2F4F4F,
    darkslategrey = 0x2F4F4F,
    darkturquoise = 0x00CED1,
    darkviolet = 0x9400D3,
    deeppink = 0xFF1493,
    deepskyblue = 0x00BFFF,
    dimgray = 0x696969,
    dimgrey = 0x696969,
    dodgerblue = 0x1E90FF,
    firebrick = 0xB22222,
    floralwhite = 0xFFFAF0,
    forestgreen = 0x228B22,
    fuchsia = 0xFF00FF,
    gainsboro = 0xDCDCDC,
    ghostwhite = 0xF8F8FF,
    gold = 0xFFD700,
    goldenrod = 0xDAA520,
    gray = 0x808080,
    green = 0x008000,
    greenyellow = 0xADFF2F,
    grey = 0x808080,
    honeydew = 0xF0FFF0,
    hotpink = 0xFF69B4,
    indianred = 0xCD5C5C,
    indigo = 0x4B0082,
    ivory = 0xFFFFF0,
    khaki = 0xF0E68C,
    lavender = 0xE6E6FA,
    lavenderblush = 0xFFF0F5,
    lawngreen = 0x7CFC00,
    lemonchiffon = 0xFFFACD,
    lightblue = 0xADD8E6,
    lightcoral = 0xF08080,
    lightcyan = 0xE0FFFF,
    lightgoldenrodyellow = 0xFAFAD2,
    lightgray = 0xD3D3D3,
    lightgreen = 0x90EE90,
    lightgrey = 0xD3D3D3,
    lightpink = 0xFFB6C1,
    lightsalmon = 0xFFA07A,
    lightseagreen = 0x20B2AA,
    lightskyblue = 0x87CEFA,
    lightslategray = 0x778899,
    lightslategrey = 0x778899,
    lightsteelblue = 0xB0C4DE,
    lightyellow = 0xFFFFE0,
    lime = 0x00FF00,
    limegreen = 0x32CD32,
    linen = 0xFAF0E6,
    magenta = 0xFF00FF,
    maroon = 0x800000,
    mediumaquamarine = 0x66CDAA,
    mediumblue = 0x0000CD,
    mediumorchid = 0xBA55D3,
    mediumpurple = 0x9370DB,
    mediumseagreen = 0x3CB371,
    mediumslateblue = 0x7B68EE,
    mediumspringgreen = 0x00FA9A,
    mediumturquoise = 0x48D1CC,
    mediumvioletred = 0xC71585,
    midnightblue = 0x191970,
    mintcream = 0xF5FFFA,
    mistyrose = 0xFFE4E1,
    moccasin = 0xFFE4B5,
    navajowhite = 0xFFDEAD,
    navy = 0x000080,
    oldlace = 0xFDF5E6,
    olive = 0x808000,
    olivedrab = 0x6B8E23,
    orange = 0xFFA500,
    orangered = 0xFF4500,
    orchid = 0xDA70D6,
    palegoldenrod = 0xEEE8AA,
    palegreen = 0x98FB98,
    paleturquoise = 0xAFEEEE,
    palevioletred = 0xDB7093,
    papayawhip = 0xFFEFD5,
    peachpuff = 0xFFDAB9,
    peru = 0xCD853F,
    pink = 0xFFC0CB,
    plum = 0xDDA0DD,
    powderblue = 0xB0E0E6,
    purple = 0x800080,
    rebeccapurple = 0x663399,
    red = 0xFF0000,
    rosybrown = 0xBC8F8F,
    royalblue = 0x4169E1,
    saddlebrown = 0x8B4513,
    salmon = 0xFA8072,
    sandybrown = 0xF4A460,
    seagreen = 0x2E8B57,
    seashell = 0xFFF5EE,
    sienna = 0xA0522D,
    silver = 0xC0C0C0,
    skyblue = 0x87CEEB,
    slateblue = 0x6A5ACD,
    slategray = 0x708090,
    slategrey = 0x708090,
    snow = 0xFFFAFA,
    springgreen = 0x00FF7F,
    steelblue = 0x4682B4,
    tan = 0xD2B48C,
    teal = 0x008080,
    thistle = 0xD8BFD8,
    tomato = 0xFF6347,
    turquoise = 0x40E0D0,
    violet = 0xEE82EE,
    wheat = 0xF5DEB3,
    white = 0xFFFFFF,
    whitesmoke = 0xF5F5F5,
    yellow = 0xFFFF00,
    yellowgreen = 0x9ACD32,
}

local function isHexDigit(c)
    return (c >= 48 and c <= 57) or (c >= 65 and c <= 70) or (c >= 97 and c <= 102)
end

local function hexDigit(c)
    if c >= 48 and c <= 57 then
        return c - 48
    end
    if c >= 97 and c <= 102 then
        return c - 87
    end
    if c >= 65 and c <= 70 then
        return c - 55
    end
    return 0
end

local function isAlpha(c)
    return (c >= 65 and c <= 90) or (c >= 97 and c <= 122)
end

local function isWs(c)
    return c == 32 or c == 9 or c == 10 or c == 13
end

local function skipWs(str, pos)
    while pos <= #str and isWs(string.byte(str, pos)) do
        pos = pos + 1
    end
    return pos
end

local function parseNumber(str, pos)
    local startPos = pos
    local sign = 1
    local len = #str

    if pos <= len then
        local c = string.byte(str, pos)
        if c == 43 then
            pos = pos + 1
        elseif c == 45 then
            pos = pos + 1
            sign = -1
        end
    end

    if pos > len then
        return nil
    end
    local c = string.byte(str, pos)
    if c ~= 46 and not (c >= 48 and c <= 57) then
        return nil
    end

    local integer = 0
    while pos <= len do
        c = string.byte(str, pos)
        if c < 48 or c > 57 then
            break
        end
        integer = integer * 10 + (c - 48)
        pos = pos + 1
    end

    local fraction = 0
    if pos <= len and string.byte(str, pos) == 46 then
        pos = pos + 1
        if pos > len or string.byte(str, pos) < 48 or string.byte(str, pos) > 57 then
            return nil
        end
        local divisor = 1
        while pos <= len do
            c = string.byte(str, pos)
            if c < 48 or c > 57 then
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
            if pos > len or string.byte(str, pos) < 48 or string.byte(str, pos) > 57 then
                return nil
            end
            while pos <= len do
                c = string.byte(str, pos)
                if c < 48 or c > 57 then
                    break
                end
                exponent = exponent * 10 + (c - 48)
                pos = pos + 1
            end
        end
    end

    if pos == startPos then
        return nil
    end

    local value = sign * (integer + fraction)
    if exponent ~= 0 then
        value = value * (10 ^ (expsign * exponent))
    end
    return value, pos
end

local function skipWsAndComma(str, pos)
    pos = skipWs(str, pos)
    if pos <= #str and string.byte(str, pos) == 44 then
        pos = pos + 1
        pos = skipWs(str, pos)
    end
    return pos
end

local function parseRgbComponent(str, pos)
    local value, newPos = parseNumber(str, pos)
    if not value then
        return nil
    end
    pos = newPos
    if pos <= #str and string.byte(str, pos) == 37 then
        value = value * 255 / 100
        pos = pos + 1
    end
    return clamp(value, 0, 255) / 255, pos
end

local function parseAlphaComponent(str, pos)
    local value, newPos = parseNumber(str, pos)
    if not value then
        return nil
    end
    pos = newPos
    if pos <= #str and string.byte(str, pos) == 37 then
        value = value / 100
        pos = pos + 1
    end
    return clamp(value, 0, 1), pos
end

function Color.parse(str)
    if type(str) ~= "string" or #str == 0 then
        return nil
    end
    local pos = skipWs(str, 1)
    local len = #str

    if pos <= len and string.byte(str, pos) == 35 then
        pos = pos + 1
        local hexStart = pos
        while pos <= len and isHexDigit(string.byte(str, pos)) do
            pos = pos + 1
        end
        local count = pos - hexStart
        local r, g, b, a
        if count == 3 or count == 4 then
            local c1 = hexDigit(string.byte(str, hexStart))
            local c2 = hexDigit(string.byte(str, hexStart + 1))
            local c3 = hexDigit(string.byte(str, hexStart + 2))
            r = c1 * 16 + c1
            g = c2 * 16 + c2
            b = c3 * 16 + c3
            a = 255
            if count == 4 then
                local c4 = hexDigit(string.byte(str, hexStart + 3))
                a = c4 * 16 + c4
            end
        elseif count == 6 or count == 8 then
            r = hexDigit(string.byte(str, hexStart)) * 16 + hexDigit(string.byte(str, hexStart + 1))
            g = hexDigit(string.byte(str, hexStart + 2)) * 16 + hexDigit(string.byte(str, hexStart + 3))
            b = hexDigit(string.byte(str, hexStart + 4)) * 16 + hexDigit(string.byte(str, hexStart + 5))
            a = 255
            if count == 8 then
                a = hexDigit(string.byte(str, hexStart + 6)) * 16 + hexDigit(string.byte(str, hexStart + 7))
            end
        else
            return nil
        end
        return Color.rgba8(r, g, b, a)
    end

    local nameStart = pos
    while pos <= len and isAlpha(string.byte(str, pos)) do
        pos = pos + 1
    end
    local name = string.lower(string.sub(str, nameStart, pos - 1))

    if name == "transparent" then
        return makeColor(0, 0, 0, 0)
    elseif name == "rgb" or name == "rgba" then
        pos = skipWs(str, pos)
        if pos > len or string.byte(str, pos) ~= 40 then
            return nil
        end
        pos = pos + 1
        pos = skipWs(str, pos)

        local r, g, b, a
        r, pos = parseRgbComponent(str, pos)
        if not r then
            return nil
        end
        pos = skipWsAndComma(str, pos)
        g, pos = parseRgbComponent(str, pos)
        if not g then
            return nil
        end
        pos = skipWsAndComma(str, pos)
        b, pos = parseRgbComponent(str, pos)
        if not b then
            return nil
        end

        a = 1
        local savedPos = pos
        pos = skipWsAndComma(str, pos)
        if pos ~= savedPos then
            local newA, newPos = parseAlphaComponent(str, pos)
            if newA then
                a = newA
                pos = newPos
            else
                pos = savedPos
            end
        end

        pos = skipWs(str, pos)
        if pos > len or string.byte(str, pos) ~= 41 then
            return nil
        end
        return Color.rgba(r, g, b, a)
    elseif name == "hsl" or name == "hsla" then
        pos = skipWs(str, pos)
        if pos > len or string.byte(str, pos) ~= 40 then
            return nil
        end
        pos = pos + 1
        pos = skipWs(str, pos)

        local h, s, l, a
        h, pos = parseNumber(str, pos)
        if not h then
            return nil
        end
        pos = skipWsAndComma(str, pos)
        s, pos = parseAlphaComponent(str, pos)
        if not s then
            return nil
        end
        pos = skipWsAndComma(str, pos)
        l, pos = parseAlphaComponent(str, pos)
        if not l then
            return nil
        end

        a = 1
        local savedPos = pos
        pos = skipWsAndComma(str, pos)
        if pos ~= savedPos then
            local newA, newPos = parseAlphaComponent(str, pos)
            if newA then
                a = newA
                pos = newPos
            else
                pos = savedPos
            end
        end

        pos = skipWs(str, pos)
        if pos > len or string.byte(str, pos) ~= 41 then
            return nil
        end
        return Color.hsla(h, s, l, a)
    else
        local value = NAMED_COLORS[name]
        if not value then
            return nil
        end
        local r = bit32.rshift(bit32.band(value, 0xFF0000), 16)
        local g = bit32.rshift(bit32.band(value, 0x00FF00), 8)
        local b = bit32.band(value, 0x0000FF)
        return Color.rgba8(r, g, b, 255)
    end
end

Color.BLACK = makeColor(0, 0, 0, 1)
Color.WHITE = makeColor(1, 1, 1, 1)
Color.RED = makeColor(1, 0, 0, 1)
Color.GREEN = makeColor(0, 1, 0, 1)
Color.BLUE = makeColor(0, 0, 1, 1)
Color.TRANSPARENT = makeColor(0, 0, 0, 0)

return Color
