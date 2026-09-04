--!native
local Color = require("./color")
local Matrix = require("./matrix")

local Paint = {}
Paint.__index = Paint


local COLOR_TABLE_SIZE = 1024

local function premultiply(r, g, b, a)
    local a8 = math.floor(a * 255 + 0.5)
    local r8 = math.floor(r * a8 + 0.5)
    local g8 = math.floor(g * a8 + 0.5)
    local b8 = math.floor(b * a8 + 0.5)
    return bit32.bor(bit32.lshift(a8, 24), bit32.lshift(r8, 16), bit32.lshift(g8, 8), b8)
end

local function interpolatePixel(p1, p2, t)
    local it = 255 - t
    local rb1 = bit32.band(p1, 0x00FF00FF)
    local ga1 = bit32.band(bit32.rshift(p1, 8), 0x00FF00FF)
    local rb2 = bit32.band(p2, 0x00FF00FF)
    local ga2 = bit32.band(bit32.rshift(p2, 8), 0x00FF00FF)
    local rb = bit32.rshift(rb1 * it + rb2 * t + 0x00800080, 8)
    local ga = bit32.rshift(ga1 * it + ga2 * t + 0x00800080, 8)
    rb = bit32.band(rb, 0x00FF00FF)
    ga = bit32.band(ga, 0x00FF00FF)
    return bit32.bor(rb, bit32.lshift(ga, 8))
end

local function buildColorTable(stops)
    local table = {}
    if #stops == 0 then
        for i = 1, COLOR_TABLE_SIZE do
            table[i] = 0
        end
        return table
    end
    if #stops == 1 then
        local c = stops[1].color
        local pixel = premultiply(c.r, c.g, c.b, c.a)
        for i = 1, COLOR_TABLE_SIZE do
            table[i] = pixel
        end
        return table
    end

    local pixels = {}
    for i, stop in ipairs(stops) do
        local c = stop.color
        pixels[i] = premultiply(c.r, c.g, c.b, c.a)
    end

    local si = 1
    for i = 1, COLOR_TABLE_SIZE do
        local pos = (i - 1) / (COLOR_TABLE_SIZE - 1)
        while si < #stops - 1 and pos > stops[si + 1].offset do
            si = si + 1
        end
        local s0 = stops[si]
        local s1 = stops[si + 1] or stops[si]
        local range = s1.offset - s0.offset
        local t
        if range <= 0 then
            t = 0
        else
            t = (pos - s0.offset) / range
            if t < 0 then
                t = 0
            end
            if t > 1 then
                t = 1
            end
        end
        table[i] = interpolatePixel(pixels[si], pixels[si + 1] or pixels[si], math.floor(t * 255 + 0.5))
    end
    return table
end

local function gradientClamp(pos, spread)
    if spread == "pad" then
        if pos < 0 then
            return 0
        end
        if pos >= COLOR_TABLE_SIZE then
            return COLOR_TABLE_SIZE - 1
        end
        return pos
    elseif spread == "repeat" then
        pos = pos % COLOR_TABLE_SIZE
        if pos < 0 then
            pos = pos + COLOR_TABLE_SIZE
        end
        return pos
    elseif spread == "reflect" then
        pos = pos % (COLOR_TABLE_SIZE * 2)
        if pos < 0 then
            pos = pos + COLOR_TABLE_SIZE * 2
        end
        if pos >= COLOR_TABLE_SIZE then
            pos = COLOR_TABLE_SIZE * 2 - 1 - pos
        end
        return pos
    end
    return pos
end

-- Paint constructors

function Paint.rgb(r, g, b)
    return Paint.color(Color.rgba(r, g, b, 1))
end

function Paint.rgba(r, g, b, a)
    return Paint.color(Color.rgba(r, g, b, a))
end

function Paint.color(c)
    return setmetatable({
        type = "solid",
        color = c,
    }, Paint)
end

function Paint.linearGradient(x1, y1, x2, y2, spread, stops, matrix)
    spread = spread or "pad"
    local colorTable = buildColorTable(stops)
    return setmetatable({
        type = "linear",
        x1 = x1,
        y1 = y1,
        x2 = x2,
        y2 = y2,
        spread = spread,
        colorTable = colorTable,
        matrix = matrix,
    }, Paint)
end

function Paint.radialGradient(cx, cy, cr, fx, fy, fr, spread, stops, matrix)
    spread = spread or "pad"
    local colorTable = buildColorTable(stops)
    return setmetatable({
        type = "radial",
        cx = cx,
        cy = cy,
        cr = cr,
        fx = fx,
        fy = fy,
        fr = fr,
        spread = spread,
        colorTable = colorTable,
        matrix = matrix,
    }, Paint)
end

-- Gradient fetch functions

function Paint:fetchLinear(x, y)
    local px, py = x + 0.5, y + 0.5
    if self.matrix then
        local inv = self.matrix:invert()
        if inv then
            px, py = inv:mapPoint(px, py)
        end
    end
    local dx = self.x2 - self.x1
    local dy = self.y2 - self.y1
    local len2 = dx * dx + dy * dy
    if len2 < 1e-10 then
        return self.colorTable[1]
    end
    local t = ((px - self.x1) * dx + (py - self.y1) * dy) / len2
    local pos = math.floor(t * (COLOR_TABLE_SIZE - 1) + 0.5)
    pos = gradientClamp(pos, self.spread)
    return self.colorTable[math.floor(pos) + 1]
end

function Paint:fetchRadial(x, y)
    local px, py = x + 0.5, y + 0.5
    if self.matrix then
        local inv = self.matrix:invert()
        if inv then
            px, py = inv:mapPoint(px, py)
        end
    end
    local dx = px - self.fx
    local dy = py - self.fy
    local dr = self.cr - self.fr
    local dcx = self.cx - self.fx
    local dcy = self.cy - self.fy

    local a = dcx * dcx + dcy * dcy - dr * dr
    local b = dx * dcx + dy * dcy + self.fr * dr
    local c = dx * dx + dy * dy - self.fr * self.fr

    local t
    if math.abs(a) < 1e-10 then
        if math.abs(b) < 1e-10 then
            t = 0
        else
            t = c / (2 * b)
        end
    else
        local det = b * b - a * c
        if det < 0 then
            det = 0
        end
        local sd = math.sqrt(det)
        if a > 0 then
            t = (b + sd) / a
        else
            t = (b - sd) / a
        end
    end
    if t < 0 then
        t = 0
    end

    local pos = math.floor(t * (COLOR_TABLE_SIZE - 1) + 0.5)
    pos = gradientClamp(pos, self.spread)
    return self.colorTable[math.floor(pos) + 1]
end

return Paint
