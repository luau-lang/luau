--!native
local Matrix = require("./matrix")

local Path = {}
Path.__index = Path

Path.MOVE_TO = 0
Path.LINE_TO = 1
Path.CUBIC_TO = 2
Path.CLOSE = 3

local MOVE_TO = Path.MOVE_TO
local LINE_TO = Path.LINE_TO
local CUBIC_TO = Path.CUBIC_TO
local CLOSE = Path.CLOSE

local PI = math.pi
local TWO_PI = PI * 2
local HALF_PI = PI / 2
local KAPPA = 0.55228474983079339840

function Path.create()
    return setmetatable({
        cmds = {},
        data = {},
        startX = 0,
        startY = 0,
        numContours = 0,
        numCurves = 0,
    }, Path)
end

function Path:moveTo(x, y)
    local cmds, data = self.cmds, self.data
    cmds[#cmds + 1] = MOVE_TO
    data[#data + 1] = x
    data[#data + 1] = y
    self.startX = x
    self.startY = y
    self.numContours = self.numContours + 1
end

function Path:lineTo(x, y)
    local cmds, data = self.cmds, self.data
    if #cmds == 0 then
        self:moveTo(x, y)
        return
    end
    cmds[#cmds + 1] = LINE_TO
    data[#data + 1] = x
    data[#data + 1] = y
end

function Path:cubicTo(x1, y1, x2, y2, x3, y3)
    local cmds, data = self.cmds, self.data
    if #cmds == 0 then
        self:moveTo(0, 0)
    end
    cmds[#cmds + 1] = CUBIC_TO
    data[#data + 1] = x1
    data[#data + 1] = y1
    data[#data + 1] = x2
    data[#data + 1] = y2
    data[#data + 1] = x3
    data[#data + 1] = y3
    self.numCurves = self.numCurves + 1
end

function Path:quadTo(x1, y1, x2, y2)
    local cx, cy = self:getCurrentPoint()
    local cp1x = cx + 2 / 3 * (x1 - cx)
    local cp1y = cy + 2 / 3 * (y1 - cy)
    local cp2x = x2 + 2 / 3 * (x1 - x2)
    local cp2y = y2 + 2 / 3 * (y1 - y2)
    self:cubicTo(cp1x, cp1y, cp2x, cp2y, x2, y2)
end

function Path:close()
    local cmds, data = self.cmds, self.data
    if #cmds == 0 then
        return
    end
    cmds[#cmds + 1] = CLOSE
    data[#data + 1] = self.startX
    data[#data + 1] = self.startY
end

function Path:getCurrentPoint()
    local data = self.data
    local n = #data
    if n < 2 then
        return 0, 0
    end
    return data[n - 1], data[n]
end

function Path:reset()
    self.cmds = {}
    self.data = {}
    self.startX = 0
    self.startY = 0
    self.numContours = 0
    self.numCurves = 0
end

-- Shape helpers

function Path:addRect(x, y, w, h)
    self:moveTo(x, y)
    self:lineTo(x + w, y)
    self:lineTo(x + w, y + h)
    self:lineTo(x, y + h)
    self:close()
end

function Path:addRoundRect(x, y, w, h, rx, ry)
    rx = math.min(rx, w / 2)
    ry = math.min(ry, h / 2)
    if rx == 0 and ry == 0 then
        self:addRect(x, y, w, h)
        return
    end

    local right = x + w
    local bottom = y + h
    local cpx = rx * KAPPA
    local cpy = ry * KAPPA

    self:moveTo(x + rx, y)
    self:lineTo(right - rx, y)
    self:cubicTo(right - rx + cpx, y, right, y + ry - cpy, right, y + ry)
    self:lineTo(right, bottom - ry)
    self:cubicTo(right, bottom - ry + cpy, right - rx + cpx, bottom, right - rx, bottom)
    self:lineTo(x + rx, bottom)
    self:cubicTo(x + rx - cpx, bottom, x, bottom - ry + cpy, x, bottom - ry)
    self:lineTo(x, y + ry)
    self:cubicTo(x, y + ry - cpy, x + rx - cpx, y, x + rx, y)
    self:close()
end

function Path:addEllipse(cx, cy, rx, ry)
    local cpx = rx * KAPPA
    local cpy = ry * KAPPA

    self:moveTo(cx, cy - ry)
    self:cubicTo(cx + cpx, cy - ry, cx + rx, cy - cpy, cx + rx, cy)
    self:cubicTo(cx + rx, cy + cpy, cx + cpx, cy + ry, cx, cy + ry)
    self:cubicTo(cx - cpx, cy + ry, cx - rx, cy + cpy, cx - rx, cy)
    self:cubicTo(cx - rx, cy - cpy, cx - cpx, cy - ry, cx, cy - ry)
    self:close()
end

function Path:addCircle(cx, cy, r)
    self:addEllipse(cx, cy, r, r)
end

function Path:addArc(cx, cy, r, a0, a1, ccw)
    local da = a1 - a0
    if not ccw then
        if da < 0 then
            da = da % TWO_PI
        end
        if da > TWO_PI then
            da = TWO_PI
        end
    else
        if da > 0 then
            da = -(-da % TWO_PI)
        end
        if da < -TWO_PI then
            da = -TWO_PI
        end
    end

    local nsegs = math.max(1, math.ceil(math.abs(da) / HALF_PI))
    local segAngle = da / nsegs

    local startX = cx + r * math.cos(a0)
    local startY = cy + r * math.sin(a0)
    if #self.cmds == 0 then
        self:moveTo(startX, startY)
    else
        self:lineTo(startX, startY)
    end

    for i = 1, nsegs do
        local angle = a0 + segAngle * i
        local prevAngle = a0 + segAngle * (i - 1)
        local halfSeg = segAngle / 2
        local t = (4 / 3) * math.tan(halfSeg / 2)

        local cos0 = math.cos(prevAngle)
        local sin0 = math.sin(prevAngle)
        local cos1 = math.cos(angle)
        local sin1 = math.sin(angle)

        local cp1x = cx + r * (cos0 - t * sin0)
        local cp1y = cy + r * (sin0 + t * cos0)
        local cp2x = cx + r * (cos1 + t * sin1)
        local cp2y = cy + r * (sin1 - t * cos1)
        local ex = cx + r * cos1
        local ey = cy + r * sin1

        self:cubicTo(cp1x, cp1y, cp2x, cp2y, ex, ey)
    end
end

-- SVG arc-to endpoint parameterization
function Path:arcTo(rx, ry, angle, largeArc, sweep, x, y)
    local cx, cy = self:getCurrentPoint()

    if rx == 0 or ry == 0 then
        self:lineTo(x, y)
        return
    end

    if cx == x and cy == y then
        return
    end

    rx = math.abs(rx)
    ry = math.abs(ry)

    local sinA = math.sin(angle)
    local cosA = math.cos(angle)

    local dx2 = (cx - x) / 2
    local dy2 = (cy - y) / 2

    local x1p = cosA * dx2 + sinA * dy2
    local y1p = -sinA * dx2 + cosA * dy2

    local x1p2 = x1p * x1p
    local y1p2 = y1p * y1p
    local rx2 = rx * rx
    local ry2 = ry * ry

    local lambda = x1p2 / rx2 + y1p2 / ry2
    if lambda > 1 then
        local s = math.sqrt(lambda)
        rx = s * rx
        ry = s * ry
        rx2 = rx * rx
        ry2 = ry * ry
    end

    local num = rx2 * ry2 - rx2 * y1p2 - ry2 * x1p2
    local den = rx2 * y1p2 + ry2 * x1p2
    if den == 0 then
        self:lineTo(x, y)
        return
    end
    local sq = math.sqrt(math.max(0, num / den))
    if largeArc == sweep then
        sq = -sq
    end

    local cxp = sq * rx * y1p / ry
    local cyp = -sq * ry * x1p / rx

    local centerX = cosA * cxp - sinA * cyp + (cx + x) / 2
    local centerY = sinA * cxp + cosA * cyp + (cy + y) / 2

    local function vecAngle(ux, uy, vx, vy)
        local dot = ux * vx + uy * vy
        local len = math.sqrt((ux * ux + uy * uy) * (vx * vx + vy * vy))
        if len == 0 then
            return 0
        end
        local cos_val = dot / len
        cos_val = math.max(-1, math.min(1, cos_val))
        local a = math.acos(cos_val)
        if ux * vy - uy * vx < 0 then
            a = -a
        end
        return a
    end

    local th1 = vecAngle(1, 0, (x1p - cxp) / rx, (y1p - cyp) / ry)
    local dth = vecAngle((x1p - cxp) / rx, (y1p - cyp) / ry, (-x1p - cxp) / rx, (-y1p - cyp) / ry)

    if not sweep and dth > 0 then
        dth = dth - TWO_PI
    elseif sweep and dth < 0 then
        dth = dth + TWO_PI
    end

    local nsegs = math.ceil(math.abs(dth) / (HALF_PI + 0.001))
    local segAngle = dth / nsegs

    for i = 1, nsegs do
        local ta = th1 + segAngle * (i - 1)
        local tb = th1 + segAngle * i
        local t = (4 / 3) * math.tan((tb - ta) / 4)

        local cos_a = math.cos(ta)
        local sin_a = math.sin(ta)
        local cos_b = math.cos(tb)
        local sin_b = math.sin(tb)

        local p1x = rx * cos_a
        local p1y = ry * sin_a
        local p2x = rx * cos_b
        local p2y = ry * sin_b

        local cp1x = p1x - t * rx * sin_a
        local cp1y = p1y + t * ry * cos_a
        local cp2x = p2x + t * rx * sin_b
        local cp2y = p2y - t * ry * cos_b

        local function transformBack(px, py)
            return cosA * px - sinA * py + centerX, sinA * px + cosA * py + centerY
        end

        local c1x, c1y = transformBack(cp1x, cp1y)
        local c2x, c2y = transformBack(cp2x, cp2y)
        local ex, ey = transformBack(p2x, p2y)

        self:cubicTo(c1x, c1y, c2x, c2y, ex, ey)
    end
end

-- Transform all points in the path
function Path:transform(matrix)
    local data = self.data
    for i = 1, #data, 2 do
        local x, y = data[i], data[i + 1]
        data[i], data[i + 1] = matrix:mapPoint(x, y)
    end
    self.startX, self.startY = matrix:mapPoint(self.startX, self.startY)
end

-- Append another path with optional transform
function Path:addPath(source, matrix)
    local srcCmds = source.cmds
    local srcData = source.data
    local di = 1
    for ci = 1, #srcCmds do
        local cmd = srcCmds[ci]
        if cmd == MOVE_TO or cmd == LINE_TO or cmd == CLOSE then
            local x, y = srcData[di], srcData[di + 1]
            if matrix then
                x, y = matrix:mapPoint(x, y)
            end
            if cmd == MOVE_TO then
                self:moveTo(x, y)
            elseif cmd == LINE_TO then
                self:lineTo(x, y)
            else
                self:close()
            end
            di = di + 2
        elseif cmd == CUBIC_TO then
            local x1, y1 = srcData[di], srcData[di + 1]
            local x2, y2 = srcData[di + 2], srcData[di + 3]
            local x3, y3 = srcData[di + 4], srcData[di + 5]
            if matrix then
                x1, y1 = matrix:mapPoint(x1, y1)
                x2, y2 = matrix:mapPoint(x2, y2)
                x3, y3 = matrix:mapPoint(x3, y3)
            end
            self:cubicTo(x1, y1, x2, y2, x3, y3)
            di = di + 6
        end
    end
end

function Path:clone()
    local new = Path.create()
    local cmds, data = self.cmds, self.data
    local nc, nd = new.cmds, new.data
    for i = 1, #cmds do
        nc[i] = cmds[i]
    end
    for i = 1, #data do
        nd[i] = data[i]
    end
    new.startX = self.startX
    new.startY = self.startY
    new.numContours = self.numContours
    new.numCurves = self.numCurves
    return new
end

-- Traverse the path, calling callback(cmd, x1, y1, ...) for each command
function Path:traverse(callback)
    local cmds = self.cmds
    local data = self.data
    local di = 1
    for ci = 1, #cmds do
        local cmd = cmds[ci]
        if cmd == MOVE_TO then
            callback(MOVE_TO, data[di], data[di + 1])
            di = di + 2
        elseif cmd == LINE_TO then
            callback(LINE_TO, data[di], data[di + 1])
            di = di + 2
        elseif cmd == CUBIC_TO then
            callback(CUBIC_TO, data[di], data[di + 1], data[di + 2], data[di + 3], data[di + 4], data[di + 5])
            di = di + 6
        elseif cmd == CLOSE then
            callback(CLOSE, data[di], data[di + 1])
            di = di + 2
        end
    end
end

-- Adaptive de Casteljau flattening
local FLATTEN_THRESHOLD = 0.25

local function flattenCubic(callback, x1, y1, x2, y2, x3, y3, x4, y4)
    -- Stack-based iterative approach (max depth 32)
    local stack = {}
    local top = 0

    top = top + 1
    stack[top] = { x1, y1, x2, y2, x3, y3, x4, y4 }

    while top > 0 do
        local b = stack[top]
        top = top - 1

        local bx1, by1, bx2, by2, bx3, by3, bx4, by4 = b[1], b[2], b[3], b[4], b[5], b[6], b[7], b[8]

        local dx = bx4 - bx1
        local dy = by4 - by1
        local l = math.sqrt(dx * dx + dy * dy)

        local flat
        if l > 1 then
            local d1 = math.abs(dx * (by2 - by1) - dy * (bx2 - bx1))
            local d2 = math.abs(dx * (by3 - by1) - dy * (bx3 - bx1))
            flat = (d1 + d2) < FLATTEN_THRESHOLD * l
        else
            local d1 = math.abs(bx2 - bx1) + math.abs(by2 - by1)
            local d2 = math.abs(bx3 - bx1) + math.abs(by3 - by1)
            flat = (d1 + d2) < FLATTEN_THRESHOLD
        end

        if flat or top >= 31 then
            callback(LINE_TO, bx4, by4)
        else
            -- de Casteljau split
            local mx12 = (bx1 + bx2) * 0.5
            local my12 = (by1 + by2) * 0.5
            local mx23 = (bx2 + bx3) * 0.5
            local my23 = (by2 + by3) * 0.5
            local mx34 = (bx3 + bx4) * 0.5
            local my34 = (by3 + by4) * 0.5
            local mx123 = (mx12 + mx23) * 0.5
            local my123 = (my12 + my23) * 0.5
            local mx234 = (mx23 + mx34) * 0.5
            local my234 = (my23 + my34) * 0.5
            local mx1234 = (mx123 + mx234) * 0.5
            local my1234 = (my123 + my234) * 0.5

            -- Push right half first (so left is processed first)
            top = top + 1
            stack[top] = { mx1234, my1234, mx234, my234, mx34, my34, bx4, by4 }
            top = top + 1
            stack[top] = { bx1, by1, mx12, my12, mx123, my123, mx1234, my1234 }
        end
    end
end

function Path:traverseFlatten(callback)
    if self.numCurves == 0 then
        self:traverse(callback)
        return
    end

    local cmds = self.cmds
    local data = self.data
    local di = 1
    local curX, curY = 0, 0

    for ci = 1, #cmds do
        local cmd = cmds[ci]
        if cmd == MOVE_TO then
            callback(MOVE_TO, data[di], data[di + 1])
            curX, curY = data[di], data[di + 1]
            di = di + 2
        elseif cmd == LINE_TO then
            callback(LINE_TO, data[di], data[di + 1])
            curX, curY = data[di], data[di + 1]
            di = di + 2
        elseif cmd == CUBIC_TO then
            local x1, y1 = data[di], data[di + 1]
            local x2, y2 = data[di + 2], data[di + 3]
            local x3, y3 = data[di + 4], data[di + 5]
            flattenCubic(callback, curX, curY, x1, y1, x2, y2, x3, y3)
            curX, curY = x3, y3
            di = di + 6
        elseif cmd == CLOSE then
            callback(CLOSE, data[di], data[di + 1])
            curX, curY = data[di], data[di + 1]
            di = di + 2
        end
    end
end

-- Dashed path traversal
function Path:traverseDashed(offset, dashes, callback)
    local ndashes = #dashes
    if ndashes == 0 then
        self:traverseFlatten(callback)
        return
    end

    local totalDash = 0
    for i = 1, ndashes do
        totalDash = totalDash + dashes[i]
    end
    if totalDash <= 0 then
        self:traverseFlatten(callback)
        return
    end

    -- Normalize offset
    offset = offset % totalDash
    if offset < 0 then
        offset = offset + totalDash
    end

    -- Find starting dash index and phase
    local dashIndex = 1
    local dashRemaining = dashes[1]
    local drawing = true

    while offset > 0 do
        if offset < dashRemaining then
            dashRemaining = dashRemaining - offset
            offset = 0
        else
            offset = offset - dashRemaining
            dashIndex = dashIndex % ndashes + 1
            dashRemaining = dashes[dashIndex]
            drawing = not drawing
        end
    end

    local curX, curY = 0, 0
    local needsMoveTo = true

    local function dashSegment(x, y)
        local dx = x - curX
        local dy = y - curY
        local dist = math.sqrt(dx * dx + dy * dy)

        if dist == 0 then
            return
        end

        while dist > 0 do
            if dist < dashRemaining then
                dashRemaining = dashRemaining - dist
                if drawing then
                    if needsMoveTo then
                        callback(MOVE_TO, curX, curY)
                        needsMoveTo = false
                    end
                    callback(LINE_TO, x, y)
                end
                curX, curY = x, y
                dist = 0
            else
                local t = dashRemaining / dist
                local splitX = curX + dx * t
                local splitY = curY + dy * t

                if drawing then
                    if needsMoveTo then
                        callback(MOVE_TO, curX, curY)
                        needsMoveTo = false
                    end
                    callback(LINE_TO, splitX, splitY)
                end

                curX, curY = splitX, splitY
                dx = x - curX
                dy = y - curY
                dist = math.sqrt(dx * dx + dy * dy)

                dashIndex = dashIndex % ndashes + 1
                dashRemaining = dashes[dashIndex]
                drawing = not drawing
                needsMoveTo = true
            end
        end
    end

    self:traverseFlatten(function(cmd, ...)
        if cmd == MOVE_TO then
            local mx, my = ...
            curX, curY = mx, my
            needsMoveTo = true
        elseif cmd == LINE_TO then
            local lx, ly = ...
            dashSegment(lx, ly)
        elseif cmd == CLOSE then
            local clx, cly = ...
            dashSegment(clx, cly)
        end
    end)
end

function Path:cloneFlatten()
    local new = Path.create()
    self:traverseFlatten(function(cmd, ...)
        if cmd == MOVE_TO then
            local x, y = ...
            new:moveTo(x, y)
        elseif cmd == LINE_TO then
            local x, y = ...
            new:lineTo(x, y)
        elseif cmd == CLOSE then
            new:close()
        end
    end)
    return new
end

function Path:cloneDashed(offset, dashes)
    local new = Path.create()
    self:traverseDashed(offset, dashes, function(cmd, ...)
        if cmd == MOVE_TO then
            local x, y = ...
            new:moveTo(x, y)
        elseif cmd == LINE_TO then
            local x, y = ...
            new:lineTo(x, y)
        elseif cmd == CLOSE then
            new:close()
        end
    end)
    return new
end

-- Bounding box and path length
function Path:extents(tight)
    local minX, minY = math.huge, math.huge
    local maxX, maxY = -math.huge, -math.huge
    local totalLength = 0
    local prevX, prevY

    local function processPoint(x, y)
        if x < minX then
            minX = x
        end
        if x > maxX then
            maxX = x
        end
        if y < minY then
            minY = y
        end
        if y > maxY then
            maxY = y
        end
    end

    local function accumulateLength(x, y)
        if prevX then
            local dx = x - prevX
            local dy = y - prevY
            totalLength = totalLength + math.sqrt(dx * dx + dy * dy)
        end
        prevX, prevY = x, y
    end

    local function doTraverse(callback)
        if tight then
            self:traverseFlatten(callback)
        else
            self:traverse(callback)
        end
    end

    doTraverse(function(cmd, ...)
        if cmd == MOVE_TO then
            local x, y = ...
            processPoint(x, y)
            prevX, prevY = x, y
        elseif cmd == LINE_TO then
            local x, y = ...
            processPoint(x, y)
            accumulateLength(x, y)
        elseif cmd == CUBIC_TO then
            local x1, y1, x2, y2, x3, y3 = ...
            processPoint(x1, y1)
            processPoint(x2, y2)
            processPoint(x3, y3)
            prevX, prevY = x3, y3
        elseif cmd == CLOSE then
            local x, y = ...
            accumulateLength(x, y)
        end
    end)

    if minX == math.huge then
        return { x = 0, y = 0, w = 0, h = 0 }, 0
    end
    return { x = minX, y = minY, w = maxX - minX, h = maxY - minY }, totalLength
end

function Path:length()
    local _, len = self:extents(true)
    return len
end

-- SVG path string parsing

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
    pos = skipWs(str, pos)
    if pos <= #str and string.byte(str, pos) == 44 then
        pos = pos + 1
        pos = skipWs(str, pos)
    end
    return pos
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

local function parseFlag(str, pos)
    pos = skipWsOrComma(str, pos)
    if pos > #str then
        return nil
    end
    local c = string.byte(str, pos)
    if c == 48 then
        return false, pos + 1
    end
    if c == 49 then
        return true, pos + 1
    end
    return nil
end

function Path.parse(str)
    if type(str) ~= "string" or #str == 0 then
        return nil
    end

    local p = Path.create()
    local pos = skipWs(str, 1)
    local len = #str
    local curX, curY = 0, 0
    local startX, startY = 0, 0
    local lastCpX, lastCpY = 0, 0
    local lastCmd = nil

    while pos <= len do
        local c = string.byte(str, pos)
        local cmd

        if (c >= 65 and c <= 90) or (c >= 97 and c <= 122) then
            cmd = string.char(c)
            pos = pos + 1
        elseif lastCmd then
            -- implicit repeat
            if lastCmd == "M" then
                cmd = "L"
            elseif lastCmd == "m" then
                cmd = "l"
            else
                cmd = lastCmd
            end
        else
            return nil
        end

        pos = skipWsOrComma(str, pos)

        if cmd == "M" or cmd == "m" then
            local x, y
            x, pos = parseNumber(str, pos)
            if not x then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            y, pos = parseNumber(str, pos)
            if not y then
                return nil
            end
            if cmd == "m" then
                x, y = curX + x, curY + y
            end
            p:moveTo(x, y)
            curX, curY = x, y
            startX, startY = x, y
            lastCpX, lastCpY = curX, curY
        elseif cmd == "L" or cmd == "l" then
            local x, y
            x, pos = parseNumber(str, pos)
            if not x then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            y, pos = parseNumber(str, pos)
            if not y then
                return nil
            end
            if cmd == "l" then
                x, y = curX + x, curY + y
            end
            p:lineTo(x, y)
            curX, curY = x, y
            lastCpX, lastCpY = curX, curY
        elseif cmd == "H" or cmd == "h" then
            local x
            x, pos = parseNumber(str, pos)
            if not x then
                return nil
            end
            if cmd == "h" then
                x = curX + x
            end
            p:lineTo(x, curY)
            curX = x
            lastCpX, lastCpY = curX, curY
        elseif cmd == "V" or cmd == "v" then
            local y
            y, pos = parseNumber(str, pos)
            if not y then
                return nil
            end
            if cmd == "v" then
                y = curY + y
            end
            p:lineTo(curX, y)
            curY = y
            lastCpX, lastCpY = curX, curY
        elseif cmd == "C" or cmd == "c" then
            local x1, y1, x2, y2, x3, y3
            x1, pos = parseNumber(str, pos)
            if not x1 then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            y1, pos = parseNumber(str, pos)
            if not y1 then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            x2, pos = parseNumber(str, pos)
            if not x2 then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            y2, pos = parseNumber(str, pos)
            if not y2 then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            x3, pos = parseNumber(str, pos)
            if not x3 then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            y3, pos = parseNumber(str, pos)
            if not y3 then
                return nil
            end
            if cmd == "c" then
                x1, y1 = curX + x1, curY + y1
                x2, y2 = curX + x2, curY + y2
                x3, y3 = curX + x3, curY + y3
            end
            p:cubicTo(x1, y1, x2, y2, x3, y3)
            lastCpX, lastCpY = x2, y2
            curX, curY = x3, y3
        elseif cmd == "S" or cmd == "s" then
            local x2, y2, x3, y3
            x2, pos = parseNumber(str, pos)
            if not x2 then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            y2, pos = parseNumber(str, pos)
            if not y2 then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            x3, pos = parseNumber(str, pos)
            if not x3 then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            y3, pos = parseNumber(str, pos)
            if not y3 then
                return nil
            end
            if cmd == "s" then
                x2, y2 = curX + x2, curY + y2
                x3, y3 = curX + x3, curY + y3
            end
            local x1 = 2 * curX - lastCpX
            local y1 = 2 * curY - lastCpY
            p:cubicTo(x1, y1, x2, y2, x3, y3)
            lastCpX, lastCpY = x2, y2
            curX, curY = x3, y3
        elseif cmd == "Q" or cmd == "q" then
            local x1, y1, x2, y2
            x1, pos = parseNumber(str, pos)
            if not x1 then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            y1, pos = parseNumber(str, pos)
            if not y1 then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            x2, pos = parseNumber(str, pos)
            if not x2 then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            y2, pos = parseNumber(str, pos)
            if not y2 then
                return nil
            end
            if cmd == "q" then
                x1, y1 = curX + x1, curY + y1
                x2, y2 = curX + x2, curY + y2
            end
            p:quadTo(x1, y1, x2, y2)
            lastCpX, lastCpY = x1, y1
            curX, curY = x2, y2
        elseif cmd == "T" or cmd == "t" then
            local x2, y2
            x2, pos = parseNumber(str, pos)
            if not x2 then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            y2, pos = parseNumber(str, pos)
            if not y2 then
                return nil
            end
            if cmd == "t" then
                x2, y2 = curX + x2, curY + y2
            end
            local x1 = 2 * curX - lastCpX
            local y1 = 2 * curY - lastCpY
            p:quadTo(x1, y1, x2, y2)
            lastCpX, lastCpY = x1, y1
            curX, curY = x2, y2
        elseif cmd == "A" or cmd == "a" then
            local rx, ry, angle, largeArc, sweep, x, y
            rx, pos = parseNumber(str, pos)
            if not rx then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            ry, pos = parseNumber(str, pos)
            if not ry then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            angle, pos = parseNumber(str, pos)
            if not angle then
                return nil
            end
            largeArc, pos = parseFlag(str, pos)
            if largeArc == nil then
                return nil
            end
            sweep, pos = parseFlag(str, pos)
            if sweep == nil then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            x, pos = parseNumber(str, pos)
            if not x then
                return nil
            end
            pos = skipWsOrComma(str, pos)
            y, pos = parseNumber(str, pos)
            if not y then
                return nil
            end
            if cmd == "a" then
                x, y = curX + x, curY + y
            end
            p:arcTo(rx, ry, angle * PI / 180, largeArc, sweep, x, y)
            curX, curY = x, y
            lastCpX, lastCpY = curX, curY
        elseif cmd == "Z" or cmd == "z" then
            p:close()
            curX, curY = startX, startY
            lastCpX, lastCpY = curX, curY
        else
            return nil
        end

        lastCmd = cmd
        pos = skipWsOrComma(str, pos)
    end

    return p
end

return Path
