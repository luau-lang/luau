--!native
local Surface = require("./surface")
local Path = require("./path")
local Matrix = require("./matrix")
local Color = require("./color")
local Paint = require("./paint")
local Rasterizer = require("./rasterizer")
local Blend = require("./blend")
local Stroker = require("./stroker")

local Canvas = {}
Canvas.__index = Canvas

local function createState()
    return {
        matrix = Matrix.identity(),
        color = Color.BLACK,
        paint = nil,
        fillRule = "nonzero",
        operator = "src-over",
        opacity = 1,
        lineWidth = 1,
        lineCap = "butt",
        lineJoin = "miter",
        miterLimit = 10,
        dashOffset = 0,
        dashArray = nil,
        clipSpans = nil,
        clipping = false,
    }
end

local function copyState(state)
    local new = {}
    for k, v in pairs(state) do
        new[k] = v
    end
    if state.dashArray then
        local arr = {}
        for i, v in ipairs(state.dashArray) do
            arr[i] = v
        end
        new.dashArray = arr
    end
    return new
end

function Canvas.create(surface)
    local self = setmetatable({}, Canvas)
    self._surface = surface
    self._path = Path.create()
    self._state = createState()
    self._stateStack = {}
    self._clipRect = { x = 0, y = 0, w = surface:getWidth(), h = surface:getHeight() }
    return self
end

-- State management

function Canvas:save()
    self._stateStack[#self._stateStack + 1] = copyState(self._state)
end

function Canvas:restore()
    local n = #self._stateStack
    if n > 0 then
        self._state = self._stateStack[n]
        self._stateStack[n] = nil
    end
end

-- Paint

function Canvas:setRgb(r, g, b)
    self._state.color = Color.rgba(r, g, b, 1)
    self._state.paint = nil
end

function Canvas:setRgba(r, g, b, a)
    self._state.color = Color.rgba(r, g, b, a)
    self._state.paint = nil
end

function Canvas:setColor(color)
    self._state.color = color
    self._state.paint = nil
end

function Canvas:setLinearGradient(x1, y1, x2, y2, spread, stops, matrix)
    self._state.paint = Paint.linearGradient(x1, y1, x2, y2, spread, stops, matrix)
end

function Canvas:setRadialGradient(cx, cy, cr, fx, fy, fr, spread, stops, matrix)
    self._state.paint = Paint.radialGradient(cx, cy, cr, fx, fy, fr, spread, stops, matrix)
end

function Canvas:setPaint(paint)
    self._state.paint = paint
end

function Canvas:getPaint()
    return self._state.paint, self._state.color
end

-- Stroke style

function Canvas:setLineWidth(width)
    self._state.lineWidth = width
end

function Canvas:getLineWidth()
    return self._state.lineWidth
end

function Canvas:setLineCap(cap)
    self._state.lineCap = cap
end

function Canvas:getLineCap()
    return self._state.lineCap
end

function Canvas:setLineJoin(join)
    self._state.lineJoin = join
end

function Canvas:getLineJoin()
    return self._state.lineJoin
end

function Canvas:setMiterLimit(limit)
    self._state.miterLimit = limit
end

function Canvas:getMiterLimit()
    return self._state.miterLimit
end

function Canvas:setDash(offset, dashes)
    self._state.dashOffset = offset
    self._state.dashArray = dashes
end

function Canvas:setDashOffset(offset)
    self._state.dashOffset = offset
end

function Canvas:getDashOffset()
    return self._state.dashOffset
end

function Canvas:setDashArray(dashes)
    self._state.dashArray = dashes
end

function Canvas:getDashArray()
    return self._state.dashArray
end

-- Fill rule & compositing

function Canvas:setFillRule(rule)
    self._state.fillRule = rule
end

function Canvas:getFillRule()
    return self._state.fillRule
end

function Canvas:setOperator(op)
    self._state.operator = op
end

function Canvas:getOperator()
    return self._state.operator
end

function Canvas:setOpacity(opacity)
    self._state.opacity = opacity
end

function Canvas:getOpacity()
    return self._state.opacity
end

-- Transform

function Canvas:translate(tx, ty)
    self._state.matrix = Matrix.translate(tx, ty):multiply(self._state.matrix)
end

function Canvas:scale(sx, sy)
    self._state.matrix = Matrix.scale(sx, sy):multiply(self._state.matrix)
end

function Canvas:rotate(angle)
    self._state.matrix = Matrix.rotate(angle):multiply(self._state.matrix)
end

function Canvas:shear(shx, shy)
    self._state.matrix = Matrix.shear(shx, shy):multiply(self._state.matrix)
end

function Canvas:transform(matrix)
    self._state.matrix = matrix:multiply(self._state.matrix)
end

function Canvas:setMatrix(matrix)
    self._state.matrix = matrix
end

function Canvas:getMatrix()
    return self._state.matrix
end

function Canvas:resetMatrix()
    self._state.matrix = Matrix.identity()
end

function Canvas:map(x, y)
    return self._state.matrix:mapPoint(x, y)
end

-- Path building

function Canvas:moveTo(x, y)
    self._path:moveTo(x, y)
end

function Canvas:lineTo(x, y)
    self._path:lineTo(x, y)
end

function Canvas:quadTo(x1, y1, x2, y2)
    self._path:quadTo(x1, y1, x2, y2)
end

function Canvas:cubicTo(x1, y1, x2, y2, x3, y3)
    self._path:cubicTo(x1, y1, x2, y2, x3, y3)
end

function Canvas:arcTo(rx, ry, angle, largeArc, sweep, x, y)
    self._path:arcTo(rx, ry, angle, largeArc, sweep, x, y)
end

function Canvas:rect(x, y, w, h)
    self._path:addRect(x, y, w, h)
end

function Canvas:roundRect(x, y, w, h, rx, ry)
    self._path:addRoundRect(x, y, w, h, rx, ry)
end

function Canvas:ellipse(cx, cy, rx, ry)
    self._path:addEllipse(cx, cy, rx, ry)
end

function Canvas:circle(cx, cy, r)
    self._path:addCircle(cx, cy, r)
end

function Canvas:arc(cx, cy, r, a0, a1, ccw)
    self._path:addArc(cx, cy, r, a0, a1, ccw)
end

function Canvas:addPath(path)
    self._path:addPath(path)
end

function Canvas:newPath()
    self._path:reset()
end

function Canvas:closePath()
    self._path:close()
end

function Canvas:getCurrentPoint()
    return self._path:getCurrentPoint()
end

function Canvas:getPath()
    return self._path
end

-- Internal rendering helpers

local function blendSpans(self, spans)
    local state = self._state
    local opacity = state.opacity

    if state.paint then
        Blend.paintSrcOver(self._surface, spans, state.paint, opacity)
    else
        Blend.solid(self._surface, spans, state.color, opacity, state.operator)
    end
end

local function rasterizeFill(self, path)
    local state = self._state
    local spans = Rasterizer.rasterize(path, state.matrix, self._clipRect, state.fillRule)
    if state.clipping and state.clipSpans then
        spans = Rasterizer.intersectSpans(spans, state.clipSpans)
    end
    return spans
end

local function rasterizeStroke(self, path)
    local state = self._state
    local strokePath = path
    if state.dashArray and #state.dashArray > 0 then
        strokePath = path:cloneDashed(state.dashOffset, state.dashArray)
    end
    local stroked = Stroker.stroke(strokePath, state.lineWidth, state.lineCap, state.lineJoin, state.miterLimit)
    local spans = Rasterizer.rasterize(stroked, state.matrix, self._clipRect, "nonzero")
    if state.clipping and state.clipSpans then
        spans = Rasterizer.intersectSpans(spans, state.clipSpans)
    end
    return spans
end

-- Drawing operations

function Canvas:fillPreserve()
    local spans = rasterizeFill(self, self._path)
    blendSpans(self, spans)
end

function Canvas:strokePreserve()
    local spans = rasterizeStroke(self, self._path)
    blendSpans(self, spans)
end

function Canvas:clipPreserve()
    local spans = rasterizeFill(self, self._path)
    local state = self._state
    if state.clipping and state.clipSpans then
        spans = Rasterizer.intersectSpans(spans, state.clipSpans)
    end
    state.clipSpans = spans
    state.clipping = true
end

function Canvas:fill()
    self:fillPreserve()
    self._path:reset()
end

function Canvas:stroke()
    self:strokePreserve()
    self._path:reset()
end

function Canvas:clip()
    self:clipPreserve()
    self._path:reset()
end

function Canvas:paint()
    local state = self._state
    local w = self._surface:getWidth()
    local h = self._surface:getHeight()
    local spans = {}
    for y = 0, h - 1 do
        spans[#spans + 1] = { x = 0, y = y, len = w, coverage = 255 }
    end
    if state.clipping and state.clipSpans then
        spans = Rasterizer.intersectSpans(spans, state.clipSpans)
    end
    blendSpans(self, spans)
end

-- Convenience drawing

function Canvas:fillRect(x, y, w, h)
    self._path:reset()
    self._path:addRect(x, y, w, h)
    self:fill()
end

function Canvas:strokeRect(x, y, w, h)
    self._path:reset()
    self._path:addRect(x, y, w, h)
    self:stroke()
end

function Canvas:clipRect(x, y, w, h)
    self._path:reset()
    self._path:addRect(x, y, w, h)
    self:clip()
end

function Canvas:fillPath(path)
    self._path:reset()
    self._path:addPath(path)
    self:fill()
end

function Canvas:strokePath(path)
    self._path:reset()
    self._path:addPath(path)
    self:stroke()
end

function Canvas:clipPath(path)
    self._path:reset()
    self._path:addPath(path)
    self:clip()
end

-- Hit testing

function Canvas:fillContains(x, y)
    local spans = rasterizeFill(self, self._path)
    local ix = math.floor(x)
    local iy = math.floor(y)
    for _, span in ipairs(spans) do
        if span.y == iy and ix >= span.x and ix < span.x + span.len then
            return true
        end
    end
    return false
end

function Canvas:strokeContains(x, y)
    local spans = rasterizeStroke(self, self._path)
    local ix = math.floor(x)
    local iy = math.floor(y)
    for _, span in ipairs(spans) do
        if span.y == iy and ix >= span.x and ix < span.x + span.len then
            return true
        end
    end
    return false
end

-- Extents

function Canvas:fillExtents()
    local spans = rasterizeFill(self, self._path)
    if #spans == 0 then
        return { x = 0, y = 0, w = 0, h = 0 }
    end
    local minX, minY = math.huge, spans[1].y
    local maxX, maxY = -math.huge, spans[#spans].y
    for _, s in ipairs(spans) do
        if s.x < minX then
            minX = s.x
        end
        if s.x + s.len > maxX then
            maxX = s.x + s.len
        end
    end
    return { x = minX, y = minY, w = maxX - minX, h = maxY - minY + 1 }
end

function Canvas:strokeExtents()
    local spans = rasterizeStroke(self, self._path)
    if #spans == 0 then
        return { x = 0, y = 0, w = 0, h = 0 }
    end
    local minX, minY = math.huge, spans[1].y
    local maxX, maxY = -math.huge, spans[#spans].y
    for _, s in ipairs(spans) do
        if s.x < minX then
            minX = s.x
        end
        if s.x + s.len > maxX then
            maxX = s.x + s.len
        end
    end
    return { x = minX, y = minY, w = maxX - minX, h = maxY - minY + 1 }
end

-- Surface access

function Canvas:getSurface()
    return self._surface
end

return Canvas
