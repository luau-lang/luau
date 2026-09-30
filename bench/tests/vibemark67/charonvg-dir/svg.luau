--!native
local Path = require("./path")
local Matrix = require("./matrix")
local Color = require("./color")
local Paint = require("./paint")
local Surface = require("./surface")
local Canvas = require("./canvas")

local Svg = {}

local PI = math.pi

-- ── Minimal XML Parser ──────────────────────────────────────────────────────

local function parseXml(str)
    local pos = 1
    local len = #str

    local function skipWs()
        while pos <= len do
            local c = string.byte(str, pos)
            if c == 32 or c == 9 or c == 10 or c == 13 then
                pos = pos + 1
            else
                break
            end
        end
    end

    local function readName()
        local start = pos
        while pos <= len do
            local c = string.byte(str, pos)
            if
                (c >= 65 and c <= 90)
                or (c >= 97 and c <= 122)
                or (c >= 48 and c <= 57)
                or c == 45
                or c == 95
                or c == 58
                or c == 46
            then
                pos = pos + 1
            else
                break
            end
        end
        if pos == start then
            return nil
        end
        return string.sub(str, start, pos - 1)
    end

    local function readAttrValue()
        local quote = string.byte(str, pos)
        if quote ~= 34 and quote ~= 39 then
            return nil
        end
        pos = pos + 1
        local start = pos
        while pos <= len and string.byte(str, pos) ~= quote do
            pos = pos + 1
        end
        local val = string.sub(str, start, pos - 1)
        pos = pos + 1
        return val
    end

    local function parseAttrs()
        local attrs = {}
        while true do
            skipWs()
            if pos > len then
                break
            end
            local c = string.byte(str, pos)
            if c == 62 or c == 47 then
                break
            end
            local name = readName()
            if not name then
                pos = pos + 1
                break
            end
            skipWs()
            if pos <= len and string.byte(str, pos) == 61 then
                pos = pos + 1
                skipWs()
                local val = readAttrValue()
                -- Normalize xlink:href to href
                if name == "xlink:href" then
                    name = "href"
                end
                attrs[name] = val or ""
            end
        end
        -- Parse inline style into individual attributes
        if attrs.style then
            for prop, val in string.gmatch(attrs.style, "([%w%-]+)%s*:%s*([^;]+)") do
                if not attrs[prop] then
                    attrs[prop] = val:match("^%s*(.-)%s*$")
                end
            end
        end
        return attrs
    end

    local function skipComment()
        local endPos = string.find(str, "-->", pos, true)
        if endPos then
            pos = endPos + 3
        else
            pos = len + 1
        end
    end

    local function skipPI()
        local endPos = string.find(str, "?>", pos, true)
        if endPos then
            pos = endPos + 2
        else
            pos = len + 1
        end
    end

    local function skipDoctype()
        local depth = 1
        while pos <= len and depth > 0 do
            local c = string.byte(str, pos)
            if c == 60 then
                depth = depth + 1
            elseif c == 62 then
                depth = depth - 1
            end
            pos = pos + 1
        end
    end

    local function parseElement()
        skipWs()
        if pos > len then
            return nil
        end

        -- Text content (skip)
        if string.byte(str, pos) ~= 60 then
            while pos <= len and string.byte(str, pos) ~= 60 do
                pos = pos + 1
            end
            return nil
        end

        pos = pos + 1 -- skip '<'
        if pos > len then
            return nil
        end

        local c = string.byte(str, pos)
        -- Comment
        if c == 33 then
            if string.sub(str, pos, pos + 1) == "!-" then
                pos = pos + 2
                skipComment()
                return nil
            end
            if string.sub(str, pos, pos + 6) == "!DOCTYP" then
                skipDoctype()
                return nil
            end
            if string.sub(str, pos, pos + 7) == "![CDATA[" then
                local endPos = string.find(str, "]]>", pos, true)
                pos = endPos and endPos + 3 or len + 1
                return nil
            end
            -- Skip other declarations
            while pos <= len and string.byte(str, pos) ~= 62 do
                pos = pos + 1
            end
            pos = pos + 1
            return nil
        end
        -- Processing instruction
        if c == 63 then
            skipPI()
            return nil
        end
        -- Closing tag
        if c == 47 then
            pos = pos + 1
            while pos <= len and string.byte(str, pos) ~= 62 do
                pos = pos + 1
            end
            pos = pos + 1
            return "CLOSE"
        end

        local tag = readName()
        if not tag then
            return nil
        end

        local attrs = parseAttrs()
        skipWs()

        local selfClose = false
        if pos <= len and string.byte(str, pos) == 47 then
            selfClose = true
            pos = pos + 1
        end
        if pos <= len and string.byte(str, pos) == 62 then
            pos = pos + 1
        end

        local node = { tag = tag, attrs = attrs, children = {}, parent = nil }

        if not selfClose then
            while pos <= len do
                local child = parseElement()
                if child == "CLOSE" then
                    break
                end
                if child then
                    child.parent = node
                    node.children[#node.children + 1] = child
                end
            end
        end

        return node
    end

    -- Skip BOM
    if string.sub(str, 1, 3) == "\239\187\191" then
        pos = 4
    end

    -- Find root element
    local root
    while pos <= len do
        local el = parseElement()
        if el and el ~= "CLOSE" then
            root = el
            break
        end
    end
    return root
end

-- ── SVG Document ────────────────────────────────────────────────────────────

local Document = {}
Document.__index = Document

local function buildIdMap(node, map)
    if node.attrs and node.attrs.id then
        map[node.attrs.id] = node
    end
    for _, child in ipairs(node.children) do
        buildIdMap(child, map)
    end
end

-- ── Value Parsers ───────────────────────────────────────────────────────────

local DPI = 96

local function parseNumber(s)
    if not s then
        return nil
    end
    return tonumber(s:match("^%s*([%d%.%-%+eE]+)"))
end

local function parseLength(s, viewDim)
    if not s then
        return nil
    end
    s = s:match("^%s*(.-)%s*$")
    local num, unit = s:match("^([%d%.%-%+eE]+)(%a*%%?)$")
    num = tonumber(num)
    if not num then
        return nil
    end
    if unit == "%" then
        return num * (viewDim or 1) / 100
    end
    if unit == "px" or unit == "" then
        return num
    end
    if unit == "pt" then
        return num * DPI / 72
    end
    if unit == "pc" then
        return num * DPI / 6
    end
    if unit == "in" then
        return num * DPI
    end
    if unit == "cm" then
        return num * DPI / 2.54
    end
    if unit == "mm" then
        return num * DPI / 25.4
    end
    return num
end

local function parseViewBox(s)
    if not s then
        return nil
    end
    local x, y, w, h = s:match("([%d%.%-%+eE]+)[%s,]+([%d%.%-%+eE]+)[%s,]+([%d%.%-%+eE]+)[%s,]+([%d%.%-%+eE]+)")
    if not x then
        return nil
    end
    return { x = tonumber(x), y = tonumber(y), w = tonumber(w), h = tonumber(h) }
end

local function parsePoints(s)
    if not s then
        return nil
    end
    local points = {}
    for x, y in s:gmatch("([%d%.%-%+eE]+)[%s,]+([%d%.%-%+eE]+)") do
        points[#points + 1] = { tonumber(x), tonumber(y) }
    end
    return points
end

local INHERIT_ATTRS = {
    fill = true,
    ["fill-opacity"] = true,
    ["fill-rule"] = true,
    stroke = true,
    ["stroke-width"] = true,
    ["stroke-opacity"] = true,
    ["stroke-linecap"] = true,
    ["stroke-linejoin"] = true,
    ["stroke-miterlimit"] = true,
    ["stroke-dasharray"] = true,
    ["stroke-dashoffset"] = true,
    opacity = true,
    color = true,
    visibility = true,
}

local function resolveVar(value, vars)
    if not value then
        return value
    end
    if not value:find("var(", 1, true) then
        return value
    end
    return (
        value:gsub("var%(%-%-([%w%-_]+)%s*,?%s*(.-)%)", function(name, fallback)
            local resolved = vars and vars[name]
            if resolved then
                return resolved
            end
            fallback = fallback:match("^%s*(.-)%s*$")
            if fallback and fallback ~= "" then
                return fallback
            end
            return ""
        end)
    )
end

local function findAttr(element, name, vars)
    local node = element
    local inherit = INHERIT_ATTRS[name]
    while node do
        local val = node.attrs and node.attrs[name]
        if val and val ~= "inherit" then
            return resolveVar(val, vars)
        end
        if not inherit then
            return nil
        end
        node = node.parent
    end
    return nil
end

-- ── Paint Resolution ────────────────────────────────────────────────────────

local function resolveGradientStops(element, idMap, depth)
    if depth > 32 then
        return {}
    end
    local stops = {}
    for _, child in ipairs(element.children) do
        if child.tag == "stop" then
            local offset = parseNumber(child.attrs.offset) or 0
            if child.attrs.offset and child.attrs.offset:find("%%") then
                offset = offset / 100
            end
            local stopColor = child.attrs["stop-color"] or findAttr(child, "stop-color") or "#000000"
            local stopOpacity = parseNumber(child.attrs["stop-opacity"] or findAttr(child, "stop-opacity")) or 1
            local c = Color.parse(stopColor) or Color.BLACK
            stops[#stops + 1] = { offset = offset, color = Color.rgba(c.r, c.g, c.b, c.a * stopOpacity) }
        end
    end
    if #stops == 0 and element.attrs.href then
        local ref = idMap[element.attrs.href:gsub("^#", "")]
        if ref then
            return resolveGradientStops(ref, idMap, depth + 1)
        end
    end
    return stops
end

local function resolveGradientAttr(element, name, idMap, depth)
    if depth > 32 then
        return nil
    end
    local val = element.attrs[name]
    if val then
        return val
    end
    if element.attrs.href then
        local ref = idMap[element.attrs.href:gsub("^#", "")]
        if ref then
            return resolveGradientAttr(ref, name, idMap, depth + 1)
        end
    end
    return nil
end

local function parseGradientCoord(str, fallback, viewDim, isBBox)
    if not str then
        return fallback
    end
    local num, pct = str:match("^%s*([%d%.%-%+eE]+)(%%?)%s*$")
    num = tonumber(num)
    if not num then
        return fallback
    end
    if pct == "%" then
        return isBBox and num / 100 or num * viewDim / 100
    end
    return num
end

local function resolveLinearGradient(element, idMap, extents, viewW, viewH)
    local units = resolveGradientAttr(element, "gradientUnits", idMap, 0) or "objectBoundingBox"
    local spread = resolveGradientAttr(element, "spreadMethod", idMap, 0) or "pad"
    local transformStr = resolveGradientAttr(element, "gradientTransform", idMap, 0)
    local isBBox = units == "objectBoundingBox"

    local x1 = parseGradientCoord(resolveGradientAttr(element, "x1", idMap, 0), isBBox and 0 or 0, viewW, isBBox)
    local y1 = parseGradientCoord(resolveGradientAttr(element, "y1", idMap, 0), 0, viewH, isBBox)
    local x2 = parseGradientCoord(resolveGradientAttr(element, "x2", idMap, 0), isBBox and 1 or 0, viewW, isBBox)
    local y2 = parseGradientCoord(resolveGradientAttr(element, "y2", idMap, 0), 0, viewH, isBBox)

    local stops = resolveGradientStops(element, idMap, 0)
    if #stops == 0 then
        return nil
    end

    local matrix = nil
    if transformStr then
        matrix = Matrix.parse(transformStr)
    end

    if units == "objectBoundingBox" and extents then
        local bbMatrix = Matrix.translate(extents.x, extents.y)
        bbMatrix = Matrix.scale(extents.w, extents.h):multiply(bbMatrix)
        if matrix then
            matrix = matrix:multiply(bbMatrix)
        else
            matrix = bbMatrix
        end
    end

    return Paint.linearGradient(x1, y1, x2, y2, spread, stops, matrix)
end

local function resolveRadialGradient(element, idMap, extents, viewW, viewH)
    local units = resolveGradientAttr(element, "gradientUnits", idMap, 0) or "objectBoundingBox"
    local spread = resolveGradientAttr(element, "spreadMethod", idMap, 0) or "pad"
    local transformStr = resolveGradientAttr(element, "gradientTransform", idMap, 0)
    local isBBox = units == "objectBoundingBox"

    local cx = parseGradientCoord(resolveGradientAttr(element, "cx", idMap, 0), isBBox and 0.5 or 0, viewW, isBBox)
    local cy = parseGradientCoord(resolveGradientAttr(element, "cy", idMap, 0), isBBox and 0.5 or 0, viewH, isBBox)
    local r = parseGradientCoord(resolveGradientAttr(element, "r", idMap, 0), isBBox and 0.5 or 0, viewW, isBBox)
    local fxStr = resolveGradientAttr(element, "fx", idMap, 0)
    local fyStr = resolveGradientAttr(element, "fy", idMap, 0)
    local fx = fxStr and parseGradientCoord(fxStr, cx, viewW, isBBox) or cx
    local fy = fyStr and parseGradientCoord(fyStr, cy, viewH, isBBox) or cy

    local stops = resolveGradientStops(element, idMap, 0)
    if #stops == 0 then
        return nil
    end

    local matrix = nil
    if transformStr then
        matrix = Matrix.parse(transformStr)
    end

    if units == "objectBoundingBox" and extents then
        local bbMatrix = Matrix.translate(extents.x, extents.y)
        bbMatrix = Matrix.scale(extents.w, extents.h):multiply(bbMatrix)
        if matrix then
            matrix = matrix:multiply(bbMatrix)
        else
            matrix = bbMatrix
        end
    end

    return Paint.radialGradient(cx, cy, r, fx, fy, 0, spread, stops, matrix)
end

local function resolvePaint(value, element, idMap, extents, viewW, viewH, vars)
    if not value or value == "none" then
        return nil
    end
    -- Resolve currentColor
    if value == "currentColor" then
        value = findAttr(element, "color", vars) or "black"
    end
    local url = value:match("url%(#([^%)]+)%)")
    if url then
        local ref = idMap[url]
        if ref then
            if ref.tag == "linearGradient" then
                return resolveLinearGradient(ref, idMap, extents, viewW, viewH)
            elseif ref.tag == "radialGradient" then
                return resolveRadialGradient(ref, idMap, extents, viewW, viewH)
            end
        end
        -- Try fallback color after url()
        local fallback = value:match("url%([^%)]+%)%s+(.+)")
        if fallback then
            return nil, Color.parse(fallback)
        end
        return nil
    end
    local c = Color.parse(value)
    return nil, c
end

-- ── ViewBox Transform ───────────────────────────────────────────────────────

local function applyViewBox(canvas, viewBox, vpWidth, vpHeight, par)
    if not viewBox then
        return
    end
    par = par or "xMidYMid meet"

    local scaleX = vpWidth / viewBox.w
    local scaleY = vpHeight / viewBox.h

    local align, meetOrSlice = par:match("(%w+)%s*(%w*)")
    if meetOrSlice == "" then
        meetOrSlice = "meet"
    end

    if align == "none" then
        canvas:translate(-viewBox.x * scaleX, -viewBox.y * scaleY)
        canvas:scale(scaleX, scaleY)
        return
    end

    local scale
    if meetOrSlice == "meet" then
        scale = math.min(scaleX, scaleY)
    else
        scale = math.max(scaleX, scaleY)
    end

    local transX = -viewBox.x * scale
    local transY = -viewBox.y * scale
    local scaledW = viewBox.w * scale
    local scaledH = viewBox.h * scale

    if align:find("xMid") then
        transX = transX + (vpWidth - scaledW) / 2
    elseif align:find("xMax") then
        transX = transX + (vpWidth - scaledW)
    end

    if align:find("YMid") then
        transY = transY + (vpHeight - scaledH) / 2
    elseif align:find("YMax") then
        transY = transY + (vpHeight - scaledH)
    end

    canvas:translate(transX, transY)
    canvas:scale(scale, scale)
end

-- ── Renderer ────────────────────────────────────────────────────────────────

local function renderElement(element, ctx, idMap, viewW, viewH, depth)
    if depth > 256 then
        return
    end
    local tag = element.tag

    local display = element.attrs.display
    if display == "none" then
        return
    end
    local visibility = findAttr(element, "visibility")

    ctx.canvas:save()

    -- Apply transform
    local transformStr = element.attrs.transform
    if transformStr then
        local m = Matrix.parse(transformStr)
        if m then
            ctx.canvas:transform(m)
        end
    end

    -- Apply opacity
    local opacityStr = element.attrs.opacity
    if opacityStr then
        local o = tonumber(opacityStr)
        if o and o < 1 then
            ctx.canvas:setOpacity(ctx.canvas:getOpacity() * o)
        end
    end

    if tag == "svg" or tag == "symbol" then
        local vb = parseViewBox(element.attrs.viewBox)
        local w = parseLength(element.attrs.width, viewW) or viewW
        local h = parseLength(element.attrs.height, viewH) or viewH
        local par = element.attrs.preserveAspectRatio
        if vb then
            applyViewBox(ctx.canvas, vb, w, h, par)
            viewW, viewH = vb.w, vb.h
        end
        for _, child in ipairs(element.children) do
            renderElement(child, ctx, idMap, viewW, viewH, depth + 1)
        end
    elseif tag == "g" then
        for _, child in ipairs(element.children) do
            renderElement(child, ctx, idMap, viewW, viewH, depth + 1)
        end
    elseif tag == "defs" then
        -- Don't render, only accessible via url(#id) or <use>
    elseif tag == "use" then
        local href = element.attrs.href
        if href then
            local refId = href:gsub("^#", "")
            local ref = idMap[refId]
            if ref then
                local x = parseLength(element.attrs.x, viewW) or 0
                local y = parseLength(element.attrs.y, viewH) or 0
                if x ~= 0 or y ~= 0 then
                    ctx.canvas:translate(x, y)
                end
                renderElement(ref, ctx, idMap, viewW, viewH, depth + 1)
            end
        end
    elseif tag == "rect" then
        if visibility == "hidden" then
            ctx.canvas:restore()
            return
        end
        local x = parseLength(element.attrs.x, viewW) or 0
        local y = parseLength(element.attrs.y, viewH) or 0
        local w = parseLength(element.attrs.width, viewW) or 0
        local h = parseLength(element.attrs.height, viewH) or 0
        local rx = parseLength(element.attrs.rx, viewW)
        local ry = parseLength(element.attrs.ry, viewH)
        if not rx and ry then
            rx = ry
        end
        if not ry and rx then
            ry = rx
        end
        if w > 0 and h > 0 then
            local p = Path.create()
            if rx and rx > 0 then
                p:addRoundRect(x, y, w, h, rx, ry)
            else
                p:addRect(x, y, w, h)
            end
            drawShape(ctx, element, p, idMap, viewW, viewH)
        end
    elseif tag == "circle" then
        if visibility == "hidden" then
            ctx.canvas:restore()
            return
        end
        local cx = parseLength(element.attrs.cx, viewW) or 0
        local cy = parseLength(element.attrs.cy, viewH) or 0
        local r = parseLength(element.attrs.r, viewW) or 0
        if r > 0 then
            local p = Path.create()
            p:addCircle(cx, cy, r)
            drawShape(ctx, element, p, idMap, viewW, viewH)
        end
    elseif tag == "ellipse" then
        if visibility == "hidden" then
            ctx.canvas:restore()
            return
        end
        local cx = parseLength(element.attrs.cx, viewW) or 0
        local cy = parseLength(element.attrs.cy, viewH) or 0
        local rx = parseLength(element.attrs.rx, viewW) or 0
        local ry = parseLength(element.attrs.ry, viewH) or 0
        if rx > 0 and ry > 0 then
            local p = Path.create()
            p:addEllipse(cx, cy, rx, ry)
            drawShape(ctx, element, p, idMap, viewW, viewH)
        end
    elseif tag == "line" then
        if visibility == "hidden" then
            ctx.canvas:restore()
            return
        end
        local x1 = parseLength(element.attrs.x1, viewW) or 0
        local y1 = parseLength(element.attrs.y1, viewH) or 0
        local x2 = parseLength(element.attrs.x2, viewW) or 0
        local y2 = parseLength(element.attrs.y2, viewH) or 0
        local p = Path.create()
        p:moveTo(x1, y1)
        p:lineTo(x2, y2)
        drawShape(ctx, element, p, idMap, viewW, viewH)
    elseif tag == "path" then
        if visibility == "hidden" then
            ctx.canvas:restore()
            return
        end
        local d = element.attrs.d
        if d then
            local p = Path.parse(d)
            if p then
                drawShape(ctx, element, p, idMap, viewW, viewH)
            end
        end
    elseif tag == "polygon" or tag == "polyline" then
        if visibility == "hidden" then
            ctx.canvas:restore()
            return
        end
        local points = parsePoints(element.attrs.points)
        if points and #points >= 2 then
            local p = Path.create()
            p:moveTo(points[1][1], points[1][2])
            for i = 2, #points do
                p:lineTo(points[i][1], points[i][2])
            end
            if tag == "polygon" then
                p:close()
            end
            drawShape(ctx, element, p, idMap, viewW, viewH)
        end
    end

    ctx.canvas:restore()
end

-- Forward declaration resolved here
function drawShape(ctx, element, path, idMap, viewW, viewH)
    local canvas = ctx.canvas
    local vars = ctx.vars

    -- Compute rough bounding box for objectBoundingBox gradients
    local extents = path:extents(false)

    -- Fill
    local fillStr = findAttr(element, "fill", vars)
    if fillStr == nil then
        fillStr = "black"
    end -- SVG default fill is black
    if fillStr ~= "none" then
        local paint, color = resolvePaint(fillStr, element, idMap, extents, viewW, viewH, vars)
        local fillOpacity = parseNumber(findAttr(element, "fill-opacity", vars)) or 1
        local fillRule = findAttr(element, "fill-rule", vars) or "nonzero"

        canvas:save()
        canvas:setFillRule(fillRule)
        if fillOpacity < 1 then
            canvas:setOpacity(canvas:getOpacity() * fillOpacity)
        end
        if paint then
            canvas:setPaint(paint)
        elseif color then
            canvas:setColor(color)
        else
            canvas:setRgb(0, 0, 0)
        end
        canvas:fillPath(path)
        canvas:restore()
    end

    -- Stroke
    local strokeStr = findAttr(element, "stroke", vars)
    if strokeStr and strokeStr ~= "none" then
        local paint, color = resolvePaint(strokeStr, element, idMap, extents, viewW, viewH, vars)
        local strokeWidth = parseLength(findAttr(element, "stroke-width", vars), viewW) or 1
        local strokeOpacity = parseNumber(findAttr(element, "stroke-opacity", vars)) or 1
        local lineCap = findAttr(element, "stroke-linecap", vars) or "butt"
        local lineJoin = findAttr(element, "stroke-linejoin", vars) or "miter"
        local miterLimit = parseNumber(findAttr(element, "stroke-miterlimit", vars)) or 4

        canvas:save()
        canvas:setLineWidth(strokeWidth)
        canvas:setLineCap(lineCap)
        canvas:setLineJoin(lineJoin)
        canvas:setMiterLimit(miterLimit)
        if strokeOpacity < 1 then
            canvas:setOpacity(canvas:getOpacity() * strokeOpacity)
        end

        local dashStr = findAttr(element, "stroke-dasharray", vars)
        if dashStr and dashStr ~= "none" then
            local dashes = {}
            for num in dashStr:gmatch("[%d%.]+") do
                dashes[#dashes + 1] = tonumber(num) or 0
            end
            if #dashes > 0 then
                local dashOffset = parseNumber(findAttr(element, "stroke-dashoffset", vars)) or 0
                canvas:setDash(dashOffset, dashes)
            end
        end

        if paint then
            canvas:setPaint(paint)
        elseif color then
            canvas:setColor(color)
        else
            canvas:setRgb(0, 0, 0)
        end
        canvas:strokePath(path)
        canvas:restore()
    end
end

-- ── Public API ──────────────────────────────────────────────────────────────

function Svg.parse(svgString)
    local root = parseXml(svgString)
    if not root or root.tag ~= "svg" then
        return nil
    end

    local idMap = {}
    buildIdMap(root, idMap)

    local doc = setmetatable({}, Document)
    doc._root = root
    doc._idMap = idMap

    -- Parse intrinsic dimensions
    local vb = parseViewBox(root.attrs.viewBox)
    local w = parseLength(root.attrs.width, vb and vb.w or 300)
    local h = parseLength(root.attrs.height, vb and vb.h or 150)
    if not w then
        w = vb and vb.w or 300
    end
    if not h then
        h = vb and vb.h or 150
    end
    doc._width = w
    doc._height = h

    return doc
end

function Svg.parseFile(filename)
    local f = io.open(filename, "r")
    if not f then
        return nil
    end
    local content = f:read("*a")
    f:close()
    return Svg.parse(content)
end

function Document:getWidth()
    return self._width
end

function Document:getHeight()
    return self._height
end

function Document:render(canvas, vars)
    local ctx = { canvas = canvas, vars = vars }
    renderElement(self._root, ctx, self._idMap, self._width, self._height, 0)
end

function Document:renderToSurface(width, height, vars)
    width = width or self._width
    height = height or self._height

    local surface = Surface.create(width, height)
    local canvas = Canvas.create(surface)

    -- Scale from intrinsic to requested size
    if width ~= self._width or height ~= self._height then
        canvas:scale(width / self._width, height / self._height)
    end

    self:render(canvas, vars)
    return surface
end

return Svg
