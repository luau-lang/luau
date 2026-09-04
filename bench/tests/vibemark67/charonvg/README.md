# CharonVG

CharonVG is a standalone 2D vector graphics library for Luau, based on [PlutoVG](https://github.com/sammycage/plutovg). It provides the same rendering capabilities — anti-aliased path filling and stroking, gradients, compositing, transforms, and SVG parsing — reimplemented in pure Luau with an idiomatic method-call API.

## Features

- Path Filling, Stroking and Dashing
- Solid, Linear Gradient and Radial Gradient Paints
- All 12 Porter-Duff Compositing Operators
- Anti-Aliased Rendering (exact-coverage scanline rasterizer)
- Clipping
- Affine Transformations
- SVG Parsing and Rendering
- CSS Custom Properties (`var(--name, fallback)`)
- BMP Output

## Example

```lua
local cvg = require("./main")

local surface = cvg.Surface.create(150, 150)
surface:clear(cvg.Color.WHITE)
local canvas = cvg.Canvas.create(surface)

-- Yellow face
canvas:arc(75, 75, 70, 0, math.pi * 2, false)
canvas:setRgb(1, 1, 0)
canvas:fillPreserve()
canvas:setRgb(0, 0, 0)
canvas:setLineWidth(5)
canvas:stroke()

-- Eyes
canvas:circle(50, 55, 10)
canvas:circle(100, 55, 10)
canvas:setRgb(0, 0, 0)
canvas:fill()

-- Mouth
canvas:arc(75, 75, 50, 0, math.pi, false)
canvas:setRgb(0, 0, 0)
canvas:setLineWidth(5)
canvas:stroke()

cvg.Bmp.write(surface, "smiley.bmp")
```

## SVG Rendering

```lua
local cvg = require("./main")

local doc = cvg.Svg.parse(svgString)
local surface = doc:renderToSurface(800, 600)
cvg.Bmp.write(surface, "output.bmp")
```

SVG custom properties can be themed at render time:

```lua
local surface = doc:renderToSurface(800, 600, {
    bg = "#1a1a2e",
    primary = "#e94560",
})
```

## Attribution

CharonVG is a Luau port of [PlutoVG](https://github.com/sammycage/plutovg) by Samuel Ugochukwu. The rendering algorithms — including the exact-coverage scanline rasterizer, path stroker, and gradient interpolation — are derived from PlutoVG's C implementation, adapted for Luau's idioms (method syntax, garbage collection, buffer type).

The SVG parser is based on [PlutoSVG](https://github.com/sammycage/plutosvg) by the same author.

## License

MIT — see [LICENSE](LICENSE) for details.
