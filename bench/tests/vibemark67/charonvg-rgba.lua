--!native
local svg = require("./charonvg/main")
local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

local Surface = svg.Surface
local Color = svg.Color
local Canvas = svg.Canvas

local W, H = 800, 600
local s = Surface.create(W, H)
s:clear(Color.WHITE)
local ctx = Canvas.create(s)

-- Create a realistic mixed surface (opaque bg + semi-transparent shapes)
ctx:setRgba(0.8, 0.2, 0.4, 0.7)
ctx:circle(400, 300, 250)
ctx:fill()
ctx:setRgba(0.2, 0.6, 1.0, 0.5)
ctx:circle(300, 250, 150)
ctx:fill()

-- Warmup
s:getRGBA()

local function test()
    for i=1,10 do
        s:getRGBA()
    end
end

bench.runCode(test, "charonvg-rgba")
