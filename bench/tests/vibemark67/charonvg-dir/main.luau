--!native
local charonvg = {}

charonvg.Surface = require("./surface")
charonvg.Canvas = require("./canvas")
charonvg.Path = require("./path")
charonvg.Matrix = require("./matrix")
charonvg.Color = require("./color")
charonvg.Paint = require("./paint")
charonvg.Bmp = require("./bmp")
charonvg.Svg = require("./svg")

charonvg.PI = math.pi
charonvg.TWO_PI = math.pi * 2
charonvg.HALF_PI = math.pi / 2
charonvg.KAPPA = 0.55228474983079339840

charonvg.VERSION = "0.1.0"

return charonvg
