local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

local noise = require("./erosion-dir/noise")
local erosion = require("./erosion-dir/erosion")
local mesh = require("./erosion-dir/mesh")

function test()

-- Erosion benchmark: terrain generation with hydraulic and thermal erosion on a grid

local GRID_SIZE = 256
local NOISE_SCALE = 4.0
local NOISE_OCTAVES = 6
local HYDRAULIC_ITERATIONS = 50000
local THERMAL_ITERATIONS = 20
local SEED = 987654

local heightmap = noise.generateHeightmap(GRID_SIZE, NOISE_SCALE, NOISE_OCTAVES)

local minH, maxH, avgH = mesh.computeStats(heightmap, GRID_SIZE)

heightmap = erosion.hydraulicErosion(heightmap, GRID_SIZE, HYDRAULIC_ITERATIONS, SEED)

heightmap = erosion.thermalErosion(heightmap, GRID_SIZE, THERMAL_ITERATIONS, 0.01)

local minH2, maxH2, avgH2 = mesh.computeStats(heightmap, GRID_SIZE)

local terrainMesh = mesh.generateMesh(heightmap, GRID_SIZE, 1.0)

print(string.format("Erosion benchmark complete: grid=%dx%d, vertices=%d",
    GRID_SIZE, GRID_SIZE, terrainMesh.vertexCount))
print(string.format("  Before erosion: min=%.17g max=%.17g avg=%.17g", minH, maxH, avgH))
print(string.format("  After erosion:  min=%.17g max=%.17g avg=%.17g", minH2, maxH2, avgH2))

if minH2 ~= 0.30927880669503277 then
    error("Bad min")
end
if maxH2 ~= 0.78981177822856874 then
    error("Bad max")
end
if avgH2 ~= 0.48523436474051113 then
    error("Bad average")
end

end

bench.runCode(test, "erosion")
