-- Perlin/Simplex noise and fractal octaves for terrain generation

local permutation = {
    151,160,137,91,90,15,131,13,201,95,96,53,194,233,7,225,
    140,36,103,30,69,142,8,99,37,240,21,10,23,190,6,148,
    247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,
    57,177,33,88,237,149,56,87,174,20,125,136,171,168,68,175,
    74,165,71,134,139,48,27,166,77,146,158,231,83,111,229,122,
    60,211,133,230,220,105,92,41,55,46,245,40,244,102,143,54,
    65,25,63,161,1,216,80,73,209,76,132,187,208,89,18,169,
    200,196,135,130,116,188,159,86,164,100,109,198,173,186,3,64,
    52,217,226,250,124,123,5,202,38,147,118,126,255,82,85,212,
    207,206,59,227,47,16,58,17,182,189,28,42,223,183,170,213,
    119,248,152,2,44,154,163,70,221,153,101,155,167,43,172,9,
    129,22,39,253,19,98,108,110,79,113,224,232,178,185,112,104,
    218,246,97,228,251,34,242,193,238,210,144,12,191,179,162,241,
    81,51,145,235,249,14,239,107,49,192,214,31,181,199,106,157,
    184,84,204,176,115,121,50,45,127,4,150,254,138,236,205,93,
    222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180,
}

local p = table.create(512, 0)
for i = 0, 255 do
    p[i + 1] = permutation[i + 1]
    p[i + 257] = permutation[i + 1]
end

local function fade(t: number): number
    return t * t * t * (t * (t * 6 - 15) + 10)
end

local function lerp(t: number, a: number, b: number): number
    return a + t * (b - a)
end

local function grad2(hash: number, x: number, y: number): number
    local h = bit32.band(hash, 7)
    local u = if h < 4 then x else y
    local v = if h < 4 then y else x
    return (if bit32.band(h, 1) == 0 then u else -u) + (if bit32.band(h, 2) == 0 then v else -v)
end

local function perlin2D(x: number, y: number): number
    local xi = bit32.band(math.floor(x), 255)
    local yi = bit32.band(math.floor(y), 255)
    local xf = x - math.floor(x)
    local yf = y - math.floor(y)

    local u = fade(xf)
    local v = fade(yf)

    local aa = p[p[xi + 1] + yi + 1]
    local ab = p[p[xi + 1] + yi + 2]
    local ba = p[p[xi + 2] + yi + 1]
    local bb = p[p[xi + 2] + yi + 2]

    local x1 = lerp(u, grad2(aa, xf, yf), grad2(ba, xf - 1, yf))
    local x2 = lerp(u, grad2(ab, xf, yf - 1), grad2(bb, xf - 1, yf - 1))

    return lerp(v, x1, x2)
end

local function fractalNoise(x: number, y: number, octaves: number, lacunarity: number, persistence: number): number
    local value = 0
    local amplitude = 1
    local frequency = 1
    local maxAmplitude = 0

    for i = 1, octaves do
        value += perlin2D(x * frequency, y * frequency) * amplitude
        maxAmplitude += amplitude
        amplitude *= persistence
        frequency *= lacunarity
    end

    return value / maxAmplitude
end

local function generateHeightmap(size: number, scale: number, octaves: number): {number}
    local heightmap = table.create(size * size, 0)
    for y = 0, size - 1 do
        for x = 0, size - 1 do
            local nx = x / size * scale
            local ny = y / size * scale
            local h = fractalNoise(nx, ny, octaves, 2.0, 0.5)
            h = (h + 1) * 0.5
            heightmap[y * size + x + 1] = h
        end
    end
    return heightmap
end

return {
    perlin2D = perlin2D,
    fractalNoise = fractalNoise,
    generateHeightmap = generateHeightmap,
}
