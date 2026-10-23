local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

local ppm = require("./ppm-dir/ppm")

function test()

-- PPM benchmark: compress and decompress a procedurally generated corpus

local function generateCorpus(size: number, seed: number): {number}
    local data: {number} = table.create(size, 0)
    local phrases = {
        "the quick brown fox jumps over the lazy dog ",
        "to be or not to be that is the question ",
        "all that glitters is not gold ",
        "a journey of a thousand miles begins with a single step ",
        "in the beginning was the word and the word was with god ",
        "it was the best of times it was the worst of times ",
        "call me ishmael some years ago never mind how long precisely ",
        "it is a truth universally acknowledged that a single man ",
        "happy families are all alike every unhappy family is unhappy ",
        "the world is full of obvious things which nobody ever observes ",
    }
    local pos = 1
    while pos <= size do
        seed = bit32.band(seed * 1103515245 + 12345, 0x7FFFFFFF)
        local phraseIdx = (seed % #phrases) + 1
        local phrase = phrases[phraseIdx]
        for i = 1, #phrase do
            if pos > size then break end
            data[pos] = string.byte(phrase, i)
            pos += 1
        end
        seed = bit32.band(seed * 1103515245 + 12345, 0x7FFFFFFF)
        if seed % 10 == 0 then
            seed = bit32.band(seed * 1103515245 + 12345, 0x7FFFFFFF)
            local upper = (seed % 26) + 65
            if pos <= size then
                data[pos] = upper
                pos += 1
            end
        end
    end
    return data
end

local CORPUS_SIZE = 4096
local ITERATIONS = 3
local SEED = 314159

local corpus = generateCorpus(CORPUS_SIZE, SEED)
local totalCompressed = 0
local totalDecompressed = 0
local verified = true

for iter = 1, ITERATIONS do
    local compressed = ppm.compress(corpus)
    totalCompressed += #compressed

    local decompressed = ppm.decompress(compressed, #corpus)
    totalDecompressed += #decompressed

    if #decompressed ~= #corpus then
        verified = false
    else
        for i = 1, #corpus do
            if decompressed[i] ~= corpus[i] then
                verified = false
                break
            end
        end
    end
end

local ratio = totalCompressed / (CORPUS_SIZE * ITERATIONS) * 100
print(string.format("PPM benchmark complete: %d iterations, size=%d, ratio=%.1f%%, verified=%s",
    ITERATIONS, CORPUS_SIZE, ratio, tostring(verified)))
if not verified then
    error("NOT VERIFIED")
end

end

bench.runCode(test, "ppm")
