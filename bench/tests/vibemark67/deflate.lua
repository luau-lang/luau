local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

local deflate = require("./deflate-dir/deflate")

function test()

-- DEFLATE benchmark: compress and decompress procedurally generated text

local function generateCorpus(size: number): {number}
    local data: {number} = table.create(size, 0)
    local seed = 73921
    local words = {
        "the ", "quick ", "brown ", "fox ", "jumps ", "over ", "lazy ", "dog ",
        "and ", "then ", "runs ", "away ", "from ", "big ", "cat ", "who ",
        "was ", "sleeping ", "under ", "old ", "oak ", "tree ", "near ",
        "river ", "bank ", "where ", "fish ", "swim ", "all ", "day ",
        "long ", "until ", "night ", "falls ", "upon ", "land ",
        "bringing ", "peace ", "quiet ", "darkness ", "throughout ",
        "entire ", "valley ", "below ", "mountain ", "peaks ",
        "covered ", "with ", "fresh ", "white ", "snow ",
    }
    local pos = 1
    while pos <= size do
        seed = bit32.band(seed * 1103515245 + 12345, 0x7FFFFFFF)
        local wordIdx = (seed % #words) + 1
        local word = words[wordIdx]
        for i = 1, #word do
            if pos > size then break end
            data[pos] = string.byte(word, i)
            pos += 1
        end
        seed = bit32.band(seed * 1103515245 + 12345, 0x7FFFFFFF)
        if seed % 20 == 0 and pos <= size then
            data[pos] = 10 -- newline
            pos += 1
        end
    end
    return data
end

local CORPUS_SIZE = 65536
local ITERATIONS = 5

local corpus = generateCorpus(CORPUS_SIZE)
local totalCompressed = 0
local totalDecompressed = 0
local verified = true

for iter = 1, ITERATIONS do
    local compressed = deflate.compress(corpus)
    totalCompressed += #compressed

    local decompressed = deflate.decompress(compressed, #corpus)
    totalDecompressed += #decompressed

    if #decompressed ~= #corpus then
        verified = false
    else
        for i = 1, math.min(1000, #corpus) do
            if decompressed[i] ~= corpus[i] then
                verified = false
                break
            end
        end
    end
end

local ratio = totalCompressed / (CORPUS_SIZE * ITERATIONS) * 100
print(string.format("Deflate benchmark complete: %d iterations, ratio=%.1f%%, verified=%s",
    ITERATIONS, ratio, tostring(verified)))

if not verified then
    error("NOT VERIFIED")
end

end

bench.runCode(test, "deflate")
