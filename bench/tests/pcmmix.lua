local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

local samples = 100_000

-- create two 16-bit stereo pcm audio buffers
local ch1 = buffer.create(samples * 2 * 2)
local ch2 = buffer.create(samples * 2 * 2)

-- just init with random data
for i = 0, samples * 2 - 1 do
  buffer.writei16(ch1, i * 2, math.random(-32768, 32767))
  buffer.writei16(ch2, i * 2, math.random(-32768, 32767))
end

function test()
  local mix = buffer.create(samples * 2 * 2)
  
  for i = 0, samples - 1 do
    local s1l = buffer.readi16(ch1, i * 4)
    local s1r = buffer.readi16(ch1, i * 4 + 2)

    local s2l = buffer.readi16(ch2, i * 4)
    local s2r = buffer.readi16(ch2, i * 4 + 2)
    
    local combinedl = s1l + s2l - s1l * s2l / 32768
    local combinedr = s1r + s2r - s1r * s2r / 32768
    
    buffer.writei16(mix, i * 4, combinedl)
    buffer.writei16(mix, i * 4 + 2, combinedr)
  end
end

bench.runCode(test, "pcmmix")
