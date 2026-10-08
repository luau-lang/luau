local function prequire(name)
    local success, result = pcall(require, name)
    return success and result
end
local bench = script and require(script.Parent.bench_support)
    or prequire("bench_support")
    or require("../bench_support")

bench.runCode(function()

    local Number = {}
    Number.__index = Number

    function Number.new(v)
        local self = {
            value = v,
        }
        setmetatable(self, Number)
        return self
    end

    function Number:Swap(other)
        local tmp = other.value
        other.value = self.value
        self.value = tmp
    end

    local numbers = {}

    for i = 1, 100 do
        numbers[i] = Number.new(math.random())
    end

    for i = 1, 100_000 do
        for j = 1, 100 do
            numbers[j]:Swap(numbers[math.random(100)])
        end
    end

end, "OOP: field random access")
