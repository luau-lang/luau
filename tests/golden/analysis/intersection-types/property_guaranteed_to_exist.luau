type Named = { name: string } & { value: number }

local function describe(item: Named): string
    return item.name .. tostring(item.value)
end

assert(describe({ name = "item", value = 1 }) == "item1")
