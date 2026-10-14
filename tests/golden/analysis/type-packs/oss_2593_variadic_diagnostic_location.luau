-- https://github.com/luau-lang/luau/issues/2593

local function func(b: buffer, i: number, bl1: boolean, ...: boolean?)
    if not ... then return end
    func(
        b,
        i + 1,
        ...
    )
end

local buf = buffer.create(128)
func(buf, 0, true)

local function func2(b: buffer, i: number, bl1: boolean, bl2: boolean, ...: boolean?)
    if not ... then return end
    func2(
        b,
        i + 1,
        ...
    )
end

local buf2 = buffer.create(128)
func2(buf2, 0, true, true)
