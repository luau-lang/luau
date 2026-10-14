-- https://github.com/luau-lang/luau/issues/2599

type function Identity(ty)
    return ty
end

type Box<T> = { value: T }

local function make<S>(value: S): Box<Identity<S>>
    return { value = value }
end

local function unbox<T>(box: Box<T>): T
    return box.value
end

local box = make({ x = 1 })

local _direct: _luau_print<typeof(box.value)> = box.value
local unboxed = unbox(box)
local _unboxed: _luau_print<typeof(unboxed)> = unboxed
