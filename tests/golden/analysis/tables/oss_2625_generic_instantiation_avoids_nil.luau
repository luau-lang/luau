type Base = { createdAt: number }
type Holder<T> = Base & { value: T }

local function get<T>(holder: Holder<T>): T
    return holder.value
end

local h: Holder<number> = { createdAt = 1, value = 5 }
local _n: number = get(h)