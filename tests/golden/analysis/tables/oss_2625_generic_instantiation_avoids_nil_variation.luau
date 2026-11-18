type Base = { createdAt: number }
type Holder<T> = Base & { read value: T }

local function get<T>(holder: Holder<T>): T
	return holder.value
end

local h: Holder<number> = { createdAt = 1, value = 5 }
local n = get(h)
local _: _luau_print<typeof(n)>