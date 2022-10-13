-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("safeenv reset")

local function envChangeInMetamethod()
	-- declare constant so that at O2 this test doesn't interfere with constant folding which we can't deoptimize
	local ten
	ten = 10

	local a = setmetatable({}, {
		__index = function()
			getfenv().math = { abs = function(n) return n*n end }
			return 2
		end
	})

	local b = a.x

	assert(math.abs(ten) == 100)
end

envChangeInMetamethod()

return"OK"
