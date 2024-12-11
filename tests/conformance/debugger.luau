-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print "testing debugger" -- note, this file can't run in isolation from C tests

local a = 5

function foo(b, ...)
	print("in foo", b)
	a = 6
end

breakpoint(8)

foo(50, 42)

breakpoint(16) -- next line
print("here")

function coro(arg)
	print("before coro break")
	a = arg
	print("after coro break")
	return 42
end

breakpoint(20) -- break inside coro()

a = 7

local co = coroutine.create(coro)
local _, res = coroutine.resume(co, 8) -- this breaks and resumes!
assert(res == 42)

local cof = coroutine.wrap(coro)
assert(cof(9) == 42) -- this breaks and resumes!

function corobad()
	print("before coro break")
	error("oops")
end

assert(a == 9)

breakpoint(38) -- break inside corobad()

local co = coroutine.create(corobad)
assert(coroutine.resume(co) == false) -- this breaks, resumes and dies!

function bar()
	print("in bar")
end

breakpoint(49)
breakpoint(49, false) -- validate that disabling breakpoints works

bar()

local function breakpointSetFromMetamethod()
	local a = setmetatable({}, {
		__index = function()
			breakpoint(67)
			return 2
		end
	})

	local b = a.x

	assert(b == 2)
end

breakpointSetFromMetamethod()

-- break inside function with non-monotonic line info
local function cond(a)
	if a then
		print('a')
	else
		print('not a')
	end
end

breakpoint(77)

pcall(cond, nil) -- prevent inlining

local function continueLocals()
	repeat
		local x = tostring(game)
		do continue end
		local a1, a2, a3, a4, a5, a6
	until pcall(
		function()
			print("1")
			print("2")
		end, nil
	) or true
end

breakpoint(93)

pcall(continueLocals, nil) -- prevent inlining

return 'OK'
