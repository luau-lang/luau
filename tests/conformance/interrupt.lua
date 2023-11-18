-- This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
print("testing interrupts")

-- this function will be called by C code with a special interrupt handler that validates hit locations
function test()
	function foo()
		for i=1,10 do end
		return
	end

	foo()

	function bar()
		local i = 0
		while i < 10 do
			i += i + 1
		end
	end

	bar()

	function baz()
	end

	baz()
end

-- these functions will be called by C code with a special interrupt handler that terminates after a few invocations
function infloop1()
	while true do end
end

function infloop2()
	while true do continue end
end

function infloop3()
	repeat until false
end

function infloop4()
	repeat continue until false
end

function infloop5()
	for i=0,0,0 do end
end

function infloop6()
	for i=0,0,0 do continue end
end

function infloop7()
	for i=1,math.huge do end
end

function infloop8()
	for i=1,math.huge do continue end
end

function infloop9()
	-- technically not a loop, but an exponentially recursive function
	local function boom()
		boom()
		boom()
	end
	boom()
end

return "OK"
