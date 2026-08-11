local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

	-- Fibonacci by rotation. Both accumulators stay live across the iteration, so `prev = a` is a copy that has to
	-- survive, and the exit test splits the loop body so const prop still knows each local's tag past it but not its
	-- value. That is the case where a copy loads and stores a whole TValue and the next read of the destination has
	-- to go back to the stack for it.
	local function fib(rounds: integer): integer
		local a = 0i
		local b = 1i
		local left = rounds

		while true do
			a = integer.band(a, 0xFFFFFFFFi)
			b = integer.band(b, 0xFFFFFFFFi)

			if integer.ule(left, 0i) then
				break
			end
			left = integer.sub(left, 1i)

			local prev = a
			a = b
			b = integer.add(prev, b)
		end

		return a
	end

	local ts0 = os.clock()

	local total = 0i
	for i = 1, 2000000 do
		total = integer.add(total, fib(15i))
	end

	local ts1 = os.clock()

	assert(integer.tonumber(integer.band(total, 0xFFFFFFFFi)) ~= 0)

	return ts1 - ts0
end

bench.runCode(test, "fib_rotate_int64")
