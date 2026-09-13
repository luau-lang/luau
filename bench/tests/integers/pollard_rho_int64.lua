local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

	-- Modular multiplication via double-and-add (binary method).
	-- Required because n > 2^31, so x*x can overflow signed int64.
	local function mulmod(a: integer, b: integer, m: integer): integer
		local result = 0i
		a = integer.rem(a, m)
		while b ~= 0i do
			if integer.band(b, 1i) ~= 0i then
				result = integer.rem(integer.add(result, a), m)
			end
			a = integer.rem(integer.add(a, a), m)
			b = integer.rshift(b, 1i)
		end
		return result
	end

	local function absi(x: integer): integer
		if integer.lt(x, 0i) then
			return integer.neg(x)
		end
		return x
	end

	local function gcd(a: integer, b: integer): integer
		while b ~= 0i do
			a, b = b, integer.rem(a, b)
		end
		return a
	end

	local function pollard_rho(n: integer): integer
		if integer.rem(n, 2i) == 0i then return 2i end

		local c = 1i
		while true do
			local x = 2i
			local y = 2i
			local d = 1i

			while d == 1i do
				x = integer.rem(integer.add(mulmod(x, x, n), c), n)
				y = integer.rem(integer.add(mulmod(y, y, n), c), n)
				y = integer.rem(integer.add(mulmod(y, y, n), c), n)
				d = gcd(absi(integer.sub(x, y)), n)
			end

			if d ~= n then
				return d
			end
			c = integer.add(c, 1i)
		end
	end

	-- 8 odd semiprimes with n > 2^32, products of two ~10^5 primes.
	-- Values are 34-bit, forcing genuine 64-bit arithmetic in mulmod and gcd.
	local primes = {
		100003i, 100019i, 100043i, 100049i, 100057i, 100069i, 100103i, 100109i,
		100129i, 100151i, 100153i, 100169i, 100183i, 100189i, 100193i, 100207i,
	}
	local semiprimes = {}
	for i = 1, #primes, 2 do
		semiprimes[(i + 1) // 2] = integer.mul(primes[i], primes[i + 1])
	end

	local ts0 = os.clock()

	for iter = 1, 1 do
		for i = 1, #semiprimes do
			local n = semiprimes[i]
			local f = pollard_rho(n)
			assert(f ~= 1i and f ~= n)
			assert(integer.rem(n, f) == 0i)
		end
	end

	local ts1 = os.clock()

	return ts1 - ts0
end

bench.runCode(test, "pollard_rho_int64")
