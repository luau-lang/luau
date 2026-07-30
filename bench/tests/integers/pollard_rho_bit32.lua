local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

	-- 64-bit wrapping add
	local function add64(ah, al, bh, bl)
		local lo = al + bl
		local hi = ah + bh + lo // 0x100000000
		return bit32.bor(hi, 0), bit32.bor(lo, 0)
	end

	-- 64-bit subtract (a - b), assumes a >= b
	local function sub64(ah, al, bh, bl)
		local lo = al - bl
		local borrow = 0
		if lo < 0 then lo = lo + 0x100000000; borrow = 1 end
		return bit32.bor(ah - bh - borrow, 0), bit32.bor(lo, 0)
	end

	-- 64-bit remainder via binary long division (shift-and-subtract).
	-- 64 iterations regardless of operand magnitude.
	local function rem64(ah, al, bh, bl)
		if ah < bh or (ah == bh and al < bl) then return ah, al end

		local rh, rl = 0, 0
		for i = 63, 0, -1 do
			rh = bit32.bor(bit32.lshift(rh, 1), bit32.rshift(rl, 31))
			rl = bit32.lshift(rl, 1)
			if i >= 32 then
				rl = bit32.bor(rl, bit32.band(bit32.rshift(ah, i - 32), 1))
			else
				rl = bit32.bor(rl, bit32.band(bit32.rshift(al, i), 1))
			end
			if rh > bh or (rh == bh and rl >= bl) then
				local lo = rl - bl
				local borrow = 0
				if lo < 0 then lo = lo + 0x100000000; borrow = 1 end
				rh = bit32.bor(rh - bh - borrow, 0)
				rl = bit32.bor(lo, 0)
			end
		end
		return rh, rl
	end

	-- Multiply two 32-bit numbers, return full 64-bit result as (hi, lo).
	local function mul32_full(a, b)
		local a0 = bit32.band(a, 0xFFFF)
		local a1 = bit32.rshift(a, 16)
		local b0 = bit32.band(b, 0xFFFF)
		local b1 = bit32.rshift(b, 16)

		local c0 = a0 * b0
		local c1 = a1 * b0 + a0 * b1
		local c2 = a1 * b1

		local r0 = c0 % 0x10000
		c1 = c1 + (c0 - r0) / 0x10000
		local r1 = c1 % 0x10000
		c2 = c2 + (c1 - r1) / 0x10000
		local r2 = c2 % 0x10000
		local r3 = (c2 - r2) / 0x10000

		return r2 + r3 * 0x10000, r0 + r1 * 0x10000
	end

	-- Modular multiplication via double-and-add (binary method, mod m).
	-- All values stay in [0, m-1]; a+a < 2m so a single compare-subtract suffices.
	local function mulmod64(ah, al, bh, bl, mh, ml)
		local rh, rl = 0, 0
		if ah > mh or (ah == mh and al >= ml) then
			ah, al = sub64(ah, al, mh, ml)
		end
		while bh ~= 0 or bl ~= 0 do
			if bit32.band(bl, 1) ~= 0 then
				rh, rl = add64(rh, rl, ah, al)
				if rh > mh or (rh == mh and rl >= ml) then
					rh, rl = sub64(rh, rl, mh, ml)
				end
			end
			ah, al = add64(ah, al, ah, al)
			if ah > mh or (ah == mh and al >= ml) then
				ah, al = sub64(ah, al, mh, ml)
			end
			bl = bit32.bor(bit32.rshift(bl, 1), bit32.lshift(bh, 31))
			bh = bit32.rshift(bh, 1)
		end
		return rh, rl
	end

	local function gcd64(ah, al, bh, bl)
		while bh ~= 0 or bl ~= 0 do
			local rh, rl = rem64(ah, al, bh, bl)
			ah, al = bh, bl
			bh, bl = rh, rl
		end
		return ah, al
	end

	local function absdiff64(ah, al, bh, bl)
		if ah > bh or (ah == bh and al >= bl) then
			return sub64(ah, al, bh, bl)
		end
		return sub64(bh, bl, ah, al)
	end

	local function pollard_rho(nh, nl)
		if bit32.band(nl, 1) == 0 then return 0, 2 end

		local ch, cl = 0, 1
		while true do
			local xh, xl = 0, 2
			local yh, yl = 0, 2
			local dh, dl = 0, 1

			while dh == 0 and dl == 1 do
				-- x = (x*x + c) mod n
				xh, xl = mulmod64(xh, xl, xh, xl, nh, nl)
				xh, xl = add64(xh, xl, ch, cl)
				if xh > nh or (xh == nh and xl >= nl) then
					xh, xl = sub64(xh, xl, nh, nl)
				end

				-- y = (y*y + c) mod n, twice
				yh, yl = mulmod64(yh, yl, yh, yl, nh, nl)
				yh, yl = add64(yh, yl, ch, cl)
				if yh > nh or (yh == nh and yl >= nl) then
					yh, yl = sub64(yh, yl, nh, nl)
				end
				yh, yl = mulmod64(yh, yl, yh, yl, nh, nl)
				yh, yl = add64(yh, yl, ch, cl)
				if yh > nh or (yh == nh and yl >= nl) then
					yh, yl = sub64(yh, yl, nh, nl)
				end

				local ah, al = absdiff64(xh, xl, yh, yl)
				dh, dl = gcd64(ah, al, nh, nl)
			end

			if dh ~= nh or dl ~= nl then
				return dh, dl
			end
			cl = cl + 1
			if cl >= 0x100000000 then cl = 0; ch = ch + 1 end
		end
	end

	-- Same 8 semiprimes as int64 version, computed from ~10^5 primes.
	local primes = {
		100003, 100019, 100043, 100049, 100057, 100069, 100103, 100109,
		100129, 100151, 100153, 100169, 100183, 100189, 100193, 100207,
	}
	local semiprimes_h = {}
	local semiprimes_l = {}
	for i = 1, #primes, 2 do
		local j = (i + 1) // 2
		semiprimes_h[j], semiprimes_l[j] = mul32_full(primes[i], primes[i + 1])
	end

	local ts0 = os.clock()

	for iter = 1, 1 do
		for i = 1, #semiprimes_h do
			local nh, nl = semiprimes_h[i], semiprimes_l[i]
			local fh, fl = pollard_rho(nh, nl)
			assert(fh ~= 0 or fl ~= 1) -- f > 1
			assert(fh ~= nh or fl ~= nl) -- f < n
			local rh, rl = rem64(nh, nl, fh, fl)
			assert(rh == 0 and rl == 0) -- f divides n
		end
	end

	local ts1 = os.clock()

	return ts1 - ts0
end

bench.runCode(test, "pollard_rho_bit32")
