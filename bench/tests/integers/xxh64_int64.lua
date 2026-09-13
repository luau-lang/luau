local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

	local PRIME64_1 = 0x9E3779B185EBCA87i
	local PRIME64_2 = 0xC2B2AE3D27D4EB4Fi
	local PRIME64_3 = 0x165667B19E3779F9i
	local PRIME64_4 = 0x85EBCA77C2B2AE63i
	local PRIME64_5 = 0x27D4EB2F165667C5i

	local function xxh64(buf, seed)
		local len = buffer.len(buf)
		local offset = 0
		local hash

		if len >= 32 then
			local a1 = integer.add(integer.add(seed, PRIME64_1), PRIME64_2)
			local a2 = integer.add(seed, PRIME64_2)
			local a3 = seed
			local a4 = integer.sub(seed, PRIME64_1)

			while offset <= len - 32 do
				local lane1 = buffer.readinteger(buf, offset)
				local lane2 = buffer.readinteger(buf, offset + 8)
				local lane3 = buffer.readinteger(buf, offset + 16)
				local lane4 = buffer.readinteger(buf, offset + 24)

				a1 = integer.mul(integer.lrotate(integer.add(a1, integer.mul(lane1, PRIME64_2)), 31i), PRIME64_1)
				a2 = integer.mul(integer.lrotate(integer.add(a2, integer.mul(lane2, PRIME64_2)), 31i), PRIME64_1)
				a3 = integer.mul(integer.lrotate(integer.add(a3, integer.mul(lane3, PRIME64_2)), 31i), PRIME64_1)
				a4 = integer.mul(integer.lrotate(integer.add(a4, integer.mul(lane4, PRIME64_2)), 31i), PRIME64_1)

				offset += 32
			end

			hash = integer.add(integer.add(integer.lrotate(a1, 1i), integer.lrotate(a2, 7i)), integer.add(integer.lrotate(a3, 12i), integer.lrotate(a4, 18i)))

			local r1 = integer.mul(integer.lrotate(integer.mul(a1, PRIME64_2), 31i), PRIME64_1)
			hash = integer.add(integer.mul(integer.bxor(hash, r1), PRIME64_1), PRIME64_4)
			local r2 = integer.mul(integer.lrotate(integer.mul(a2, PRIME64_2), 31i), PRIME64_1)
			hash = integer.add(integer.mul(integer.bxor(hash, r2), PRIME64_1), PRIME64_4)
			local r3 = integer.mul(integer.lrotate(integer.mul(a3, PRIME64_2), 31i), PRIME64_1)
			hash = integer.add(integer.mul(integer.bxor(hash, r3), PRIME64_1), PRIME64_4)
			local r4 = integer.mul(integer.lrotate(integer.mul(a4, PRIME64_2), 31i), PRIME64_1)
			hash = integer.add(integer.mul(integer.bxor(hash, r4), PRIME64_1), PRIME64_4)
		else
			hash = integer.add(seed, PRIME64_5)
		end

		hash = integer.add(hash, integer.create(len))

		while offset <= len - 8 do
			local lane = buffer.readinteger(buf, offset)
			local r = integer.mul(integer.lrotate(integer.mul(lane, PRIME64_2), 31i), PRIME64_1)
			hash = integer.add(integer.mul(integer.lrotate(integer.bxor(hash, r), 27i), PRIME64_1), PRIME64_4)
			offset += 8
		end

		if offset <= len - 4 then
			local lane = integer.create(buffer.readu32(buf, offset))
			hash = integer.add(integer.mul(integer.lrotate(integer.bxor(hash, integer.mul(lane, PRIME64_1)), 23i), PRIME64_2), PRIME64_3)
			offset += 4
		end

		while offset < len do
			local b = integer.create(buffer.readu8(buf, offset))
			hash = integer.mul(integer.lrotate(integer.bxor(hash, integer.mul(b, PRIME64_5)), 11i), PRIME64_1)
			offset += 1
		end

		-- avalanche
		hash = integer.bxor(hash, integer.rshift(hash, 33i))
		hash = integer.mul(hash, PRIME64_2)
		hash = integer.bxor(hash, integer.rshift(hash, 29i))
		hash = integer.mul(hash, PRIME64_3)
		hash = integer.bxor(hash, integer.rshift(hash, 32i))

		return string.format("%016x", hash)
	end

	local input = buffer.fromstring(string.rep(".", 1e3))

	local ts0 = os.clock()

	for i = 1, 2500 do
		local res = xxh64(input, 0i)
		assert(res == "1d27c09e95a70d9e")
	end

	local ts1 = os.clock()

	return ts1 - ts0
end

bench.runCode(test, "xxh64_int64")
