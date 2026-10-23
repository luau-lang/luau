--!nonstrict
-- Pointer-flow analysis over the lifted IR (register webs = temps).
-- Determines which temps hold pointers and, for each, the temp that carries its
-- `lower` (capability). Lowers propagate unchanged through copies and address
-- arithmetic (GEP), matching GIMSO semantics.
--
-- Seeds (provided by the caller):
--   argPtrTemps  : array of temp ids that are pointer arguments
--   loadPtrDefs  : set of node -> true for `;! load ptr` nodes (their def temp is a ptr)
--   callPtrDefs  : set of node -> true for call nodes whose result is a pointer
--
-- Returns { isPtr = {temp->bool}, lowerTemp = {temp->loTemp}, newTemps = {...} }
-- New lower temps are allocated via allocTemp().

local ptrflow = {}

function ptrflow.analyze(lf, seeds, allocTemp, opts)
	opts = opts or {}
	local baseMnemonic = opts.baseMnemonic or function(m) return string.lower(m) end
	local isPtr = {}
	local lowerTemp = {}     -- ptr temp -> its lower temp
	local nodes = lf.nodes

	local function ensureLower(t)
		if not lowerTemp[t] then
			lowerTemp[t] = allocTemp({ isLower = true, forPtr = t })
		end
		return lowerTemp[t]
	end

	-- Seed argument pointers.
	for _, t in (seeds.argPtrTemps or {}) do
		isPtr[t] = true
		ensureLower(t)
	end
	-- Seed load-ptr / call-ptr / alloca-result definitions (their def temp is a pointer
	-- with a fresh lower).
	for _, node in nodes do
		if node.kind == "insn" then
			if (seeds.loadPtrDefs and seeds.loadPtrDefs[node])
				or (seeds.callPtrDefs and seeds.callPtrDefs[node])
				or (seeds.allocaResultDefs and seeds.allocaResultDefs[node]) then
				local d = node.defTemps and node.defTemps[1]
				if d then isPtr[d] = true; ensureLower(d) end
			end
		end
	end

	-- Fixpoint propagation through copies and address arithmetic.
	local changed = true
	while changed do
		changed = false
		for _, node in nodes do
			local seeded = (seeds.loadPtrDefs and seeds.loadPtrDefs[node])
				or (seeds.allocaResultDefs and seeds.allocaResultDefs[node])
			if node.kind == "insn" and not seeded then
				local m = baseMnemonic(node.stmt and node.stmt.mnemonic or node.mnemonic or "")
				local d = node.defTemps and node.defTemps[1]
				local us = node.useTemps or {}
				local function markFrom(srcPtr)
					if not isPtr[d] then isPtr[d] = true; changed = true end
					local lo = lowerTemp[srcPtr]
					if lo and lowerTemp[d] ~= lo then lowerTemp[d] = lo; changed = true end
				end
				-- A memory-source instruction is a LOAD, not a register copy/arithmetic:
				-- the loaded value's pointer-ness comes from a `;! load ptr` seed, never the
				-- address register. On x86 a load is `mov reg, [mem]`, so without this guard
				-- ptrflow would treat it as `reg = base` and flip-flop the lower forever.
				local hasMemSrc = false
				local sops = node.stmt and node.stmt.operands
				if sops then for _, op in sops do if op.kind == "mem" then hasMemSrc = true end end end
				if d then
					if m == "mov" and #us == 1 and not hasMemSrc then
						if isPtr[us[1]] then markFrom(us[1]) end
					elseif m == "add" or m == "sub" or m == "orr" or m == "lea"
						or m == "lsl" or m == "lsr" or m == "asr" or m == "and"
						or m == "or" or m == "shl" or m == "shr" or m == "sar" then
						-- address arithmetic / alignment: a pointer combined with integers
						-- keeps the pointer's lower (bounds checks still guard any misuse).
						local ptrSrc, intCount = nil, 0
						for _, u in us do
							if isPtr[u] then ptrSrc = u else intCount += 1 end
						end
						if ptrSrc and not (m == "sub" and intCount == 0) then
							markFrom(ptrSrc)
						end
					end
				end
			end
		end
	end

	return { isPtr = isPtr, lowerTemp = lowerTemp }
end

return ptrflow
