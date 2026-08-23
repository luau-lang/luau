--!nonstrict
-- Lift a function's parsed body into a virtual-register IR built from register "webs"
-- (reaching-definitions connected components). Each web becomes a temp so the IRC
-- allocator can reallocate the whole GPR file.
--
-- Input: `body` = array of parsed statements (labels + insns) for one function.
-- Output: {
--   nodes = { <node>... },       -- one per statement, in order (labels kept as markers)
--   temps = { tempId -> {reg=num, precolor=nil} },
--   insnDefTemps/UseTemps via node.defTemps / node.useTemps
--   node fields: kind="insn"/"label", stmt=<parsed>, defTemps={}, useTemps={},
--                opTemps = { <operand occurrence> -> tempId } keyed by identity
-- }

local arm64 = require("./arm64_isa")

local lift = {}

-- Union-find.
local function makeUF()
	local parent = {}
	local function find(x)
		while parent[x] and parent[x] ~= x do
			parent[x] = parent[parent[x]] or parent[x]
			x = parent[x]
		end
		return x
	end
	local function union(a, b)
		local ra, rb = find(a), find(b)
		if ra ~= rb then parent[rb] = ra end
		return find(a)
	end
	local function ensure(x) if parent[x] == nil then parent[x] = x end end
	return { find = find, union = union, ensure = ensure, parent = parent }
end

function lift.liftFunction(body, opts)
	opts = opts or {}
	local classify = opts.classify or arm64.classify  -- per-target instruction semantics
	local returnRegs = opts.returnRegs  -- array of reg nums that `ret` reads (AAPCS return)
	-- Separate into ordered instruction list; keep label markers.
	local insns = {}      -- { {stmt, classify, index} } real instructions, 1-based
	local labelIndex = {} -- label name -> position in `insns` (the following insn) ; we use a marker approach
	local nodes = {}      -- ordered nodes matching `body` (labels + insns)
	-- We build a parallel array `flow` of just instructions for dataflow, and record
	-- for each label the index of the next instruction (its CFG entry).
	local flow = {}
	local labelToFlow = {}
	local pendingLabels = {}
	for _, st in body do
		if st.kind == "label" then
			table.insert(pendingLabels, st.name)
			table.insert(nodes, { kind = "label", name = st.name })
		elseif st.kind == "insn" then
			local c = classify(st)
			if c.control == "return" and returnRegs then
				for _, rn in returnRegs do
					table.insert(c.uses, { kind = "reg", rc = "gpr", num = rn, width = 64, name = "x" .. rn, retReg = true })
				end
			end
			if c.control == "call" and opts.callInfo then
				local ci = opts.callInfo(st)
				if ci then
					for _, rn in (ci.argRegs or {}) do
						table.insert(c.uses, { kind = "reg", rc = "gpr", num = rn, width = 64, name = "x" .. rn, argReg = true })
					end
					if ci.resultReg ~= nil then
						table.insert(c.defs, { kind = "reg", rc = "gpr", num = ci.resultReg, width = 64, name = "x" .. ci.resultReg, resultReg = true })
					end
				end
			end
			local fi = #flow + 1
			table.insert(flow, { stmt = st, c = c, fi = fi })
			for _, ln in pendingLabels do labelToFlow[ln] = fi end
			pendingLabels = {}
			table.insert(nodes, { kind = "insn", stmt = st, c = c, fi = fi })
		else
			table.insert(nodes, { kind = "other", stmt = st })
		end
	end
	local n = #flow

	-- CFG successors over flow indices. Index 0 = virtual ENTRY -> flows to 1.
	local succ = {}
	for i = 1, n do succ[i] = {} end
	for i = 1, n do
		local c = flow[i].c
		local added = {}
		if c.targets then
			for _, t in c.targets do
				local ti = labelToFlow[t]
				if ti then table.insert(succ[i], ti); added[ti] = true end
			end
		end
		if c.fallsThrough ~= false and i + 1 <= n then
			table.insert(succ[i], i + 1)
		end
	end
	-- predecessors
	local pred = {}
	for i = 0, n do pred[i] = {} end
	table.insert(pred[1] or {}, 0)  -- entry
	pred[1] = pred[1] or {}
	if n >= 1 then table.insert(pred[1], 0) end
	for i = 1, n do
		for _, s in succ[i] do table.insert(pred[s], i) end
	end

	-- Def sites. Each def of reg num at flow i is a site id. ENTRY (i=0) defs all regs.
	local sites = {}          -- id -> { fi, reg }
	local siteId = {}         -- key "fi:reg" -> id
	local function keyOf(fi, reg) return tostring(fi) .. ":" .. tostring(reg) end
	local function addSite(fi, reg)
		local k = keyOf(fi, reg)
		if siteId[k] then return siteId[k] end
		local id = #sites + 1
		sites[id] = { fi = fi, reg = reg }
		siteId[k] = id
		return id
	end
	-- Which regs are used anywhere (to bound entry pseudo-defs).
	local usedRegs = {}
	for i = 1, n do
		for _, r in flow[i].c.uses do usedRegs[r.num] = true end
		for _, r in flow[i].c.defs do usedRegs[r.num] = true end
	end
	-- ENTRY defs for every used reg.
	local entrySites = {}
	for reg in usedRegs do
		local id = addSite(0, reg)
		entrySites[reg] = id
	end
	-- Real def sites.
	for i = 1, n do
		for _, r in flow[i].c.defs do addSite(i, r.num) end
	end

	-- GEN/KILL per flow index (over site ids). GEN[i] = sites defined at i.
	local gen = {}
	local defRegs = {}
	for i = 0, n do gen[i] = {}; defRegs[i] = {} end
	for reg, id in entrySites do gen[0][id] = true; defRegs[0][reg] = true end
	for i = 1, n do
		for _, r in flow[i].c.defs do
			gen[i][addSite(i, r.num)] = true
			defRegs[i][r.num] = true
		end
	end

	-- Reaching definitions fixpoint (per flow index).
	local IN, OUT = {}, {}
	for i = 0, n do IN[i] = {}; OUT[i] = {} end
	-- OUT[0] = gen[0]
	for id in gen[0] do OUT[0][id] = true end
	local changed = true
	while changed do
		changed = false
		for i = 1, n do
			local newIn = {}
			for _, p in pred[i] do
				for id in OUT[p] do newIn[id] = true end
			end
			local newOut = {}
			for id in gen[i] do newOut[id] = true end
			for id in newIn do
				local s = sites[id]
				if not defRegs[i][s.reg] then newOut[id] = true end
			end
			-- compare
			local function differ(a, b)
				for k in a do if not b[k] then return true end end
				for k in b do if not a[k] then return true end end
				return false
			end
			if differ(newIn, IN[i]) or differ(newOut, OUT[i]) then
				changed = true; IN[i] = newIn; OUT[i] = newOut
			end
		end
	end

	-- Build webs: union def-sites reaching common uses.
	local uf = makeUF()
	for id = 1, #sites do uf.ensure(id) end
	-- reaching defs of reg at instruction i = { id in IN[i] : sites[id].reg == reg }
	local function reachingDefs(i, reg)
		local out = {}
		for id in IN[i] do if sites[id].reg == reg then table.insert(out, id) end end
		return out
	end
	-- For each use, union reaching defs.
	local useWeb = {}  -- "fi:reg" -> web root (for the use occurrence)
	for i = 1, n do
		for _, r in flow[i].c.uses do
			local ds = reachingDefs(i, r.num)
			if #ds > 0 then
				local root = ds[1]
				for k = 2, #ds do root = uf.union(root, ds[k]) end
				useWeb[keyOf(i, r.num)] = uf.find(root)
			end
		end
	end
	-- RMW: only genuine read-modify-write regs (writeback bases, movk/bfi) must share a
	-- single register for use+def. A load whose destination reuses the base register is
	-- NOT rmw (base and dest are independent values), so it is not unioned here.
	for i = 1, n do
		for _, rn in (flow[i].c.rmw or {}) do
			local uw = useWeb[keyOf(i, rn)]
			local dw = uf.find(addSite(i, rn))
			if uw then uf.union(uw, dw) end
		end
	end

	-- Assign temp ids per web root.
	local temps = {}
	local rootToTemp = {}
	local nextTemp = 0
	local function tempFor(root)
		if not rootToTemp[root] then
			nextTemp += 1
			rootToTemp[root] = nextTemp
			temps[nextTemp] = { reg = sites[root].reg }
		end
		return rootToTemp[root]
	end
	-- entry temps
	local entryTemp = {}   -- reg num -> temp id (the live-in value)
	for reg, id in entrySites do
		entryTemp[reg] = tempFor(uf.find(id))
	end

	-- Annotate nodes with def/use temps and per-operand temp mapping.
	for _, node in nodes do
		if node.kind == "insn" then
			local i = node.fi
			local c = node.c
			node.defTemps = {}
			node.useTemps = {}
			node.opTemp = {}   -- reg-operand table -> tempId
			for _, r in c.defs do
				local t = tempFor(uf.find(addSite(i, r.num)))
				table.insert(node.defTemps, t)
				node.opTemp[r] = t
			end
			for _, r in c.uses do
				local uw = useWeb[keyOf(i, r.num)]
				-- re-canonicalize: later unions may have merged this web.
				local t = uw and tempFor(uf.find(uw)) or (entryTemp[r.num])
				if t then
					table.insert(node.useTemps, t)
					node.opTemp[r] = t
				end
			end
		end
	end

	return {
		nodes = nodes,
		flow = flow,
		temps = temps,
		entryTemp = entryTemp,
		succ = succ,
		labelToFlow = labelToFlow,
		nextTemp = nextTemp,
	}
end

return lift
