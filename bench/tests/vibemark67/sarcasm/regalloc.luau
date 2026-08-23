--!nonstrict
-- Iterated Register Coalescing (Appel & George) over a generic instruction list.
--
-- The allocator consumes a "machine function" describing instructions in terms of
-- abstract temporaries (temps). It computes liveness over a CFG built from labels and
-- branch instructions, builds the interference graph, and colors temps into the
-- physical GPR set, coalescing moves and spilling when necessary.
--
-- Instruction contract (each entry of func.insns):
--   { defs = {tempId...}, uses = {tempId...},
--     move = { dst = tempId, src = tempId } | nil,   -- reg-reg copy (coalesce candidate)
--     isLabelDef = "name" | nil,                      -- this insn is a label definition
--     targets = { "name"... } | nil,                  -- explicit branch targets
--     fallsThrough = bool }                            -- control can fall to next insn
--
-- func.temps: tempId -> { precolor = physRegName | nil, spillSlot = n | nil, spillCost = num }
-- The caller assigns temp ids (integers). Precolored temps model fixed physical regs.
--
-- Returns: { color = { tempId -> physRegName }, spilled = { tempId... } }
-- The driver re-runs after inserting spill code (handled by the caller) if spilled is
-- non-empty; here we implement the classic actual-spill rewrite inline via a callback.

local regalloc = {}

-- This allocator is architecture-agnostic: the allocatable GPR set (the K colors) is
-- supplied by the caller via `opts.allocatable` (arm64_isa.ALLOCATABLE / x86_64_isa.ALLOCATABLE).
-- Register-set knowledge (allocatable / caller- / callee-saved, reg numbers) lives in the
-- per-arch isa modules, not here.

-- Helpers ------------------------------------------------------------------

local function newSet(): { [any]: boolean }
	return {}
end
local function setAdd(s, x) s[x] = true end
local function setHas(s, x) return s[x] == true end

-- Build basic blocks from the instruction list.
-- Returns blocks = { { first=i, last=j, succ={blockIdx...}, insns={i..j} } }, and
-- labelToBlock mapping.
local function buildCFG(insns)
	local n = #insns
	-- leaders: first insn, any label def, any insn after a branch.
	local labelAt = {}          -- label name -> insn index
	for i, ins in insns do
		if ins.isLabelDef then
			labelAt[ins.isLabelDef] = i
		end
	end
	-- Determine block starts.
	local isLeader = {}
	isLeader[1] = true
	for i, ins in insns do
		if ins.isLabelDef then isLeader[i] = true end
		if ins.targets or (ins.fallsThrough == false) then
			if i + 1 <= n then isLeader[i + 1] = true end
		end
	end
	-- Create blocks.
	local blocks = {}
	local blockOfInsn = {}
	local cur
	for i = 1, n do
		if isLeader[i] or i == 1 then
			cur = { first = i, last = i, insns = {} }
			table.insert(blocks, cur)
		end
		cur.last = i
		table.insert(cur.insns, i)
		blockOfInsn[i] = #blocks
	end
	-- Successors.
	local labelToBlock = {}
	for bi, b in blocks do
		local firstIns = insns[b.first]
		-- a block may start with multiple stacked labels; map all leading labels
	end
	for name, idx in labelAt do
		labelToBlock[name] = blockOfInsn[idx]
	end
	for bi, b in blocks do
		b.succ = {}
		local lastIns = insns[b.last]
		local addFall = true
		if lastIns.targets then
			for _, t in lastIns.targets do
				local tb = labelToBlock[t]
				if tb then table.insert(b.succ, tb) end
			end
			addFall = lastIns.fallsThrough ~= false
		end
		if lastIns.fallsThrough == false then addFall = false end
		if addFall and bi + 1 <= #blocks then
			table.insert(b.succ, bi + 1)
		end
	end
	return blocks, labelToBlock, blockOfInsn
end
regalloc.buildCFG = buildCFG

-- Liveness: compute live-out set (of temps) for each instruction.
local function computeLiveness(insns, blocks)
	-- block-level def/use
	for _, b in blocks do
		local use = newSet()
		local def = newSet()
		for _, i in b.insns do
			local ins = insns[i]
			for _, u in (ins.uses or {}) do
				if not def[u] then use[u] = true end
			end
			for _, d in (ins.defs or {}) do
				def[d] = true
			end
		end
		b.useSet = use
		b.defSet = def
		b.liveIn = newSet()
		b.liveOut = newSet()
	end
	-- iterate to fixpoint (backward)
	local changed = true
	while changed do
		changed = false
		for bi = #blocks, 1, -1 do
			local b = blocks[bi]
			local newOut = newSet()
			for _, s in b.succ do
				for t in blocks[s].liveIn do newOut[t] = true end
			end
			-- liveIn = use ∪ (out − def)
			local newIn = newSet()
			for t in b.useSet do newIn[t] = true end
			for t in newOut do
				if not b.defSet[t] then newIn[t] = true end
			end
			-- detect change
			local function differ(a, c)
				for t in a do if not c[t] then return true end end
				for t in c do if not a[t] then return true end end
				return false
			end
			if differ(newIn, b.liveIn) or differ(newOut, b.liveOut) then
				changed = true
				b.liveIn = newIn
				b.liveOut = newOut
			end
		end
	end
	-- per-instruction live-out (backward within block)
	local liveOutOf = {}
	for _, b in blocks do
		local live = newSet()
		for t in b.liveOut do live[t] = true end
		for idx = #b.insns, 1, -1 do
			local i = b.insns[idx]
			local ins = insns[i]
			-- live-out of instruction i = current live
			local lo = newSet()
			for t in live do lo[t] = true end
			liveOutOf[i] = lo
			-- transfer: live = use ∪ (live − def)
			for _, d in (ins.defs or {}) do live[d] = nil end
			for _, u in (ins.uses or {}) do live[u] = true end
		end
	end
	return liveOutOf
end
regalloc.computeLiveness = computeLiveness

-- Per-instruction live-out sets for a generic insn list (defs/uses may be any variable ids,
-- e.g. spill-slot ids). Builds the CFG and runs the backward liveness fixpoint. Returns
-- liveOutOf[i] = set of variables live after instruction i.
function regalloc.liveOut(insns)
	return computeLiveness(insns, buildCFG(insns))
end

-- The core IRC coloring. Returns color map and spill list.
-- `temps` maps tempId -> descriptor. physical precolored temps have .precolor.
function regalloc.color(insns, temps, opts)
	opts = opts or {}
	-- Per-target register set (caller-supplied); K is the color count.
	local ALLOCATABLE = opts.allocatable or error("regalloc.color: opts.allocatable is required")
	local K = #ALLOCATABLE
	local blocks = buildCFG(insns)
	local liveOutOf = computeLiveness(insns, blocks)

	-- Interference graph.
	local adj = {}            -- tempId -> set of tempId
	local adjList = {}        -- tempId -> array
	local degree = {}
	local moveList = {}       -- tempId -> set of move-insn indices
	local worklistMoves = {}  -- set of move-insn indices
	local alias = {}          -- coalesced alias
	local nodes = {}          -- all temp ids seen

	local function ensureNode(t)
		if not nodes[t] then
			nodes[t] = true
			adj[t] = newSet()
			adjList[t] = {}
			degree[t] = 0
			moveList[t] = newSet()
		end
	end

	for t in temps do ensureNode(t) end
	for _, ins in insns do
		for _, d in (ins.defs or {}) do ensureNode(d) end
		for _, u in (ins.uses or {}) do ensureNode(u) end
	end

	local function precolored(t) return temps[t] and temps[t].precolor ~= nil end

	local function addEdge(u, v)
		if u == v or adj[u][v] then return end
		adj[u][v] = true
		adj[v][u] = true
		if not precolored(u) then
			table.insert(adjList[u], v)
			degree[u] = degree[u] + 1
		end
		if not precolored(v) then
			table.insert(adjList[v], u)
			degree[v] = degree[v] + 1
		end
	end

	-- Build moves + interference from liveness.
	for i, ins in insns do
		local live = newSet()
		for t in (liveOutOf[i] or {}) do live[t] = true end
		if ins.move then
			-- uses of a move don't interfere with its def
			for _, u in (ins.uses or {}) do live[u] = nil end
			setAdd(moveList[ins.move.dst], i)
			setAdd(moveList[ins.move.src], i)
			worklistMoves[i] = true
		end
		for _, d in (ins.defs or {}) do live[d] = true end
		for _, d in (ins.defs or {}) do
			for l in live do addEdge(l, d) end
		end
	end

	-- Coloring worklists.
	local precoloredSet = newSet()
	local initial = {}
	for t in nodes do
		if precolored(t) then precoloredSet[t] = true
		else table.insert(initial, t) end
	end

	local selectStack = {}
	local onStack = newSet()
	local coalescedNodes = newSet()
	local coloredNodes = newSet()
	local spilledNodes = {}
	local frozenMoves = newSet()
	local activeMoves = newSet()
	local constrainedMoves = newSet()
	local coalescedMoves = newSet()

	local simplifyWorklist = {}
	local freezeWorklist = {}
	local spillWorklist = {}

	local function inArray(arr, x)
		for idx, v in arr do if v == x then return idx end end
		return nil
	end
	local function removeFrom(arr, x)
		local idx = inArray(arr, x)
		if idx then table.remove(arr, idx) end
	end

	local function nodeMoves(t)
		local out = {}
		for m in moveList[t] do
			if activeMoves[m] or worklistMoves[m] then out[m] = true end
		end
		return out
	end
	local function moveRelated(t)
		return next(nodeMoves(t)) ~= nil
	end

	-- adjacent = adjacent nodes not on select stack / coalesced
	local function adjacent(t)
		local out = {}
		for _, v in adjList[t] do
			if not onStack[v] and not coalescedNodes[v] then out[v] = true end
		end
		return out
	end

	-- build initial worklists
	for _, t in initial do
		if degree[t] >= K then
			table.insert(spillWorklist, t)
		elseif moveRelated(t) then
			table.insert(freezeWorklist, t)
		else
			table.insert(simplifyWorklist, t)
		end
	end

	local function enableMoves(nodesSet)
		for t in nodesSet do
			for m in nodeMoves(t) do
				if activeMoves[m] then
					activeMoves[m] = nil
					worklistMoves[m] = true
				end
			end
		end
	end

	local function decrementDegree(t)
		local d = degree[t]
		degree[t] = d - 1
		if d == K and not precolored(t) then
			local s = adjacent(t)
			s[t] = true
			enableMoves(s)
			removeFrom(spillWorklist, t)
			if moveRelated(t) then
				table.insert(freezeWorklist, t)
			else
				table.insert(simplifyWorklist, t)
			end
		end
	end

	local function simplify()
		local t = table.remove(simplifyWorklist)
		table.insert(selectStack, t)
		onStack[t] = true
		for v in adjacent(t) do
			decrementDegree(v)
		end
	end

	local function getAlias(t)
		while coalescedNodes[t] do t = alias[t] end
		return t
	end

	local function addWorklist(t)
		if not precolored(t) and not moveRelated(t) and degree[t] < K then
			removeFrom(freezeWorklist, t)
			table.insert(simplifyWorklist, t)
		end
	end

	local function ok(t, r)
		-- George criterion helper
		return degree[t] < K or precolored(t) or adj[t][r]
	end

	local function conservative(u, v)
		-- Briggs: count high-degree neighbors of the combined node
		local seen = newSet()
		local cnt = 0
		for w in adjacent(u) do
			if not seen[w] then seen[w] = true; if degree[w] >= K then cnt += 1 end end
		end
		for w in adjacent(v) do
			if not seen[w] then seen[w] = true; if degree[w] >= K then cnt += 1 end end
		end
		return cnt < K
	end

	local function combine(u, v)
		removeFrom(freezeWorklist, v)
		removeFrom(spillWorklist, v)
		coalescedNodes[v] = true
		alias[v] = u
		for m in moveList[v] do moveList[u][m] = true end
		for w in adjacent(v) do
			addEdge(w, u)
			decrementDegree(w)
		end
		if degree[u] >= K and inArray(freezeWorklist, u) then
			removeFrom(freezeWorklist, u)
			table.insert(spillWorklist, u)
		end
	end

	local function coalesce()
		local m
		for mm in worklistMoves do m = mm; break end
		worklistMoves[m] = nil
		local mv = insns[m].move
		local x = getAlias(mv.dst)
		local y = getAlias(mv.src)
		local u, v
		if precolored(y) then u, v = y, x else u, v = x, y end
		if u == v then
			coalescedMoves[m] = true
			addWorklist(u)
		elseif precolored(v) or adj[u][v] then
			constrainedMoves[m] = true
			addWorklist(u); addWorklist(v)
		else
			local canCoalesce
			if precolored(u) then
				canCoalesce = true
				for w in adjacent(v) do
					if not ok(w, u) then canCoalesce = false break end
				end
			else
				canCoalesce = conservative(u, v)
			end
			if canCoalesce then
				coalescedMoves[m] = true
				combine(u, v)
				addWorklist(u)
			else
				activeMoves[m] = true
			end
		end
	end

	local function freezeMoves(u)
		for m in nodeMoves(u) do
			activeMoves[m] = nil
			worklistMoves[m] = nil
			frozenMoves[m] = true
			local mv = insns[m].move
			local v = getAlias(mv.src)
			if getAlias(mv.dst) == getAlias(u) then v = getAlias(mv.dst) == u and getAlias(mv.src) or getAlias(mv.src) end
			local other = (getAlias(mv.dst) == getAlias(u)) and getAlias(mv.src) or getAlias(mv.dst)
			if not moveRelated(other) and degree[other] < K and not precolored(other) then
				removeFrom(freezeWorklist, other)
				table.insert(simplifyWorklist, other)
			end
		end
	end

	local function freeze()
		local u = table.remove(freezeWorklist)
		table.insert(simplifyWorklist, u)
		freezeMoves(u)
	end

	local function selectSpill()
		-- pick highest-degree (cheap heuristic) to spill optimistically
		local best, bestIdx, bestScore
		for idx, t in spillWorklist do
			local cost = (temps[t] and temps[t].spillCost) or 1
			local score = degree[t] / math.max(cost, 0.0001)
			if not bestScore or score > bestScore then
				bestScore = score; best = t; bestIdx = idx
			end
		end
		table.remove(spillWorklist, bestIdx)
		table.insert(simplifyWorklist, best)
		freezeMoves(best)
	end

	-- main loop
	while true do
		if #simplifyWorklist > 0 then simplify()
		elseif next(worklistMoves) then coalesce()
		elseif #freezeWorklist > 0 then freeze()
		elseif #spillWorklist > 0 then selectSpill()
		else break end
	end

	-- assign colors
	local color = {}
	for t in precoloredSet do color[t] = temps[t].precolor end

	local actualSpills = {}
	while #selectStack > 0 do
		local t = table.remove(selectStack)
		onStack[t] = nil
		local okColors = {}
		for _, r in ALLOCATABLE do okColors[r] = true end
		for _, w in adjList[t] do
			local aw = getAlias(w)
			if coloredNodes[aw] or precoloredSet[aw] then
				if color[aw] then okColors[color[aw]] = nil end
			end
		end
		local chosen
		for _, r in ALLOCATABLE do
			if okColors[r] then chosen = r break end
		end
		if not chosen then
			table.insert(actualSpills, t)
		else
			coloredNodes[t] = true
			color[t] = chosen
		end
	end
	for t in coalescedNodes do
		color[t] = color[getAlias(t)]
	end

	return { color = color, spilled = actualSpills, blocks = blocks, aliasOf = getAlias }
end

return regalloc
