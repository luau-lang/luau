--!nonstrict
-- Shared FIP emission: render a transformed function `tr` (its IR nodes + register coloring)
-- into assembly text. The reload-elimination walk, spill store/reload bookkeeping, no-op
-- self-move dropping, and control-flow region resets live here ONCE; the arch-specific
-- rendering (instruction text, prologue/epilogue/frame layout, color decoding) is supplied
-- by a `render` module (arm64_render / x86_64_render) implementing:
--   makeColorNum(color)->fn, usedCalleeSaved(color)->list, computeLayout(cs,nRoots,spillBytes),
--   header(rc), prologue(rc), epilogue(rc), rootStore(rc,node),
--   renderInsn(node,colorNum,syntax)->string, isSelfMove(node,colorNum)->bool.
-- `rc` = { out, tr, layout, colorNum, calleeSaved, mythNum, sigName, syntax }.

local emit = {}

function emit.emitFip(out, tr, colorResult, render, syntax)
	-- the driver resolves all spills before calling us; this only trips on an internal bug
	assert(#colorResult.spilled == 0, "sarcasm: internal error: uncolored temps remain after spilling")
	local color = colorResult.color
	local colorNum = render.makeColorNum(color)
	local calleeSaved = render.usedCalleeSaved(color)
	local layout = render.computeLayout(calleeSaved, tr.numRoots, (tr.numSpillSlots or 0) * 8)
	local sigName = "pizlonatedFIP" .. tostring(tr.sigDesc.signature) .. "_" .. tr.name
	local rc = {
		out = out, tr = tr, layout = layout, colorNum = colorNum,
		calleeSaved = calleeSaved, mythNum = colorNum(tr.vMyth), sigName = sigName, syntax = syntax,
	}

	render.header(rc)

	-- Reload elimination: track which physical register currently holds each spill slot, and
	-- drop a reload whose destination register already holds that slot's value. `avail` is
	-- reset at every control-flow boundary (labels, branches, calls, prologue/epilogue).
	local avail = {}  -- spillOff -> physReg num
	local function invalidateReg(r)
		if r == nil then return end
		for off, reg in avail do if reg == r then avail[off] = nil end end
	end

	for _, node in tr.fipNodes do
		if node.kind == "prologue" then
			avail = {}
			render.prologue(rc)
		elseif node.kind == "epilogue" then
			avail = {}
			render.epilogue(rc)
		elseif node.kind == "rootstore" then
			render.rootStore(rc, node)
		elseif node.kind == "label" then
			avail = {}
			table.insert(out, node.name .. ":")
		elseif node.kind == "insn" then
			if node.spillReload then
				local r = colorNum(node.spillTemp)
				if avail[node.spillOff] == r then
					-- redundant: the slot's value is already live in r; drop the reload
				else
					invalidateReg(r)               -- r is overwritten by the load
					avail[node.spillOff] = r
					table.insert(out, render.renderInsn(node, colorNum, syntax))
				end
			elseif node.spillStore then
				avail[node.spillOff] = colorNum(node.spillTemp)
				table.insert(out, render.renderInsn(node, colorNum, syntax))
			else
				-- drop no-op self-moves left behind by register coalescing
				if not render.isSelfMove(node, colorNum) then
					table.insert(out, render.renderInsn(node, colorNum, syntax))
				end
				for _, t in (node.defTemps or {}) do invalidateReg(colorNum(t)) end
				local ctl = node.control or (node.c and node.c.control) or "none"
				if ctl ~= "none" then avail = {} end  -- branch/call/ret ends the region
			end
		elseif node.kind == "raw" then
			table.insert(out, node.text)
		end
	end

	table.insert(out, "\t.size\t" .. sigName .. ", .-" .. sigName)
	table.insert(out, "")
	return layout
end

return emit
