--!nonstrict
-- ARM64 rendering + frame policy for the shared emit loop (emit.luau). Renders IR
-- instruction nodes (lifted or synthesized) back to AArch64 assembly text substituting
-- register allocations, and synthesizes the Fil-C prologue/epilogue/frame. Interface used
-- by emit.emitFip: makeColorNum / usedCalleeSaved / computeLayout / header / prologue /
-- epilogue / rootStore / renderInsn / isSelfMove.

local isa = require("./arm64_isa")

local arm64_render = {}

local function regName(num: number, width: number): string
	if num == -1 then return width == 32 and "wzr" or "xzr" end
	if num == -2 then return "sp" end
	return (width == 32 and "w" or "x") .. tostring(num)
end
arm64_render.regName = regName

-- Render a shift/extend suffix.
local function renderShift(sh): string
	if not sh then return "" end
	if sh.amountText then return " " .. sh.op .. " " .. sh.amountText end
	return " " .. sh.op
end

-- Render one operand. `numOf(op)` returns the physical reg number to use for a reg
-- operand (or nil to use its original name).
local function renderOperand(op, numOf): string
	if op.kind == "reg" then
		local num = numOf and numOf(op)
		local name
		if num ~= nil then name = regName(num, op.width) else name = op.name end
		return name .. renderShift(op.shift)
	elseif op.kind == "imm" then
		return "#" .. op.text
	elseif op.kind == "sym" then
		if op.reloc then return ":" .. op.reloc .. ":" .. op.text end
		return op.text
	elseif op.kind == "mem" then
		local parts = {}
		table.insert(parts, renderOperand(op.base, numOf))
		if op.index then table.insert(parts, renderOperand(op.index, numOf)) end
		if op.shift then
			local s = op.shift.op
			if op.shift.amountText then s = s .. " " .. op.shift.amountText end
			table.insert(parts, s)
		end
		local inner = "[" .. table.concat(parts, ", ") .. "]"
		if op.writeback == "pre" then inner = inner .. "!" end
		if op.writeback == "post" then
			if op.postShift then
				inner = inner .. ", " .. op.postShift.op .. " " .. (op.postShift.amountText or "")
			end
		end
		return inner
	end
	return "?"
end
arm64_render.renderOperand = renderOperand

-- Render a full instruction node. colorOf: tempId -> regNum (or nil). Handles both lifted
-- nodes (node.stmt + node.opTemp) and synthesized nodes (node.mnemonic + node.operands with
-- op.tempRef / op.fixedNum). The `syntax` arg is ignored (ARM has a single output syntax).
function arm64_render.renderInsn(node, colorOf, syntax): string
	local st = node.stmt or { mnemonic = node.mnemonic, operands = node.operands }
	local function numOf(op)
		if op.fixedNum ~= nil then return op.fixedNum end
		if op.tempRef ~= nil then return colorOf and colorOf(op.tempRef) end
		if node.opTemp then
			local t = node.opTemp[op]
			if t ~= nil then return colorOf and colorOf(t) end
		end
		return nil
	end
	local function shiftText(sh)
		if sh.amountText then return sh.op .. " " .. sh.amountText end
		return sh.op
	end
	local ops = {}
	for _, op in st.operands do
		if op.kind == "mem" and op.writeback == "post" and op.index then
			local base = "[" .. renderOperand(op.base, numOf) .. "]"
			table.insert(ops, base)
			table.insert(ops, renderOperand(op.index, numOf))
		elseif op.kind == "mem" and op.writeback == "post" and op.postShift then
			local base = "[" .. renderOperand(op.base, numOf) .. "]"
			table.insert(ops, base .. ", " .. op.postShift.op .. " " .. (op.postShift.amountText or ""))
		elseif op.kind == "reg" and op.shift then
			local saved = op.shift
			op.shift = nil
			table.insert(ops, renderOperand(op, numOf))
			op.shift = saved
			table.insert(ops, shiftText(saved))
		elseif op.kind == "imm" and op.shift then
			-- a shifted immediate is a separate comma operand, e.g. `movk x6, #0xbc5, lsl 16`;
			-- dropping the shift would silently corrupt the built constant.
			table.insert(ops, "#" .. op.text)
			table.insert(ops, shiftText(op.shift))
		else
			table.insert(ops, renderOperand(op, numOf))
		end
	end
	if #ops == 0 then return "\t" .. st.mnemonic end
	return "\t" .. st.mnemonic .. "\t" .. table.concat(ops, ", ")
end

-- Physical register number for a rendered operand; nil if not a plain register.
local function physOf(node, op, colorOf)
	if not op or op.kind ~= "reg" then return nil end
	if op.fixedNum ~= nil then return op.fixedNum end
	if op.tempRef ~= nil then return colorOf and colorOf(op.tempRef) end
	if node.opTemp and node.opTemp[op] ~= nil then return colorOf and colorOf(node.opTemp[op]) end
	return nil
end

-- True if `node` renders to `mov Xd, Xd` (same physical register) — a no-op left behind by
-- coalescing. `mov w0, w0` (32-bit) is NOT a no-op (it zero-extends into x0), so only a
-- 64-bit self-move is droppable.
function arm64_render.isSelfMove(node, colorOf)
	local mnem = node.stmt and node.stmt.mnemonic or node.mnemonic
	if not mnem or string.lower(mnem) ~= "mov" then return false end
	local ops = node.stmt and node.stmt.operands or node.operands
	if not ops or #ops ~= 2 then return false end
	if ops[1].kind ~= "reg" or ops[2].kind ~= "reg" or ops[2].shift then return false end
	if (ops[1].width or 64) ~= 64 or (ops[2].width or 64) ~= 64 then return false end
	local a = physOf(node, ops[1], colorOf)
	local b = physOf(node, ops[2], colorOf)
	return a ~= nil and a == b
end

-- Frame layout -----------------------------------------------------------
local function align16(x) return math.floor((x + 15) / 16) * 16 end

-- ARM color name -> physical number (sp -> -2, xzr -> -1, xN/wN -> N).
function arm64_render.makeColorNum(color)
	return function(t)
		local c = color[t]
		if not c then return nil end
		if c == "sp" then return -2 elseif c == "xzr" then return -1 end
		return tonumber(string.match(c, "%d+"))
	end
end

-- Used callee-saved GPRs (x19..x28) in canonical order.
function arm64_render.usedCalleeSaved(color)
	local usedCallee = {}
	for _, r in isa.ALLOCATABLE do
		if isa.CALLEE_SAVED[r] then
			for _, c in color do if c == r then usedCallee[r] = true break end end
		end
	end
	local calleeSaved = {}
	for _, r in isa.CALLEE_SAVED_ORDER do
		if usedCallee[r] then table.insert(calleeSaved, r) end
	end
	return calleeSaved
end

function arm64_render.computeLayout(calleeSaved, numRoots, spillBytes)
	spillBytes = spillBytes or 0
	local frameData = align16(16 + 8 * numRoots + spillBytes)  -- prev+origin+roots(+spills)
	local saveList = { "x29", "x30" }                          -- fp/lr first (top), then callee-saved
	for _, r in calleeSaved do table.insert(saveList, r) end
	local numSlots = math.ceil(#saveList / 2)
	local saveBytes = numSlots * 16
	local total = align16(frameData + saveBytes)
	local pairs = {}
	for k = 0, numSlots - 1 do
		local r1, r2 = saveList[2 * k + 1], saveList[2 * k + 2]
		local off = total - 16 - 16 * k
		table.insert(pairs, { r1 = r1, r2 = r2, off = off })
	end
	return {
		total = total, fpOff = total - 16, pairs = pairs, frameData = frameData,
		rootBase = 16, spillBase = 16 + 8 * numRoots,
	}
end

-- `op dst, src, #imm` (add/sub), splitting when imm exceeds the 12-bit range so large
-- frames still assemble (imm up to ~16 MB via a shifted second op).
local function emitAddSub(out, op, dst, src, imm)
	if imm <= 4095 then
		table.insert(out, "\t" .. op .. "\t" .. dst .. ", " .. src .. ", #" .. imm)
	elseif (imm % 4096) == 0 then
		table.insert(out, "\t" .. op .. "\t" .. dst .. ", " .. src .. ", #" .. math.floor(imm / 4096) .. ", lsl #12")
	else
		table.insert(out, "\t" .. op .. "\t" .. dst .. ", " .. src .. ", #" .. (imm % 4096))
		table.insert(out, "\t" .. op .. "\t" .. dst .. ", " .. dst .. ", #" .. math.floor(imm / 4096) .. ", lsl #12")
	end
end

-- Save/restore the callee-saved pairs. stp/ldp only allow a ±512 (÷8) offset, so once the
-- callee-saved region sits above that we compute its base once into x8 (reserved scratch)
-- and address the pairs off x8 with small offsets.
local function emitSaves(out, layout, mem)  -- mem = "st"/"ld"
	local savesNeedBase = layout.fpOff > 504
	local base = savesNeedBase and "x8" or "sp"
	if savesNeedBase then emitAddSub(out, "add", "x8", "sp", layout.frameData) end
	local pairOp, singleOp = (mem == "st") and "stp" or "ldp", (mem == "st") and "str" or "ldr"
	for _, p in layout.pairs do
		local off = savesNeedBase and (p.off - layout.frameData) or p.off
		if p.r2 then
			table.insert(out, "\t" .. pairOp .. "\t" .. p.r1 .. ", " .. p.r2 .. ", [" .. base .. ", #" .. off .. "]")
		else
			table.insert(out, "\t" .. singleOp .. "\t" .. p.r1 .. ", [" .. base .. ", #" .. off .. "]")
		end
	end
end

function arm64_render.header(rc)
	local out, sigName = rc.out, rc.sigName
	table.insert(out, "\t.text")
	table.insert(out, "\t.globl\t" .. sigName)
	table.insert(out, "\t.p2align\t2")
	table.insert(out, "\t.type\t" .. sigName .. ",@function")
	table.insert(out, sigName .. ":")
end

function arm64_render.prologue(rc)
	local out, tr, layout = rc.out, rc.tr, rc.layout
	emitAddSub(out, "sub", "sp", "sp", layout.total)
	emitSaves(out, layout, "st")
	emitAddSub(out, "add", "x29", "sp", layout.fpOff)
	-- stack overflow check (x0 = myth at entry)
	table.insert(out, "\tldr\tx9, [x0]")
	table.insert(out, "\tcmp\tsp, x9")
	table.insert(out, "\tb.hs\t.Lsovok_" .. tr.name)
	table.insert(out, "\tb\tfilc_stack_overflow_failure")
	table.insert(out, ".Lsovok_" .. tr.name .. ":")
	-- push filc_frame at sp+0
	table.insert(out, "\tldr\tx9, [x0, #16]")
	table.insert(out, "\tmov\tx8, sp")
	table.insert(out, "\tadrp\tx10, " .. tr.originSym)
	table.insert(out, "\tadd\tx10, x10, :lo12:" .. tr.originSym)
	table.insert(out, "\tstp\tx9, x10, [sp, #0]")
	table.insert(out, "\tstr\tx8, [x0, #16]")
end

function arm64_render.epilogue(rc)
	local out, layout, mythNum = rc.out, rc.layout, rc.mythNum
	-- pop frame: myth->top_frame = frame.prev  (myth in vMyth's color)
	table.insert(out, "\tldr\tx8, [sp, #0]")
	table.insert(out, "\tstr\tx8, [x" .. mythNum .. ", #16]")
	emitSaves(out, layout, "ld")   -- restore callee-saved (reverse not required for correctness)
	emitAddSub(out, "add", "sp", "sp", layout.total)
end

function arm64_render.rootStore(rc, node)
	local off = rc.layout.rootBase + 8 * node.index
	table.insert(rc.out, "\tstr\tx" .. rc.colorNum(node.temp) .. ", [sp, #" .. off .. "]")
end

return arm64_render
