--!nonstrict
-- x86_64 rendering + frame policy for the shared emit loop (emit.luau). Renders IR nodes
-- (dest-first operand model) back to assembly text in the requested syntax ("att"/"intel"),
-- substituting register allocations, and synthesizes the Fil-C prologue/epilogue/frame.
-- Interface used by emit.emitFip: makeColorNum / usedCalleeSaved / computeLayout / header /
-- prologue / epilogue / rootStore / renderInsn / isSelfMove.

local x86 = require("./x86_64_isa")
local x86parse = require("./x86_64_parse")

local x86_64_render = {}

local SUFFIX = { [8] = "b", [16] = "w", [32] = "l", [64] = "q" }

-- mnemonics that take an AT&T size suffix when synthesized (bare)
local SUFFIXABLE = {
	mov=true, add=true, sub=true, ["and"]=true, ["or"]=true, xor=true, cmp=true, test=true,
	shl=true, shr=true, sal=true, sar=true, imul=true, inc=true, dec=true, neg=true, ["not"]=true,
}

local function regName(num, width) return x86parse.regName(num, width) end
x86_64_render.regName = regName

-- physical number for an operand (fixed, tempRef, or lifted opTemp).
local function physOf(node, op, colorNum)
	if not op or op.kind ~= "reg" then return nil end
	if op.fixedNum ~= nil then return op.fixedNum end
	if op.tempRef ~= nil then return colorNum and colorNum(op.tempRef) end
	if node.opTemp and node.opTemp[op] ~= nil then return colorNum and colorNum(node.opTemp[op]) end
	return nil
end

local function renderOperand(node, op, colorNum, syntax)
	local function numOf(o)
		if o.fixedNum ~= nil then return o.fixedNum end
		if o.tempRef ~= nil then return colorNum and colorNum(o.tempRef) end
		if node.opTemp then local t = node.opTemp[o]; if t ~= nil then return colorNum and colorNum(t) end end
		return nil
	end
	if op.kind == "reg" then
		if op.rc == "rip" then return syntax == "att" and "%rip" or "rip" end
		local num = numOf(op)
		local name = (num ~= nil) and regName(num, op.width or 64) or op.name
		return syntax == "att" and ("%" .. name) or name
	elseif op.kind == "imm" then
		return syntax == "att" and ("$" .. op.text) or op.text
	elseif op.kind == "sym" then
		local t = op.text
		if op.reloc == "plt" then t = t .. "@PLT"
		elseif op.reloc == "gotpcrel" then t = t .. "@GOTPCREL" end
		return t
	elseif op.kind == "mem" then
		local ripBase = op.base and op.base.kind == "reg" and op.base.rc == "rip"
		local disp = ""
		if op.dispSym then
			disp = op.dispSym
			if op.dispReloc == "gotpcrel" then disp = disp .. "@GOTPCREL"
			elseif op.dispReloc == "plt" then disp = disp .. "@PLT" end
		end
		if syntax == "att" then
			local dtext = disp
			if op.dispVal and op.dispVal ~= 0 then
				dtext = (dtext ~= "" and dtext or "") .. (op.dispVal < 0 and tostring(op.dispVal) or ("+" .. op.dispVal))
				if disp == "" then dtext = tostring(op.dispVal) end
			end
			if ripBase then return dtext .. "(%rip)" end
			if op.index then
				local b = op.base and ("%" .. regName(numOf(op.base), 64)) or ""
				return dtext .. "(" .. b .. ",%" .. regName(numOf(op.index), 64) .. "," .. tostring(op.scale or 1) .. ")"
			end
			return dtext .. "(%" .. regName(numOf(op.base), 64) .. ")"
		else
			if ripBase then return "[rip + " .. disp .. "]" end
			local parts = {}
			if op.base then table.insert(parts, regName(numOf(op.base), 64)) end
			if op.index then table.insert(parts, regName(numOf(op.index), 64) .. "*" .. tostring(op.scale or 1)) end
			if disp ~= "" then table.insert(parts, disp) end
			if op.dispVal and op.dispVal ~= 0 then
				table.insert(parts, (op.dispVal < 0 and tostring(op.dispVal)) or ("+" .. tostring(op.dispVal)))
			end
			local body = parts[1] or ""
			for k = 2, #parts do
				local p = parts[k]
				if string.sub(p, 1, 1) == "+" or string.sub(p, 1, 1) == "-" then body = body .. " " .. string.sub(p,1,1) .. " " .. string.sub(p,2)
				else body = body .. " + " .. p end
			end
			return "[" .. body .. "]"
		end
	end
	return "?"
end
x86_64_render.renderOperand = renderOperand

-- operation size (bytes) for a synthesized instruction (widest reg, or mem.size).
local function opSize(node, colorNum)
	local w = nil
	for _, op in (node.operands or {}) do
		if op.kind == "reg" and op.rc ~= "rip" then w = math.max(w or 0, op.width or 64)
		elseif op.kind == "mem" and op.size then w = math.max(w or 0, op.size * 8) end
	end
	return w or 64
end

local INTEL_PTR = { [1]="BYTE PTR", [2]="WORD PTR", [4]="DWORD PTR", [8]="QWORD PTR" }

function x86_64_render.renderInsn(node, colorNum, syntax)
	local mnem = node.stmt and node.stmt.mnemonic or node.mnemonic
	local ops = node.stmt and node.stmt.operands or node.operands or {}
	local low = string.lower(mnem)

	-- control-only forms
	if node.indirect and (low == "call" or low == "jmp") then
		local tgt = renderOperand(node, ops[1], colorNum, syntax)
		return "\t" .. low .. "\t" .. (syntax == "att" and ("*" .. tgt) or tgt)
	end

	-- AT&T size suffix. movzx/movsx need an explicit source+dest size pair (movzbl, ...)
	-- since bare `movzx` with a memory source is ambiguous in AT&T; derive it from the
	-- source size (AT&T suffix letter or BYTE/WORD PTR) and the destination register width.
	local outMnem = mnem
	local base = x86.baseMnemonic(mnem)
	if syntax == "att" then
		if (base == "movzx" or base == "movsx") and string.match(low, "^mov[zs][bwlq][bwlq]$") then
			outMnem = low  -- already a full AT&T form (movzbl, movslq, ...): render verbatim
		elseif base == "movzx" or base == "movsx" then
			local letter = string.match(low, "^mov[zs]([bw])")
			local srcBytes = letter and ({ b = 1, w = 2 })[letter] or nil
			if not srcBytes then for _, op in ops do if op.kind == "mem" and op.size then srcBytes = op.size end end end
			if not srcBytes and ops[2] and ops[2].kind == "reg" and ops[2].rc ~= "rip" then
				srcBytes = math.floor((ops[2].width or 16) / 8)
			end
			srcBytes = srcBytes or 1
			local dstW = 32
			for _, op in ops do if op.kind == "reg" and op.rc ~= "rip" then dstW = op.width or 64; break end end
			outMnem = "mov" .. (base == "movzx" and "z" or "s") .. SUFFIX[srcBytes * 8] .. SUFFIX[dstW]
		elseif node.synth then
			if low == "lea" then outMnem = "leaq"
			elseif SUFFIXABLE[low] then
				outMnem = low .. (SUFFIX[opSize(node, colorNum)] or "q")
			end
		elseif SUFFIXABLE[low] then
			-- Lifted node with a bare mnemonic (Intel input) and no register operand to imply
			-- the operand width (e.g. `cmp $0, 8(%rdx)`): AT&T needs an explicit size suffix.
			local hasReg = false
			for _, op in ops do if op.kind == "reg" and op.rc ~= "rip" then hasReg = true; break end end
			if not hasReg then outMnem = low .. (SUFFIX[opSize(node, colorNum)] or "q") end
		end
	end

	-- determine a memory size for intel PTR prefixes (imm+mem ambiguity)
	local memSize = nil
	do
		local w = 0
		local hasMem = false
		for _, op in ops do
			if op.kind == "reg" and op.rc ~= "rip" then w = math.max(w, op.width or 64)
			elseif op.kind == "mem" then hasMem = true; if op.size then w = math.max(w, op.size*8) end end
		end
		if hasMem and syntax == "intel" then memSize = math.floor((w > 0 and w or 64) / 8) end
	end

	local rendered = {}
	for _, op in ops do
		local s = renderOperand(node, op, colorNum, syntax)
		if op.kind == "mem" and syntax == "intel" and memSize and INTEL_PTR[memSize] then
			s = INTEL_PTR[memSize] .. " " .. s
		end
		table.insert(rendered, s)
	end
	if #rendered == 0 then return "\t" .. outMnem end
	-- Operands are stored destination-first (Intel order). AT&T lists them source-first.
	if syntax == "att" and #rendered >= 2 then
		local rev = {}
		for k = #rendered, 1, -1 do table.insert(rev, rendered[k]) end
		rendered = rev
	end
	return "\t" .. outMnem .. "\t" .. table.concat(rendered, ", ")
end

function x86_64_render.isSelfMove(node, colorNum)
	local mnem = node.stmt and node.stmt.mnemonic or node.mnemonic
	if not mnem then return false end
	local low = string.lower(mnem)
	if low ~= "mov" and low ~= "movq" then return false end
	-- a 32-bit `movl %eax, %eax` is NOT a no-op — it zero-extends into %rax. Only a full
	-- 64-bit reg-reg move to the same register is a true no-op we may drop.
	local ops = node.stmt and node.stmt.operands or node.operands
	if not ops or #ops ~= 2 then return false end
	if ops[1].kind ~= "reg" or ops[2].kind ~= "reg" then return false end
	if (ops[1].width or 64) ~= 64 or (ops[2].width or 64) ~= 64 then return false end
	local a = physOf(node, ops[1], colorNum)
	local b = physOf(node, ops[2], colorNum)
	return a ~= nil and a == b
end

-- Frame layout -----------------------------------------------------------
local function computeLayout(calleeSaved, numRoots, spillBytes)
	spillBytes = spillBytes or 0
	local p = #calleeSaved
	local base = 16 + 8 * numRoots + spillBytes  -- prev+origin+roots(+spills)
	-- Maintain 16-byte alignment at call sites: (8 retaddr + 8*p pushes + N) % 16 == 0.
	local target = ((p % 2) == 0) and 8 or 0
	local N = base
	while (N % 16) ~= target do N += 1 end
	return { total = N, rootBase = 16, spillBase = 16 + 8 * numRoots, pushed = calleeSaved }
end
x86_64_render.computeLayout = computeLayout

-- x86 color name -> physical number (via classifyReg).
function x86_64_render.makeColorNum(color)
	return function(t)
		local c = color[t]
		if not c then return nil end
		local r = x86parse.classifyReg(c)
		return r and r.num or nil
	end
end

-- Used callee-saved GPRs in canonical push order.
function x86_64_render.usedCalleeSaved(color)
	local usedCallee = {}
	for _, c in color do usedCallee[c] = true end
	local calleeSaved = {}
	for _, r in x86.CALLEE_SAVED_ORDER do if usedCallee[r] then table.insert(calleeSaved, r) end end
	return calleeSaved
end

-- prologue/epilogue/rootStore emit in the requested syntax.
local function att_(rc) return rc.syntax == "att" end

function x86_64_render.header(rc)
	local out, sigName = rc.out, rc.sigName
	table.insert(out, "\t.text")
	table.insert(out, "\t.globl\t" .. sigName)
	table.insert(out, "\t.p2align\t4")
	table.insert(out, "\t.type\t" .. sigName .. ",@function")
	table.insert(out, sigName .. ":")
end

function x86_64_render.prologue(rc)
	local out, tr, layout = rc.out, rc.tr, rc.layout
	local att = att_(rc)
	local SC = x86.SCRATCH  -- r11
	local mq = att and "movq" or "mov"
	local function ln(s) table.insert(out, s) end
	local function I(v) return att and ("$" .. v) or tostring(v) end
	for _, r in rc.calleeSaved do ln("\t" .. (att and "pushq" or "push") .. "\t" .. (att and ("%" .. r) or r)) end
	ln("\t" .. (att and "subq" or "sub") .. "\t" .. (att and (I(layout.total) .. ", %rsp") or ("rsp, " .. layout.total)))
	-- stack-overflow check: fail when rsp <= limit (limit at [myth=rdi]).
	if att then
		ln("\tcmpq\t%rsp, (%rdi)")
		ln("\tjae\tfilc_stack_overflow_failure@PLT")
	else
		ln("\tcmp\trsp, QWORD PTR [rdi]")
		ln("\tjbe\tfilc_stack_overflow_failure@PLT")
	end
	-- push filc_frame at (rsp): prev, origin; myth->top_frame = rsp.
	ln("\t" .. mq .. "\t" .. (att and ("16(%rdi), %" .. SC) or (SC .. ", QWORD PTR [rdi + 16]")))
	ln("\t" .. mq .. "\t" .. (att and ("%" .. SC .. ", (%rsp)") or ("QWORD PTR [rsp], " .. SC)))
	ln("\t" .. mq .. "\t" .. (att and ("%rsp, 16(%rdi)") or ("QWORD PTR [rdi + 16], rsp")))
	ln("\t" .. (att and "leaq" or "lea") .. "\t" .. (att and (tr.originSym .. "(%rip), %" .. SC) or (SC .. ", [rip + " .. tr.originSym .. "]")))
	ln("\t" .. mq .. "\t" .. (att and ("%" .. SC .. ", 8(%rsp)") or ("QWORD PTR [rsp + 8], " .. SC)))
end

function x86_64_render.epilogue(rc)
	local out, layout = rc.out, rc.layout
	local att = att_(rc)
	local SC = x86.SCRATCH
	local mq = att and "movq" or "mov"
	local mythName = regName(rc.mythNum, 64)
	local function ln(s) table.insert(out, s) end
	local function I(v) return att and ("$" .. v) or tostring(v) end
	-- pop frame: myth->top_frame = frame.prev  (scratch = r11, preserves rax/rdx/rcx)
	ln("\t" .. mq .. "\t" .. (att and ("(%rsp), %" .. SC) or (SC .. ", QWORD PTR [rsp]")))
	ln("\t" .. mq .. "\t" .. (att and ("%" .. SC .. ", 16(%" .. mythName .. ")") or ("QWORD PTR [" .. mythName .. " + 16], " .. SC)))
	ln("\t" .. (att and "addq" or "add") .. "\t" .. (att and (I(layout.total) .. ", %rsp") or ("rsp, " .. layout.total)))
	for i = #rc.calleeSaved, 1, -1 do ln("\t" .. (att and "popq" or "pop") .. "\t" .. (att and ("%" .. rc.calleeSaved[i]) or rc.calleeSaved[i])) end
end

function x86_64_render.rootStore(rc, node)
	local att = att_(rc)
	local mq = att and "movq" or "mov"
	local off = rc.layout.rootBase + 8 * node.index
	local rn = regName(rc.colorNum(node.temp), 64)
	table.insert(rc.out, "\t" .. mq .. "\t" .. (att and ("%" .. rn .. ", " .. off .. "(%rsp)") or ("QWORD PTR [rsp + " .. off .. "], " .. rn)))
end

return x86_64_render
