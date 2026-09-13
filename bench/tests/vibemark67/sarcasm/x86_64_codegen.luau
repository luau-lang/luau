--!nonstrict
-- x86_64 codegen: the per-arch instruction emitter used by the shared GIMSO transform
-- (transform.luau). It exposes (a) static config — calling convention, operand analysis —
-- and (b) make(ctx) returning a `cg` of neutral "micro-ops" the shared transform composes
-- into access checks, invisicap loads/stores, calls, pollchecks and allocations. Output is
-- always AT&T (see x86_64_render); the IR nodes are dest-first.

local build = require("./build")
local isa = require("./x86_64_isa")
local xparse = require("./x86_64_parse")

local M = {}

-- ISA passthrough ---------------------------------------------------------
M.classify = isa.classify
M.baseMnemonic = isa.baseMnemonic
M.precolorName = function(num) return xparse.regName(num, 64) end
M.ALLOCATABLE = isa.ALLOCATABLE

-- Calling convention (numbers are register encodings; see x86_64_isa).
-- Fast entrypoint: rdi=myth, (rdx,rcx)=arg0, (r8,r9)=arg1; ret al=flags, rdx=iv, rcx=lo.
-- Runtime (SysV) calls: rdi,rsi,rdx,... ; result in rax.  yolo params arrive in SysV regs.
M.cc = {
	myth = isa.CC.myth, retFlag = isa.CC.retFlag, retIv = isa.CC.retIv, retLo = isa.CC.retLo,
	argIv = isa.CC.argIv, argLo = isa.CC.argLo,
	yoloArg = function(k) return isa.SYSV[k] end,
	rtArg = function(k) return isa.SYSV[k] end,   -- runtime-call arg registers
	rtRet = 0,                                    -- rax
}

-- Static operand analysis -------------------------------------------------
function M.memOperand(stmt)
	for _, op in stmt.operands do if op.kind == "mem" then return op end end
	return nil
end

local WIDTH_LETTER = { b = 1, w = 2, l = 4, q = 8 }
function M.accessSizeAlign(stmt)
	local mnem = string.lower(stmt.mnemonic)
	local zs = string.match(mnem, "^mov[zs]([bw])")  -- movzbl/movswq/... source width letter
	if zs then local s = WIDTH_LETTER[zs]; return s, s end
	local mem
	for _, op in stmt.operands do if op.kind == "mem" then mem = op end end
	if (mnem == "movzx" or mnem == "movsx") and mem and mem.size then return mem.size, mem.size end
	local sz = (mem and mem.size) or stmt.opsize
	if not sz then
		for _, op in stmt.operands do
			if op.kind == "reg" and op.rc == "gpr" then sz = math.floor((op.width or 64) / 8); break end
		end
	end
	sz = sz or 8
	return sz, sz
end

-- x86 uses `lea`/`mov` for address arithmetic, so there is no `mov rd, rsp` alloca-base
-- idiom to recognize for fixed-size regions (gcc emits `lea`, handled as a region result).
function M.allocaRegionBase(stmt) return nil end

-- Is this a load/store the transform models? On x86 most instructions can carry a memory
-- operand (it is a load/store architecture only in spirit), so any mem operand is a real
-- access to bounds-check; nothing to reject as "mem on a non-load/store".
function M.isMemInsn(mnem) return true end

-- `alloca size` size operand: `sub rsp, X` -> X is operands[2] (dest-first).
function M.allocaSizeOperand(stmt) return stmt.operands[2] end
-- a store writes memory when the (dest-first) first operand is the memory operand.
function M.isStoreInsn(stmt) return stmt.operands[1] and stmt.operands[1].kind == "mem" end

-- x86 conditions for the neutral cmp/test conditions used by the shared transform.
local JCC = { ult = "jb", uge = "jae", ugt = "ja", ule = "jbe", eq = "je", ne = "jne" }

function M.make(ctx)
	local emit, allocTemp, pColored, temps = ctx.emit, ctx.allocTemp, ctx.pColored, ctx.temps
	local pf = ctx.pf
	local cg = { cc = M.cc }

	local function rt(t, w) return build.rt(t, w) end
	local function branch(cc_, lbl)
		emit(build.insn(cc_, { build.sym(lbl) }, { control = "condbranch", targets = {lbl} }))
	end

	function cg.move(dst, src, coalesce)
		emit(build.insn("mov", { rt(dst), rt(src) },
			{ defs = {dst}, uses = {src}, move = coalesce and { dst = dst, src = src } or nil }))
	end
	function cg.moveImm(dst, v) emit(build.insn("mov", { rt(dst), build.imm(v) }, { defs = {dst} })) end
	function cg.moveZero(dst) emit(build.insn("mov", { rt(dst), build.imm(0) }, { defs = {dst} })) end
	function cg.movBig(dst, hex) emit(build.insn("mov", { rt(dst), build.imm(hex) }, { defs = {dst} })) end

	function cg.load(dst, base, off, size)
		-- byte loads zero-extend into a 32-bit dest (mirrors ARM `ldrb`); wider loads are plain.
		local mnem = (size == 1) and "movzbl" or "mov"
		local w = (size == 1) and 32 or (size and size * 8 or 64)
		emit(build.insn(mnem, { rt(dst, w),
			build.memx(rt(base), { dispVal = off ~= 0 and off or nil }) }, { defs = {dst}, uses = {base} }))
	end
	function cg.loadIndexed(dst, base, idx, scale, size)
		emit(build.insn("mov", { rt(dst, size and size * 8 or 64),
			build.memx(rt(base), { index = rt(idx), scale = scale or 1 }) }, { defs = {dst}, uses = {base, idx} }))
	end
	function cg.store(src, base, off)
		emit(build.insn("mov", { build.memx(rt(base), { dispVal = off ~= 0 and off or nil }), rt(src) },
			{ uses = {src, base} }))
	end
	function cg.storeIndexed(src, base, idx, scale)
		emit(build.insn("mov", { build.memx(rt(base), { index = rt(idx), scale = scale or 1 }), rt(src) },
			{ uses = {src, base, idx} }))
	end
	-- dst = base + off + idx*scale (lea; base/idx are temps, may be nil)
	function cg.effAddr(dst, base, off, idx, scale)
		local uses, mopts = {}, { dispVal = (off ~= 0) and off or nil }
		if base then table.insert(uses, base) end
		if idx then mopts.index = rt(idx); mopts.scale = scale or 1; table.insert(uses, idx) end
		emit(build.insn("lea", { rt(dst), build.memx(base and rt(base) or nil, mopts) }, { defs = {dst}, uses = uses }))
	end
	function cg.addImm(dst, src, imm)  -- dst = src + imm
		emit(build.insn("lea", { rt(dst), build.memx(rt(src), { dispVal = imm }) }, { defs = {dst}, uses = {src} }))
	end
	function cg.subImm(dst, src, imm)  -- dst = src - imm
		emit(build.insn("lea", { rt(dst), build.memx(rt(src), { dispVal = -imm }) }, { defs = {dst}, uses = {src} }))
	end
	function cg.subReg(dst, a, b)      -- dst = a - b
		cg.move(dst, a)
		emit(build.insn("sub", { rt(dst), rt(b) }, { defs = {dst}, uses = {dst, b} }))
	end
	function cg.andImmInPlace(dst, mask)  -- dst = dst & mask
		emit(build.insn("and", { rt(dst), build.imm(mask) }, { defs = {dst}, uses = {dst} }))
	end
	function cg.maskLow48(dst)  -- dst = dst & 0xffffffffffff  (shl 16; shr 16 clears top 16)
		emit(build.insn("shl", { rt(dst), build.imm(16) }, { defs = {dst}, uses = {dst} }))
		emit(build.insn("shr", { rt(dst), build.imm(16) }, { defs = {dst}, uses = {dst} }))
	end

	function cg.branchZero(reg, lbl)
		emit(build.insn("test", { rt(reg), rt(reg) }, { uses = {reg} })); branch("je", lbl)
	end
	function cg.branchNonZero(reg, lbl)
		emit(build.insn("test", { rt(reg), rt(reg) }, { uses = {reg} })); branch("jne", lbl)
	end
	function cg.cmpBranch(a, b, cond, lbl)
		emit(build.insn("cmp", { rt(a), rt(b) }, { uses = {a, b} })); branch(JCC[cond], lbl)
	end
	function cg.cmpImmBranch(a, imm, cond, lbl)
		emit(build.insn("cmp", { rt(a), build.imm(imm) }, { uses = {a} })); branch(JCC[cond], lbl)
	end
	-- branch if (reg & mask) cond 0   (cond "eq"/"ne"). x86 `test` only takes a 32-bit
	-- (sign-extended) immediate, so a mask that doesn't fit imm32 must go through a register.
	function cg.testMaskBranch(reg, mask, cond, lbl, width)
		local mval = (type(mask) == "number") and mask or tonumber(mask)
		local big = (mval == nil) or mval > 0x7fffffff or mval < -0x80000000
		if big then
			local m = allocTemp({ role = "scratch" }); cg.movBig(m, mask)
			emit(build.insn("test", { rt(reg), rt(m) }, { uses = {reg, m} }))
		else
			emit(build.insn("test", { rt(reg, width), build.imm(mask) }, { uses = {reg} }))
		end
		branch(cond == "eq" and "je" or "jne", lbl)
	end
	function cg.jump(lbl)
		emit(build.insn("jmp", { build.sym(lbl) }, { control = "branch", targets = {lbl}, fallsThrough = false }))
	end
	function cg.label(name) emit(build.label(name)) end

	function cg.call(sym, uses, clobbers, indirect)
		local node = build.insn("call", { build.sym(sym, indirect and nil or "plt") },
			{ control = "call", uses = uses, defs = clobbers })
		if indirect then node.indirect = true end
		emit(node)
	end
	function cg.callerSavedClobbers()
		local clob = {}
		for _, rn in isa.CALLER_SAVED_NUMS do table.insert(clob, pColored(rn)) end
		return clob
	end
	function cg.leaGlobal(destPhys, sym)  -- destPhys is a physical reg num
		emit(build.insn("lea", { build.rp(destPhys), build.memx({ kind = "reg", rc = "rip" }, { dispSym = sym }) },
			{ defs = { pColored(destPhys) } }))
	end

	-- effective address + capability selection for a memory operand.
	function cg.computeAddr(node, mem)
		local baseTemp = mem.base and node.opTemp[mem.base] or nil
		local indexTemp = (mem.index and mem.index.kind == "reg") and node.opTemp[mem.index] or nil
		local capLo
		if baseTemp and pf.isPtr[baseTemp] then capLo = pf.lowerTemp[baseTemp]
		elseif indexTemp and pf.isPtr[indexTemp] then capLo = pf.lowerTemp[indexTemp] end
		local disp = mem.dispVal or 0
		local effTemp = baseTemp
		if indexTemp or disp ~= 0 or not baseTemp then
			effTemp = allocTemp({ role = "eff" })
			cg.effAddr(effTemp, baseTemp, disp, indexTemp, mem.scale or 1)
		end
		return effTemp, capLo
	end

	-- store-barrier marking-state test: branch to `lbl` if FUGC marking is active.
	function cg.markingStateNonzeroBranch(lbl)
		local msp = allocTemp({ role = "scratch" })
		emit(build.insn("mov", { rt(msp), build.memx({ kind = "reg", rc = "rip" },
			{ dispSym = "filc_current_marking_state", dispReloc = "gotpcrel" }) }, { defs = {msp} }))
		local msv = allocTemp({ role = "scratch" })
		emit(build.insn("mov", { rt(msv, 32), build.memx(rt(msp), {}) }, { defs = {msv}, uses = {msp} }))
		emit(build.insn("test", { rt(msv, 32), rt(msv, 32) }, { uses = {msv} })); branch("jne", lbl)
	end
	-- branch to exception epilogue if the call's return flag bit 0 is set (al bit0).
	function cg.exceptionBranch(lbl)
		emit(build.insn("test", { build.rp(M.cc.retFlag, 8), build.imm(1) }, { uses = { pColored(M.cc.retFlag) } }))
		branch("jne", lbl)
	end

	function cg.clearRetFlag()
		emit(build.insn("mov", { build.rp(M.cc.retFlag, 32), build.imm(0) }, { defs = { pColored(M.cc.retFlag) } }))
	end
	-- passthrough special-casing: cltq -> renameable movslq on the rax web; lea does not
	-- access memory (its [mem] is an address computation), so pass it through unchecked.
	function cg.lowerSpecial(node, mem)
		local base = isa.baseMnemonic(node.stmt.mnemonic)
		if base == "cltq" then
			local t = node.defTemps and node.defTemps[1]
			if t then emit(build.insn("movslq", { rt(t, 64), rt(t, 32) }, { defs = {t}, uses = {t} })) else emit(node) end
			return true
		elseif base == "lea" then
			emit(node); return true
		end
		return false
	end
	function cg.checkMemInsn(node, mem) end  -- x86: any memory operand is a real access

	-- one access-check-fail stub, appended to `list` (arch-specific arg regs + rip origin).
	function cg.failStub(list, lbl, ivTemp, loTemp, acoSym)
		local a1, a2, a3 = M.cc.rtArg(1), M.cc.rtArg(2), M.cc.rtArg(3)
		table.insert(list, build.label(lbl))
		table.insert(list, build.insn("mov", { build.rp(a1), rt(ivTemp) }, { defs = { pColored(a1) }, uses = {ivTemp} }))
		if loTemp then
			table.insert(list, build.insn("mov", { build.rp(a2), rt(loTemp) }, { defs = { pColored(a2) }, uses = {loTemp} }))
		else
			table.insert(list, build.insn("mov", { build.rp(a2), build.imm(0) }, { defs = { pColored(a2) } }))
		end
		table.insert(list, build.insn("lea", { build.rp(a3), build.memx({ kind = "reg", rc = "rip" }, { dispSym = acoSym }) },
			{ defs = { pColored(a3) } }))
		table.insert(list, build.insn("call", { build.sym("filc_optimized_access_check_fail", "plt") },
			{ control = "call", uses = { pColored(a1), pColored(a2), pColored(a3) } }))
	end

	return cg
end

return M
