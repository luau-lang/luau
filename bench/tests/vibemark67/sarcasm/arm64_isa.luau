--!nonstrict
-- ARM64 instruction semantics: classify a parsed instruction into register
-- defs/uses and control-flow behavior. Register operands are reported as the
-- reg operand tables produced by parse.luau (with .num/.width/.name).

local arm64 = {}

-- Data-processing mnemonics whose first operand is the destination (def) and the
-- rest are sources (uses).
local DST_FIRST = {}
for _, m in {
	"mov","movz","mvn","neg","negs","add","adds","sub","subs","adc","adcs","sbc","sbcs",
	"and","ands","orr","orn","eor","eon","bic","bics",
	"lsl","lsr","asr","ror","lslv","lsrv","asrv","rorv",
	"mul","madd","msub","mneg","smull","umull","smulh","umulh","sdiv","udiv",
	"sxtw","sxtb","sxth","uxtw","uxtb","uxth","sxtl","uxtl","clz","cls","rbit","rev","rev16","rev32",
	"adr","adrp","csel","csinc","csinv","csneg","cinc","cinv","cneg","cset","csetm",
	"ubfx","sbfx","ubfm","sbfm","bfi","bfxil","extr","lsl.","abs",
} do DST_FIRST[m] = true end

-- movk / bfi / bfxil read-modify-write the destination.
local DST_RMW = { movk=true, bfi=true, bfxil=true, ins=true }

-- Flags-only (no GPR destination); all reg operands are uses.
local FLAGS_ONLY = { cmp=true, cmn=true, tst=true, ccmp=true, ccmn=true }

-- Loads: first operand is a destination.
local LOADS = {
	ldr=true, ldrb=true, ldrh=true, ldrsb=true, ldrsh=true, ldrsw=true,
	ldur=true, ldurb=true, ldurh=true, ldursb=true, ldursh=true, ldursw=true,
	ldar=true, ldarb=true, ldarh=true, ldapr=true, ldxr=true, ldaxr=true,
}
local LOAD_PAIR = { ldp=true, ldpsw=true, ldnp=true, ldaxp=true, ldxp=true }

-- Stores: first operand is a source.
local STORES = {
	str=true, strb=true, strh=true, stur=true, sturb=true, sturh=true,
	stlr=true, stlrb=true, stlrh=true,
}
local STORE_PAIR = { stp=true, stnp=true }

local COND_BRANCH_ALIASES = {
	beq=true,bne=true,bcs=true,bhs=true,bcc=true,blo=true,bmi=true,bpl=true,
	bvs=true,bvc=true,bhi=true,bls=true,bge=true,blt=true,bgt=true,ble=true,bal=true,
}

local function isGpr(op) return op and op.kind == "reg" and op.rc == "gpr" end

-- Collect reg uses from a memory operand (base + index), and its writeback def.
-- Writeback bases are recorded in `rmw` (they must occupy one register for use+def).
local function memRegs(op, uses, defs, rmw)
	if op.base and isGpr(op.base) then table.insert(uses, op.base) end
	if op.index and isGpr(op.index) then table.insert(uses, op.index) end
	if op.writeback and op.base and isGpr(op.base) then
		table.insert(defs, op.base)  -- writeback updates base
		if rmw then table.insert(rmw, op.base.num) end
	end
end

-- Classify. Returns:
--   { defs = {regop...}, uses = {regop...},
--     control = "none"|"branch"|"condbranch"|"call"|"return"|"indirect"|"indcall",
--     targets = {label...}, fallsThrough = bool, isMove = {dstIdx,srcIdx} | nil }
function arm64.classify(insn)
	local m = string.lower(insn.mnemonic)
	local ops = insn.operands or {}
	local defs, uses = {}, {}
	local rmw = {}  -- regnums that need use+def in the same register (writeback / merge ops)
	local control, targets, fallsThrough = "none", nil, true
	local isMove = nil

	-- Control flow first.
	if m == "b" then
		control, fallsThrough = "branch", false
		targets = {}
		for _, o in ops do if o.kind == "sym" then table.insert(targets, o.text) end end
	elseif m == "br" then
		control, fallsThrough = "indirect", false
		if isGpr(ops[1]) then table.insert(uses, ops[1]) end
	elseif m == "bl" then
		control = "call"
	elseif m == "blr" then
		control = "indcall"
		if isGpr(ops[1]) then table.insert(uses, ops[1]) end
	elseif m == "ret" then
		control, fallsThrough = "return", false
	elseif m == "cbz" or m == "cbnz" then
		control = "condbranch"
		if isGpr(ops[1]) then table.insert(uses, ops[1]) end
		targets = {}
		for _, o in ops do if o.kind == "sym" then table.insert(targets, o.text) end end
	elseif m == "tbz" or m == "tbnz" then
		control = "condbranch"
		if isGpr(ops[1]) then table.insert(uses, ops[1]) end
		targets = {}
		for _, o in ops do if o.kind == "sym" then table.insert(targets, o.text) end end
	elseif string.sub(m, 1, 2) == "b." or COND_BRANCH_ALIASES[m] then
		control = "condbranch"
		targets = {}
		for _, o in ops do if o.kind == "sym" then table.insert(targets, o.text) end end
	end

	if control == "call" or control == "indcall" then
		-- caller-saved clobber is modeled by the transform/regalloc, not here.
		return { defs = defs, uses = uses, control = control, targets = targets,
			fallsThrough = fallsThrough, isMove = isMove }
	end
	if control ~= "none" then
		return { defs = defs, uses = uses, control = control, targets = targets,
			fallsThrough = fallsThrough, isMove = isMove }
	end

	-- Non-control instructions.
	if FLAGS_ONLY[m] then
		for _, o in ops do
			if isGpr(o) then table.insert(uses, o) elseif o.kind == "mem" then memRegs(o, uses, defs, rmw) end
		end
	elseif LOADS[m] then
		if isGpr(ops[1]) then table.insert(defs, ops[1]) end
		for i = 2, #ops do
			local o = ops[i]
			if o.kind == "mem" then memRegs(o, uses, defs, rmw)
			elseif isGpr(o) then table.insert(uses, o) end
		end
	elseif LOAD_PAIR[m] then
		if isGpr(ops[1]) then table.insert(defs, ops[1]) end
		if isGpr(ops[2]) then table.insert(defs, ops[2]) end
		for i = 3, #ops do
			local o = ops[i]
			if o.kind == "mem" then memRegs(o, uses, defs, rmw)
			elseif isGpr(o) then table.insert(uses, o) end
		end
	elseif STORES[m] then
		if isGpr(ops[1]) then table.insert(uses, ops[1]) end
		for i = 2, #ops do
			local o = ops[i]
			if o.kind == "mem" then memRegs(o, uses, defs, rmw)
			elseif isGpr(o) then table.insert(uses, o) end
		end
	elseif STORE_PAIR[m] then
		if isGpr(ops[1]) then table.insert(uses, ops[1]) end
		if isGpr(ops[2]) then table.insert(uses, ops[2]) end
		for i = 3, #ops do
			local o = ops[i]
			if o.kind == "mem" then memRegs(o, uses, defs, rmw)
			elseif isGpr(o) then table.insert(uses, o) end
		end
	elseif DST_RMW[m] then
		if isGpr(ops[1]) then table.insert(defs, ops[1]); table.insert(uses, ops[1]); table.insert(rmw, ops[1].num) end
		for i = 2, #ops do if isGpr(ops[i]) then table.insert(uses, ops[i]) end end
	elseif DST_FIRST[m] then
		if isGpr(ops[1]) then table.insert(defs, ops[1]) end
		for i = 2, #ops do
			local o = ops[i]
			if isGpr(o) then table.insert(uses, o) elseif o.kind == "mem" then memRegs(o, uses, defs, rmw) end
		end
		if m == "mov" and isGpr(ops[1]) and isGpr(ops[2]) and #ops == 2 and not ops[2].shift then
			isMove = { dstIdx = 1, srcIdx = 2 }
		end
	else
		-- Unknown mnemonic: conservatively treat first reg as def, rest as uses.
		local first = true
		for _, o in ops do
			if isGpr(o) then
				if first then table.insert(defs, o); first = false else table.insert(uses, o) end
			elseif o.kind == "mem" then memRegs(o, uses, defs, rmw) end
		end
		return { defs = defs, uses = uses, rmw = rmw, control = "none", fallsThrough = true,
			isMove = isMove, unknown = true }
	end

	return { defs = defs, uses = uses, rmw = rmw, control = control, targets = targets,
		fallsThrough = fallsThrough, isMove = isMove }
end

-- True if `mnemonic` is a load/store that sarcasm recognizes and models (and can
-- therefore bounds-check). A memory operand on any other mnemonic cannot be proven
-- memory-safe and must be rejected.
function arm64.isMemInsn(mnemonic)
	local m = string.lower(mnemonic)
	return (LOADS[m] or STORES[m] or LOAD_PAIR[m] or STORE_PAIR[m]) and true or false
end

arm64.isGpr = isGpr

-- Register/calling-convention configuration (AAPCS64 + Fil-C fast entrypoint) --------
-- Reserved: x8 (dedicated glue/epilogue scratch), x16/x17 (IP0/IP1 veneer), x18 (platform),
-- x29 (fp), x30 (lr), sp, xzr. Allocatable = the remaining GPRs. Mirrors x86_64_isa.
arm64.ALLOCATABLE = {
	"x0","x1","x2","x3","x4","x5","x6","x7","x9","x10","x11","x12","x13","x14","x15",
	"x19","x20","x21","x22","x23","x24","x25","x26","x27","x28",
}
arm64.CALLEE_SAVED = { x19=true,x20=true,x21=true,x22=true,x23=true,x24=true,x25=true,x26=true,x27=true,x28=true }
arm64.CALLEE_SAVED_ORDER = { "x19","x20","x21","x22","x23","x24","x25","x26","x27","x28" }
arm64.CALLER_SAVED = { x0=true,x1=true,x2=true,x3=true,x4=true,x5=true,x6=true,x7=true,x9=true,x10=true,x11=true,x12=true,x13=true,x14=true,x15=true }
arm64.SCRATCH = "x8"       -- non-allocatable glue scratch
arm64.SCRATCH_NUM = 8
-- caller-saved register numbers clobbered by a call (used to model call defs).
arm64.CALLER_SAVED_NUMS = { 0,1,2,3,4,5,6,7,9,10,11,12,13,14,15 }

return arm64
