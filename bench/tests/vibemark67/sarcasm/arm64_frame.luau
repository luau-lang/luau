--!nonstrict
-- ARM64 frame policy for the shared frame preprocessor (frame.luau). Provides frame-size
-- analysis and the per-statement rewrite: dropping AAPCS frame setup, virtualising
-- sp/x29-relative slots as pseudo-registers, and rejecting stack-address escapes.

local arm64_frame = {}

local function isFrameBase(op)
	return op and op.kind == "reg" and (op.name == "sp" or op.name == "x29")
end
local function immVal(op)
	if not op then return 0 end
	if op.kind == "imm" then return op.value or tonumber(op.text) or 0 end
	return 0
end

-- Pre-scan: total frame size (sum of sp decrements) and fp offset (x29 = sp + fpOffset).
function arm64_frame.analyzeFrame(body)
	local frameSize, fpOffset = 0, 0
	for _, st in body do
		if st.kind == "insn" then
			local m = string.lower(st.mnemonic)
			local ops = st.operands
			if m == "sub" and ops[1] and ops[1].name == "sp" and ops[2] and ops[2].name == "sp" then
				frameSize += math.abs(immVal(ops[3]))
			elseif m == "mov" and ops[1] and ops[1].name == "x29" and ops[2] and ops[2].name == "sp" then
				fpOffset = 0
			elseif m == "add" and ops[1] and ops[1].name == "x29" and ops[2] and ops[2].name == "sp" then
				fpOffset = immVal(ops[3])
			else
				for _, o in ops do
					if o.kind == "mem" and o.base and o.base.name == "sp" and o.writeback == "pre" then
						frameSize += math.abs(immVal(o.index))
					end
				end
			end
		end
	end
	return frameSize, fpOffset, false
end

function arm64_frame.rewrite(st, ctx)
	local emit = ctx.emit
	local m = string.lower(st.mnemonic)
	local ops = st.operands

	-- sp-relative accesses that fall inside a fixed-size alloca region are redirected to the
	-- allocation by the transform; pass them through untouched here.
	local function immVal2(op) return (op and op.kind == "imm") and (op.value or tonumber(op.text) or 0) or nil end
	if m == "add" and ops[2] and ops[2].name == "sp" and ops[1] and ops[1].name ~= "sp" then
		local off = immVal2(ops[3])
		if off and ctx.regionOf(off) then emit(st); return end
	end
	if m == "mov" and ops[2] and ops[2].name == "sp" and ops[1] and ops[1].name ~= "sp" then
		if ctx.regionOf(0) then emit(st); return end
	end
	do
		local mem
		for _, o in ops do if o.kind == "mem" then mem = o end end
		if mem and isFrameBase(mem.base) and not mem.writeback then
			local base = (mem.base.name == "sp") and 0 or ctx.fpOffset
			if ctx.regionOf(base + immVal(mem.index)) then emit(st); return end
		end
	end

	-- sp adjustments and fp setup: drop.
	if (m == "sub" or m == "add") and ops[1] and ops[1].name == "sp" then return end
	if (m == "mov" or m == "add") and ops[1] and ops[1].name == "x29" then return end
	if m == "mov" and ops[1] and ops[1].name == "sp" then return end  -- e.g. `mov sp, x29` (restore)

	local mem = nil
	for _, o in ops do if o.kind == "mem" then mem = o end end
	if mem and isFrameBase(mem.base) then
		-- Only writeback stp/ldp (the classic `stp x29,x30,[sp,-N]!`/`ldp ...,[sp],N`) is pure
		-- frame setup we drop. A plain `str/ldr x30` (or x29) must NOT be dropped: in a leaf
		-- function gcc reuses x30 (lr) as a general register and spills it, so those are real
		-- values to virtualise. A genuine fp/lr save becomes a dead slot.
		if mem.writeback then return end  -- frame-setup writeback store
		local base = (mem.base.name == "sp") and 0 or ctx.fpOffset
		local off = base + immVal(mem.index)
		if m == "str" or m == "stur" then
			ctx.checkBounds(off, ops[1].width == 32 and 4 or 8, true, st.raw)
			emit({ kind = "insn", mnemonic = "mov", operands = { ctx.slotReg(off, ops[1].width), ops[1] }, raw = st.raw })
		elseif m == "ldr" or m == "ldur" then
			ctx.checkBounds(off, ops[1].width == 32 and 4 or 8, false, st.raw)
			emit({ kind = "insn", mnemonic = "mov", operands = { ops[1], ctx.slotReg(off, ops[1].width) }, raw = st.raw })
		elseif m == "stp" then
			ctx.checkBounds(off, 16, true, st.raw)
			emit({ kind = "insn", mnemonic = "mov", operands = { ctx.slotReg(off, ops[1].width), ops[1] }, raw = st.raw })
			emit({ kind = "insn", mnemonic = "mov", operands = { ctx.slotReg(off + 8, ops[2].width), ops[2] }, raw = st.raw })
		elseif m == "ldp" then
			ctx.checkBounds(off, 16, false, st.raw)
			emit({ kind = "insn", mnemonic = "mov", operands = { ops[1], ctx.slotReg(off, ops[1].width) }, raw = st.raw })
			emit({ kind = "insn", mnemonic = "mov", operands = { ops[2], ctx.slotReg(off + 8, ops[2].width) }, raw = st.raw })
		else
			emit(st)
		end
		return
	end

	-- Reject taking the address of the stack frame (potential escape).
	if (m == "add" or m == "mov" or m == "sub") then
		for _, o in ops do
			if o.kind == "reg" and o.name == "sp" and ops[1] and ops[1].name ~= "sp" then
				ctx.rejectEscape(st.raw or m)
			end
		end
	end

	emit(st)
end

return arm64_frame
