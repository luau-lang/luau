--!nonstrict
-- x86_64 frame policy for the shared frame preprocessor (frame.luau). Provides frame-size
-- analysis and the per-statement rewrite: dropping SysV frame setup/teardown (push/pop of
-- rbp + callee-saved, `mov %rsp,%rbp`, `sub/add %rsp`, `leave`, `endbr64`), virtualising
-- rbp/rsp-relative slots as pseudo-registers, and rejecting stack-address escapes.

local x86 = require("./x86_64_isa")

local x86_64_frame = {}

local WIDTH_LETTER = { b = 1, w = 2, l = 4, q = 8 }  -- AT&T size suffix -> bytes

-- rsp is always the stack pointer; rbp is a stack (frame) register only when the function
-- actually establishes it as a frame pointer (`mov %rsp,%rbp`). With frame-pointer omission
-- (the -O2 default), rbp is just an ordinary callee-saved GPR holding values.
local function isStackReg(op, usesRbp) return op and op.kind == "reg" and (op.rc == "sp" or (usesRbp and op.name == "rbp")) end
local function immVal(op) if op and op.kind == "imm" then return op.value or tonumber(op.text) or 0 end return 0 end

-- Pre-scan the frame size (sum of `sub rsp, N`) and whether rbp is used as frame pointer.
function x86_64_frame.analyzeFrame(body)
	local frameSize, usesRbp = 0, false
	for _, st in body do
		if st.kind == "insn" then
			local m = string.lower(st.mnemonic)
			local ops = st.operands
			local base = x86.baseMnemonic(m)
			if (base == "sub") and ops[1] and ops[1].rc == "sp" then
				frameSize += math.abs(immVal(ops[2]))
			elseif base == "mov" and ops[1] and ops[1].name == "rbp" and ops[2] and ops[2].rc == "sp" then
				usesRbp = true
			end
		end
	end
	return frameSize, 0, usesRbp
end

function x86_64_frame.rewrite(st, ctx)
	local emit = ctx.emit
	local usesRbp = ctx.usesFp
	local m = string.lower(st.mnemonic)
	local b = x86.baseMnemonic(m)
	local ops = st.operands

	-- drop CET marker and frame setup/teardown
	if b == "nop" and m == "endbr64" then return end
	if b == "leave" then return end
	if (b == "sub" or b == "add") and ops[1] and ops[1].rc == "sp" then return end
	if b == "mov" and ops[1] and ops[1].name == "rbp" and ops[2] and ops[2].rc == "sp" then return end
	if b == "mov" and ops[1] and ops[1].rc == "sp" and ops[2] and ops[2].name == "rbp" then return end

	-- push/pop of callee-saved registers (and rbp frame pointer): save/restore round-trips -> drop.
	if b == "push" and ops[1] and ops[1].kind == "reg" and (x86.CALLEE_SAVED[ops[1].name] or ops[1].name == "rbp") then return end
	if b == "pop" and ops[1] and ops[1].kind == "reg" and (x86.CALLEE_SAVED[ops[1].name] or ops[1].name == "rbp") then return end

	-- rbp/rsp-relative memory access -> slot pseudo-register (unless it lands in a region).
	local mem
	for _, o in ops do if o.kind == "mem" then mem = o end end
	if mem and isStackReg(mem.base, usesRbp) and not mem.dispSym then
		local off = (mem.dispVal or 0)
		if ctx.regionOf(off) then emit(st); return end
		-- lea of a stack address computes/escapes a pointer into the frame: reject.
		if b == "lea" then ctx.rejectEscape(st.raw or m) end
		-- only plain base+disp slots are virtualisable (no index register)
		if mem.index then
			error("sarcasm: indexed stack-relative access is not supported on x86: " .. (st.raw or m))
		end
		-- Replace the stack-slot memory operand in place with a slot pseudo-register of the
		-- access width, keeping the original mnemonic. This uniformly handles plain moves
		-- (movq), zero/sign-extending loads (movzwl/movsbl/...), and other reg<->slot forms:
		-- e.g. `movzwl 40(%rsp), %r14d` -> `movzwl %<slot:16>, %r14d`. The slot is keyed by
		-- byte offset, so a 64-bit store and a 16-bit reload of the same offset share one web.
		local wb = mem.size
		if not wb then
			local zs = string.match(m, "^mov[zs]([bwlq])")   -- movzwl -> source letter 'w'
			if zs then wb = WIDTH_LETTER[zs]
			else local suf = string.match(m, "([bwlq])$"); wb = (suf and WIDTH_LETTER[suf]) or 8 end
		end
		local slot = ctx.slotReg(off, wb * 8)
		local newops = {}
		for _, o in ops do table.insert(newops, (o == mem) and slot or o) end
		emit({ kind = "insn", mnemonic = st.mnemonic, operands = newops, raw = st.raw, line = st.line, annotation = st.annotation })
		return
	end

	-- reject taking the address of the stack frame (escape)
	if b == "lea" and mem and isStackReg(mem.base, usesRbp) and not ctx.regionOf(mem.dispVal or 0) then
		ctx.rejectEscape(st.raw or m)
	end

	emit(st)
end

return x86_64_frame
