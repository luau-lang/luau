--!nonstrict
-- Frame preprocessing (architecture-independent skeleton). Rewrites a function body so the
-- register allocator can treat the input's own stack frame as virtual locals:
--   * the input's frame setup/teardown (sp/rsp adjustment, fp establishment, callee-saved
--     save/restore, writeback frame stores, CET/`leave`) is DROPPED — sarcasm synthesizes
--     its own Fil-C frame.
--   * every other stack-pointer-relative load/store becomes a copy to/from a slot pseudo-
--     register keyed by its byte offset, so reaching-definitions connects producers and
--     consumers of that slot (uniformly handling ABI callee-saved round-trips and genuine
--     local spills).
--   * taking the address of the stack frame (a potential escape) is REJECTED.
--   * a stack access outside the frame [0, frameSize) is a compile-time error.
--
-- The per-stmt classification/rewrite is architecture-specific and supplied via `policy`
-- (arm64_frame / x86_64_frame): `policy.analyzeFrame(body) -> frameSize, fpOffset, usesFp`
-- and `policy.rewrite(stmt, ctx)`, where the shared `ctx` provides the slot constructor,
-- region lookup, bounds check, escape rejection, and the output sink.

local frame = {}

-- Slot pseudo-registers get reg numbers in a high range so they never collide with real
-- GPRs (0..30) and are always virtual (never precolored).
local SLOT_BASE = 1000

local function slotReg(off, width)
	return { kind = "reg", rc = "gpr", num = SLOT_BASE + off, width = width or 64,
		name = "__slot" .. tostring(off), isSlot = true }
end

local function immVal(op)
	if op and op.kind == "imm" then return op.value or tonumber(op.text) or 0 end
	return 0
end

-- Rewrite `body` using the arch `policy`. `regions` are fixed-size alloca regions (their
-- stack math is redirected to the allocation by the transform, so it passes through here).
function frame.preprocess(body, regions, policy)
	local frameSize, fpOffset, usesFp = policy.analyzeFrame(body)
	local out = {}
	local function emit(n) out[#out + 1] = n end

	local function regionOf(off)
		for _, r in (regions or {}) do
			if off >= r.base and off < r.base + r.size then return r end
		end
		return nil
	end

	local function checkBounds(off, width, isWrite, raw)
		if off < 0 then
			error("sarcasm: stack access below frame (sp offset " .. off .. "): " .. (raw or "?"))
		end
		if off + width > frameSize then
			if isWrite then
				error("sarcasm: store into the caller's argument area / outside the frame "
					.. "(sp offset " .. off .. ", frame size " .. frameSize .. "): " .. (raw or "?"))
			end
			error("sarcasm: stack access outside the frame (sp offset " .. off
				.. ", frame size " .. frameSize .. "): " .. (raw or "?"))
		end
	end

	local function rejectEscape(raw)
		error("sarcasm: taking address of stack frame is not supported (cannot prove safety): "
			.. (raw or "?"))
	end

	local ctx = {
		frameSize = frameSize, fpOffset = fpOffset, usesFp = usesFp,
		slotReg = slotReg, immVal = immVal,
		regionOf = regionOf, checkBounds = checkBounds, rejectEscape = rejectEscape,
		emit = emit,
	}

	for _, st in body do
		if st.kind ~= "insn" then
			emit(st)
		elseif st.annotation and string.find(st.annotation, "alloca", 1, true) then
			-- alloca-annotated instructions are turned into GC allocations by the transform;
			-- pass them through untouched (no setup drop, escape check, or slot conversion).
			emit(st)
		else
			policy.rewrite(st, ctx)
		end
	end
	return out
end

frame.SLOT_BASE = SLOT_BASE
frame.slotReg = slotReg
return frame
