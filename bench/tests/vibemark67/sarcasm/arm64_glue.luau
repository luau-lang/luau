--!nonstrict
-- Emits the Fil-C calling-convention "glue" for an exported function: the function
-- getter, the FO (function object), the generic (buffer) entrypoint thunk, the
-- per-function origin objects, and the direct-call alias. These templates are
-- validated against the live runtime (see ABI-NOTES.md / hash.yolo.s).

local glue = {}

local FUNCTION_UPPER_FLAGS = "36873221949095936" -- 0x83 << 48

-- Function getter: returns the FO flight pointer (intval in x0, lower in x1).
function glue.getter(out, name: string)
	local fo = "pizlonatedFO_" .. name
	table.insert(out, "\t.text")
	table.insert(out, "\t.globl\tpizlonated_" .. name)
	table.insert(out, "\t.p2align\t2")
	table.insert(out, "\t.type\tpizlonated_" .. name .. ",@function")
	table.insert(out, "pizlonated_" .. name .. ":")
	table.insert(out, "\tadrp\tx0, " .. fo .. "+16")
	table.insert(out, "\tadd\tx0, x0, :lo12:" .. fo .. "+16")
	table.insert(out, "\tmov\tx1, x0")
	table.insert(out, "\tret")
	table.insert(out, ".Lfunc_end_getter_" .. name .. ":")
	table.insert(out, "\t.size\tpizlonated_" .. name .. ", .Lfunc_end_getter_" .. name .. "-pizlonated_" .. name)
	table.insert(out, "")
end

-- Generic (buffer-based) entrypoint thunk pizlonated2ET<sig>. Marshals args from the
-- thread CC buffers into the register CC, calls the fast entrypoint, marshals the
-- return value back. argWords/retWords count 8-byte words. retIsPtr controls whether
-- the return lower is copied (else zeroed).
function glue.genericThunk(out, signature: number, argWords: number, retWords: number, retIsPtr: boolean)
	assert(argWords <= 3, "sarcasm: >3 register args not yet supported")
	local sym = "pizlonated2ET" .. tostring(signature)
	local argBytes = argWords * 8
	local retBytes = retWords * 8
	local lend = ".LBB_2ET" .. signature
	table.insert(out, "\t.text")
	table.insert(out, "\t.weak\t" .. sym)
	table.insert(out, "\t.p2align\t2")
	table.insert(out, "\t.type\t" .. sym .. ",@function")
	table.insert(out, sym .. ":")
	table.insert(out, "\tstp\tx30, x19, [sp, #-16]!")
	table.insert(out, "\tcmp\tx2, #" .. tostring(argBytes))
	table.insert(out, "\tb.lo\t" .. lend .. "_fail")
	-- load args: intval reg = x2 + 2*i, lower reg = x3 + 2*i
	for i = 0, argWords - 1 do
		local ivReg = "x" .. tostring(2 + 2 * i)
		local loReg = "x" .. tostring(3 + 2 * i)
		table.insert(out, "\tldr\t" .. ivReg .. ", [x0, #" .. tostring(128 + 8 * i) .. "]")
		table.insert(out, "\tldr\t" .. loReg .. ", [x0, #" .. tostring(384 + 8 * i) .. "]")
	end
	table.insert(out, "\tmov\tx19, x0")
	table.insert(out, "\tldr\tx8, [x1]")
	table.insert(out, "\tblr\tx8")
	table.insert(out, "\ttbnz\tw0, #0, " .. lend .. "_exc")
	if retWords >= 1 then
		table.insert(out, "\tstr\tx1, [x19, #128]")
	end
	table.insert(out, "\tmov\tw1, #" .. tostring(retBytes))
	if retWords >= 1 then
		if retIsPtr then
			table.insert(out, "\tstr\tx2, [x19, #384]")
		else
			table.insert(out, "\tstr\txzr, [x19, #384]")
		end
	end
	table.insert(out, lend .. "_ret:")
	table.insert(out, "\tand\tw0, w0, #0x1")
	table.insert(out, "\tldp\tx30, x19, [sp], #16")
	table.insert(out, "\tret")
	table.insert(out, lend .. "_exc:")
	table.insert(out, "\tb\t" .. lend .. "_ret")
	table.insert(out, lend .. "_fail:")
	table.insert(out, "\tmov\tx0, x2")
	table.insert(out, "\tmov\tw1, #" .. tostring(argBytes))
	table.insert(out, "\tmov\tx2, xzr")
	table.insert(out, "\tbl\tfilc_cc_args_check_failure")
	table.insert(out, "\t.size\t" .. sym .. ", .-" .. sym)
	table.insert(out, "")
end

-- The FO object plus per-function origin objects.
-- numRoots = number of GC pointer-root slots in the frame (matches origin count field).
-- `dbg`, when present, is { filename = <string>, funcLine = <number> } and makes the
-- function origin reference a filename string and carry the function's source line.
function glue.foAndOrigins(out, name: string, signature: number, numRoots: number, dbg)
	local fo = "pizlonatedFO_" .. name
	local strSym = ".Lfilc_string_" .. name
	local fileSym = ".Lfilc_filename_" .. name
	local fnOrigin = ".Lfilc_function_origin_" .. name
	local origin = ".Lfilc_origin_" .. name

	table.insert(out, "\t.type\t" .. fo .. ",@object")
	table.insert(out, "\t.section\t.data.rel.ro,\"aw\",@progbits")
	table.insert(out, "\t.p2align\t4, 0x0")
	table.insert(out, fo .. ":")
	table.insert(out, "\t.xword\t" .. fo .. "+16")
	table.insert(out, "\t.xword\t(" .. fo .. "+16)+" .. FUNCTION_UPPER_FLAGS)
	table.insert(out, "\t.xword\tpizlonatedFIP" .. tostring(signature) .. "_" .. name)
	table.insert(out, "\t.xword\tpizlonated2ET" .. tostring(signature))
	table.insert(out, "\t.xword\t" .. tostring(signature))
	table.insert(out, "\t.size\t" .. fo .. ", 40")
	table.insert(out, "")

	table.insert(out, "\t.type\t" .. strSym .. ",@object")
	table.insert(out, "\t.section\t.rodata.str1.4,\"aMS\",@progbits,1")
	table.insert(out, "\t.p2align\t2, 0x0")
	table.insert(out, strSym .. ":")
	table.insert(out, "\t.asciz\t\"" .. name .. "\"")
	table.insert(out, "\t.size\t" .. strSym .. ", " .. tostring(#name + 1))
	table.insert(out, "")

	-- optional filename string (only with -g)
	if dbg then
		table.insert(out, "\t.type\t" .. fileSym .. ",@object")
		table.insert(out, "\t.section\t.rodata.str1.4,\"aMS\",@progbits,1")
		table.insert(out, "\t.p2align\t2, 0x0")
		table.insert(out, fileSym .. ":")
		table.insert(out, "\t.asciz\t\"" .. dbg.filename .. "\"")
		table.insert(out, "\t.size\t" .. fileSym .. ", " .. tostring(#dbg.filename + 1))
		table.insert(out, "")
	end

	table.insert(out, "\t.type\t" .. fnOrigin .. ",@object")
	table.insert(out, "\t.section\t.data.rel.ro,\"aw\",@progbits")
	table.insert(out, "\t.p2align\t4, 0x0")
	table.insert(out, fnOrigin .. ":")
	table.insert(out, "\t.xword\t" .. strSym)
	table.insert(out, "\t.xword\t" .. (dbg and fileSym or "0"))
	table.insert(out, "\t.word\t" .. tostring(numRoots))
	table.insert(out, "\t.zero\t4")
	table.insert(out, "\t.xword\t0")
	table.insert(out, "\t.byte\t0")
	table.insert(out, "\t.byte\t0")
	table.insert(out, "\t.byte\t0")
	table.insert(out, "\t.zero\t1")
	table.insert(out, "\t.word\t0")
	table.insert(out, "\t.size\t" .. fnOrigin .. ", 40")
	table.insert(out, "")

	table.insert(out, "\t.type\t" .. origin .. ",@object")
	table.insert(out, "\t.p2align\t3, 0x0")
	table.insert(out, origin .. ":")
	table.insert(out, "\t.xword\t" .. fnOrigin)
	table.insert(out, "\t.word\t" .. tostring(dbg and dbg.funcLine or 0))
	table.insert(out, "\t.word\t0")
	table.insert(out, "\t.size\t" .. origin .. ", 16")
	table.insert(out, "")
end

-- An optimized-access-check origin (one per distinct access site).
-- size = access size in bytes, align = required alignment, isWrite marks a store.
-- `dbg`, when present, is { line = <number> } and gives this access its own source-line
-- origin (rather than sharing the function origin).
function glue.accessOrigin(out, sym: string, size: number, align: number, funcName: string, isWrite: boolean?, dbg)
	local origin = ".Lfilc_origin_" .. funcName
	if dbg then
		-- per-access origin referencing the function origin, with this access's line
		origin = sym .. "_o"
		table.insert(out, "\t.type\t" .. origin .. ",@object")
		table.insert(out, "\t.section\t.data.rel.ro,\"aw\",@progbits")
		table.insert(out, "\t.p2align\t3, 0x0")
		table.insert(out, origin .. ":")
		table.insert(out, "\t.xword\t.Lfilc_function_origin_" .. funcName)
		table.insert(out, "\t.word\t" .. tostring(dbg.line or 0))
		table.insert(out, "\t.word\t0")
		table.insert(out, "\t.size\t" .. origin .. ", 16")
		table.insert(out, "")
	end
	table.insert(out, "\t.type\t" .. sym .. ",@object")
	table.insert(out, "\t.section\t.data.rel.ro,\"aw\",@progbits")
	table.insert(out, "\t.p2align\t4, 0x0")
	table.insert(out, sym .. ":")
	table.insert(out, "\t.word\t" .. tostring(size))
	table.insert(out, "\t.byte\t" .. tostring(align))
	table.insert(out, "\t.byte\t0")
	table.insert(out, "\t.byte\t" .. (isWrite and "1" or "0"))
	table.insert(out, "\t.zero\t1")
	table.insert(out, "\t.xword\t" .. origin)
	table.insert(out, "\t.xword\t" .. origin)
	table.insert(out, "\t.xword\t0")
	table.insert(out, "\t.size\t" .. sym .. ", 32")
	table.insert(out, "")
end

-- The direct-call alias pizlonatedFI<sig>_<name> = pizlonatedFIP<sig>_<name>.
function glue.alias(out, name: string, signature: number)
	table.insert(out, "\t.globl\tpizlonatedFI" .. tostring(signature) .. "_" .. name)
	table.insert(out, "\t.type\tpizlonatedFI" .. tostring(signature) .. "_" .. name .. ",@function")
	table.insert(out, ".set pizlonatedFI" .. tostring(signature) .. "_" .. name
		.. ", pizlonatedFIP" .. tostring(signature) .. "_" .. name)
end

function glue.originName(funcName: string): string
	return ".Lfilc_origin_" .. funcName
end

-- Emit a weak/hidden callsite resolver thunk pizlonatedFI<sig>_NAME for a called
-- external function. It resolves the callee's function object via the pizlonated_NAME
-- getter, validates that it is a function object whose intval is the canonical
-- entrypoint and whose signature matches, then tail-calls the fast entrypoint. On a
-- signature mismatch it marshals through the generic (buffer) calling convention. On a
-- non-function object it raises filc_check_function_call_fail. (Template: clang's
-- pizlonatedFI<sig>_NAME in test3.c output.)
function glue.callsiteThunk(out, callee: string, sigDesc)
	local sig = sigDesc.signature
	local sym = "pizlonatedFI" .. tostring(sig) .. "_" .. callee
	local L = ".Lcsr_" .. callee .. "_" .. tostring(sig)

	-- register save plan: x19 = myth; then per-arg iv (+ lo if pointer)
	local myth = 19
	local nextReg = 20
	local args = {}  -- k -> { iv = reg, lo = reg|nil, isPtr = bool }
	for k = 1, sigDesc.argWords do
		local isPtr = sigDesc.argTypes[k] == 7
		local ent = { iv = nextReg, isPtr = isPtr }
		nextReg += 1
		if isPtr then ent.lo = nextReg; nextReg += 1 end
		args[k] = ent
	end
	local saved = { myth }
	for k = 1, sigDesc.argWords do
		table.insert(saved, args[k].iv)
		if args[k].lo then table.insert(saved, args[k].lo) end
	end
	local total = math.floor((8 * (1 + #saved) + 15) / 16) * 16
	local argBytes = sigDesc.argWords * 8
	local retBytes = sigDesc.retWords * 8

	local function e(s) table.insert(out, s) end
	e("\t.text")
	e("\t.weak\t" .. sym)
	e("\t.hidden\t" .. sym)
	e("\t.p2align\t2")
	e("\t.type\t" .. sym .. ",@function")
	e(sym .. ":")
	e("\tsub\tsp, sp, #" .. total)
	e("\tstr\tx30, [sp, #0]")
	for i, r in saved do e("\tstr\tx" .. r .. ", [sp, #" .. (8 * i) .. "]") end
	-- save incoming CC registers into callee-saved
	e("\tmov\tx" .. myth .. ", x0")
	for k = 1, sigDesc.argWords do
		e("\tmov\tx" .. args[k].iv .. ", x" .. (2 + 2 * (k - 1)))
		if args[k].lo then e("\tmov\tx" .. args[k].lo .. ", x" .. (3 + 2 * (k - 1))) end
	end
	-- resolve the function object
	e("\tbl\tpizlonated_" .. callee)
	e("\tcbz\tx1, " .. L .. "_fail")
	e("\tldur\tx8, [x1, #-8]")
	e("\tmov\tx10, #36028797018963968")          -- 0x80000000000000
	e("\tand\tx9, x8, #0x780000000000000")
	e("\tcmp\tx9, x10")
	e("\tb.ne\t" .. L .. "_fail")
	e("\tand\tx8, x8, #0xffffffffffff")
	e("\tcmp\tx0, x8")
	e("\tb.ne\t" .. L .. "_fail")
	e("\tldr\tx8, [x1, #16]")                      -- signature
	e("\tcmp\tx8, #" .. tostring(sig))
	e("\tb.ne\t" .. L .. "_generic")
	-- direct fast call
	e("\tmov\tx0, x" .. myth)
	for k = 1, sigDesc.argWords do
		e("\tmov\tx" .. (2 + 2 * (k - 1)) .. ", x" .. args[k].iv)
		if args[k].lo then e("\tmov\tx" .. (3 + 2 * (k - 1)) .. ", x" .. args[k].lo) end
	end
	for i, r in saved do e("\tldr\tx" .. r .. ", [sp, #" .. (8 * i) .. "]") end
	e("\tldr\tx5, [x1]")
	e("\tldr\tx30, [sp, #0]")
	e("\tadd\tsp, sp, #" .. total)
	e("\tbr\tx5")
	-- generic buffer call
	e(L .. "_generic:")
	for k = 1, sigDesc.argWords do
		e("\tstr\tx" .. args[k].iv .. ", [x" .. myth .. ", #" .. (128 + 8 * (k - 1)) .. "]")
		if args[k].lo then
			e("\tstr\tx" .. args[k].lo .. ", [x" .. myth .. ", #" .. (384 + 8 * (k - 1)) .. "]")
		else
			e("\tstr\txzr, [x" .. myth .. ", #" .. (384 + 8 * (k - 1)) .. "]")
		end
	end
	e("\tmov\tx0, x" .. myth)
	e("\tmov\tw2, #" .. argBytes)
	e("\tldr\tx8, [x1, #8]")
	e("\tblr\tx8")
	e("\ttbnz\tw0, #0, " .. L .. "_exc")
	if sigDesc.retWords >= 1 then
		e("\tcmp\tx1, #" .. tostring(retBytes))
		e("\tb.lo\t" .. L .. "_retfail")
		e("\tldr\tx1, [x" .. myth .. ", #128]")
		if sigDesc.retIsPtr then e("\tldr\tx2, [x" .. myth .. ", #384]") end
	end
	e(L .. "_done:")
	for i, r in saved do e("\tldr\tx" .. r .. ", [sp, #" .. (8 * i) .. "]") end
	e("\tand\tw0, w0, #0x1")
	e("\tldr\tx30, [sp, #0]")
	e("\tadd\tsp, sp, #" .. total)
	e("\tret")
	e(L .. "_exc:")
	e("\tb\t" .. L .. "_done")
	e(L .. "_fail:")
	e("\tbl\tfilc_check_function_call_fail")
	if sigDesc.retWords >= 1 then
		e(L .. "_retfail:")
		e("\tmov\tx0, x1")
		e("\tmov\tw1, #" .. retBytes)
		e("\tmov\tx2, xzr")
		e("\tbl\tfilc_cc_rets_check_failure")
	end
	e("\t.size\t" .. sym .. ", .-" .. sym)
	e("")
end

return glue
