--!nonstrict
-- x86_64 Fil-C calling-convention glue (AT&T output): function getter, generic (buffer)
-- entrypoint thunk, FO object + origins, direct-call alias, and callsite resolver thunk.
-- Templates validated against the reference Fil-C clang x86_64 output (hash-filc-x86.s).
-- Data layout is identical to ARM64 (only .xword->.quad, .word->.long).

local glue_x86 = {}

local FUNCTION_UPPER_FLAGS = "36873221949095936" -- 0x83 << 48

function glue_x86.getter(out, name)
	local fo = "pizlonatedFO_" .. name
	table.insert(out, "\t.text")
	table.insert(out, "\t.globl\tpizlonated_" .. name)
	table.insert(out, "\t.p2align\t4")
	table.insert(out, "\t.type\tpizlonated_" .. name .. ",@function")
	table.insert(out, "pizlonated_" .. name .. ":")
	table.insert(out, "\tleaq\t" .. fo .. "+16(%rip), %rax")
	table.insert(out, "\tmovq\t%rax, %rdx")
	table.insert(out, "\tretq")
	table.insert(out, ".Lfunc_end_getter_" .. name .. ":")
	table.insert(out, "\t.size\tpizlonated_" .. name .. ", .Lfunc_end_getter_" .. name .. "-pizlonated_" .. name)
	table.insert(out, "")
end

-- generic (buffer) entrypoint pizlonated2ET<sig>.
function glue_x86.genericThunk(out, signature, argWords, retWords, retIsPtr)
	assert(argWords <= 3, "sarcasm: >3 register args not yet supported")
	local sym = "pizlonated2ET" .. tostring(signature)
	local argBytes = argWords * 8
	local retBytes = retWords * 8
	local L = ".LBB_2ET" .. signature
	local function e(s) table.insert(out, s) end
	-- CC iv/lo register for arg k (1-based): iv=rdx,r8,(r10?); lo=rcx,r9. Support up to 3.
	local ivReg = { "%rdx", "%r8", "%r10" }
	local loReg = { "%rcx", "%r9", "%r11" }
	e("\t.text")
	e("\t.weak\t" .. sym)
	e("\t.p2align\t4")
	e("\t.type\t" .. sym .. ",@function")
	e(sym .. ":")
	e("\tpushq\t%rbx")
	e("\tcmpq\t$" .. tostring(argBytes - 1) .. ", %rdx")
	e("\tjbe\t" .. L .. "_fail")
	e("\tmovq\t%rdi, %rbx")
	for i = 0, argWords - 1 do
		e("\tmovq\t" .. tostring(128 + 8 * i) .. "(%rdi), " .. ivReg[i + 1])
		e("\tmovq\t" .. tostring(384 + 8 * i) .. "(%rdi), " .. loReg[i + 1])
	end
	e("\tcallq\t*(%rsi)")
	e("\ttestb\t$1, %al")
	e("\tjne\t" .. L .. "_exc")
	if retWords >= 1 then
		e("\tmovq\t%rdx, 128(%rbx)")
		if retIsPtr then e("\tmovq\t%rcx, 384(%rbx)") else e("\tmovq\t$0, 384(%rbx)") end
	end
	e("\tmovl\t$" .. tostring(retBytes) .. ", %edx")
	e("\tpopq\t%rbx")
	e("\tretq")
	e(L .. "_exc:")
	e("\tpopq\t%rbx")
	e("\tretq")
	e(L .. "_fail:")
	e("\tmovl\t$" .. tostring(argBytes) .. ", %esi")
	e("\tmovq\t%rdx, %rdi")
	e("\txorl\t%edx, %edx")
	e("\tcallq\tfilc_cc_args_check_failure@PLT")
	e("\t.size\t" .. sym .. ", .-" .. sym)
	e("")
end

function glue_x86.foAndOrigins(out, name, signature, numRoots, dbg)
	local fo = "pizlonatedFO_" .. name
	local strSym = ".Lfilc_string_" .. name
	local fileSym = ".Lfilc_filename_" .. name
	local fnOrigin = ".Lfilc_function_origin_" .. name
	local origin = ".Lfilc_origin_" .. name

	table.insert(out, "\t.type\t" .. fo .. ",@object")
	table.insert(out, "\t.section\t.data.rel.ro,\"aw\",@progbits")
	table.insert(out, "\t.p2align\t4, 0x0")
	table.insert(out, fo .. ":")
	table.insert(out, "\t.quad\t" .. fo .. "+16")
	table.insert(out, "\t.quad\t(" .. fo .. "+16)+" .. FUNCTION_UPPER_FLAGS)
	table.insert(out, "\t.quad\tpizlonatedFIP" .. tostring(signature) .. "_" .. name)
	table.insert(out, "\t.quad\tpizlonated2ET" .. tostring(signature))
	table.insert(out, "\t.quad\t" .. tostring(signature))
	table.insert(out, "\t.size\t" .. fo .. ", 40")
	table.insert(out, "")

	table.insert(out, "\t.type\t" .. strSym .. ",@object")
	table.insert(out, "\t.section\t.rodata.str1.1,\"aMS\",@progbits,1")
	table.insert(out, strSym .. ":")
	table.insert(out, "\t.asciz\t\"" .. name .. "\"")
	table.insert(out, "\t.size\t" .. strSym .. ", " .. tostring(#name + 1))
	table.insert(out, "")

	if dbg then
		table.insert(out, "\t.type\t" .. fileSym .. ",@object")
		table.insert(out, "\t.section\t.rodata.str1.1,\"aMS\",@progbits,1")
		table.insert(out, fileSym .. ":")
		table.insert(out, "\t.asciz\t\"" .. dbg.filename .. "\"")
		table.insert(out, "\t.size\t" .. fileSym .. ", " .. tostring(#dbg.filename + 1))
		table.insert(out, "")
	end

	table.insert(out, "\t.type\t" .. fnOrigin .. ",@object")
	table.insert(out, "\t.section\t.data.rel.ro,\"aw\",@progbits")
	table.insert(out, "\t.p2align\t4, 0x0")
	table.insert(out, fnOrigin .. ":")
	table.insert(out, "\t.quad\t" .. strSym)
	table.insert(out, "\t.quad\t" .. (dbg and fileSym or "0"))
	table.insert(out, "\t.long\t" .. tostring(numRoots))
	table.insert(out, "\t.zero\t4")
	table.insert(out, "\t.quad\t0")
	table.insert(out, "\t.byte\t0")
	table.insert(out, "\t.byte\t0")
	table.insert(out, "\t.byte\t0")
	table.insert(out, "\t.zero\t1")
	table.insert(out, "\t.long\t0")
	table.insert(out, "\t.size\t" .. fnOrigin .. ", 40")
	table.insert(out, "")

	table.insert(out, "\t.type\t" .. origin .. ",@object")
	table.insert(out, "\t.p2align\t3, 0x0")
	table.insert(out, origin .. ":")
	table.insert(out, "\t.quad\t" .. fnOrigin)
	table.insert(out, "\t.long\t" .. tostring(dbg and dbg.funcLine or 0))
	table.insert(out, "\t.long\t0")
	table.insert(out, "\t.size\t" .. origin .. ", 16")
	table.insert(out, "")
end

function glue_x86.accessOrigin(out, sym, size, align, funcName, isWrite, dbg)
	local origin = ".Lfilc_origin_" .. funcName
	if dbg then
		origin = sym .. "_o"
		table.insert(out, "\t.type\t" .. origin .. ",@object")
		table.insert(out, "\t.section\t.data.rel.ro,\"aw\",@progbits")
		table.insert(out, "\t.p2align\t3, 0x0")
		table.insert(out, origin .. ":")
		table.insert(out, "\t.quad\t.Lfilc_function_origin_" .. funcName)
		table.insert(out, "\t.long\t" .. tostring(dbg.line or 0))
		table.insert(out, "\t.long\t0")
		table.insert(out, "\t.size\t" .. origin .. ", 16")
		table.insert(out, "")
	end
	table.insert(out, "\t.type\t" .. sym .. ",@object")
	table.insert(out, "\t.section\t.data.rel.ro,\"aw\",@progbits")
	table.insert(out, "\t.p2align\t4, 0x0")
	table.insert(out, sym .. ":")
	table.insert(out, "\t.long\t" .. tostring(size))
	table.insert(out, "\t.byte\t" .. tostring(align))
	table.insert(out, "\t.byte\t0")
	table.insert(out, "\t.byte\t" .. (isWrite and "1" or "0"))
	table.insert(out, "\t.zero\t1")
	table.insert(out, "\t.quad\t" .. origin)
	table.insert(out, "\t.quad\t" .. origin)
	table.insert(out, "\t.quad\t0")
	table.insert(out, "\t.size\t" .. sym .. ", 32")
	table.insert(out, "")
end

function glue_x86.alias(out, name, signature)
	table.insert(out, "\t.globl\tpizlonatedFI" .. tostring(signature) .. "_" .. name)
	table.insert(out, "\t.type\tpizlonatedFI" .. tostring(signature) .. "_" .. name .. ",@function")
	table.insert(out, ".set pizlonatedFI" .. tostring(signature) .. "_" .. name
		.. ", pizlonatedFIP" .. tostring(signature) .. "_" .. name)
end

function glue_x86.originName(funcName) return ".Lfilc_origin_" .. funcName end

-- Callsite resolver thunk pizlonatedFI<sig>_NAME (weak/hidden), validated against the
-- reference Fil-C clang x86_64 output (test3.c -> pizlonatedFI2529_foo). Resolves the
-- callee's FO via pizlonated_NAME, validates it is a function object with the canonical
-- intval and matching signature, then tail-calls the fast entrypoint; on a signature
-- mismatch it marshals through the generic (buffer) CC.
function glue_x86.callsiteThunk(out, callee, sigDesc)
	local sig = sigDesc.signature
	local sym = "pizlonatedFI" .. tostring(sig) .. "_" .. callee
	local L = ".Lcsr_" .. callee .. "_" .. tostring(sig)
	local ivReg = { "%rdx", "%r8", "%r10" }   -- incoming fast-call arg iv registers
	local loReg = { "%rcx", "%r9", "%r11" }   -- incoming fast-call arg lo registers
	local myth = "%rbx"
	local pool = { "%r12", "%r13", "%r14", "%r15" }
	local nextHold = 1
	local args = {}
	for k = 1, sigDesc.argWords do
		local isPtr = sigDesc.argTypes[k] == 7
		local ent = { iv = pool[nextHold], isPtr = isPtr }; nextHold += 1
		if isPtr then ent.lo = pool[nextHold]; nextHold += 1 end
		args[k] = ent
	end
	local argBytes = sigDesc.argWords * 8
	local retBytes = sigDesc.retWords * 8
	local function e(s) table.insert(out, s) end

	-- push list (must leave rsp 16-byte aligned at the call: retaddr + N pushes, N odd).
	local pushList = { myth }
	for k = 1, sigDesc.argWords do
		table.insert(pushList, args[k].iv)
		if args[k].lo then table.insert(pushList, args[k].lo) end
	end
	local filler = (#pushList % 2) == 0
	local function restore()
		if filler then e("\taddq\t$8, %rsp") end
		for i = #pushList, 1, -1 do e("\tpopq\t" .. pushList[i]) end
	end

	e("\t.text")
	e("\t.weak\t" .. sym)
	e("\t.hidden\t" .. sym)
	e("\t.p2align\t4")
	e("\t.type\t" .. sym .. ",@function")
	e(sym .. ":")
	for _, r in pushList do e("\tpushq\t" .. r) end
	if filler then e("\tpushq\t%rax") end
	-- stash incoming CC (myth + args) into callee-saved holds
	e("\tmovq\t%rdi, " .. myth)
	for k = 1, sigDesc.argWords do
		e("\tmovq\t" .. ivReg[k] .. ", " .. args[k].iv)
		if args[k].lo then e("\tmovq\t" .. loReg[k] .. ", " .. args[k].lo) end
	end
	-- resolve FO
	e("\txorl\t%esi, %esi")
	e("\tcallq\tpizlonated_" .. callee .. "@PLT")   -- rax = iv, rdx = lower(FO)
	e("\tmovq\t%rdx, %rsi")                          -- rsi = FO payload
	e("\ttestq\t%rdx, %rdx")
	e("\tje\t" .. L .. "_fail")
	e("\tmovq\t-8(%rsi), %rcx")                       -- aux word (flags)
	e("\tmovabsq\t$0x780000000000000, %rdx")
	e("\tandq\t%rcx, %rdx")
	e("\tmovabsq\t$0x80000000000000, %rdi")
	e("\tcmpq\t%rdi, %rdx")
	e("\tjne\t" .. L .. "_fail")
	e("\tmovabsq\t$0xffffffffffff, %rdx")
	e("\tandq\t%rdx, %rcx")
	e("\tcmpq\t%rcx, %rax")                           -- intval must equal canonical entrypoint
	e("\tjne\t" .. L .. "_fail")
	e("\tcmpq\t$" .. tostring(sig) .. ", 16(%rsi)")   -- signature
	e("\tjne\t" .. L .. "_generic")
	-- direct fast call
	e("\tmovq\t(%rsi), %rax")                         -- fast entrypoint
	e("\tmovq\t" .. myth .. ", %rdi")
	for k = 1, sigDesc.argWords do
		e("\tmovq\t" .. args[k].iv .. ", " .. ivReg[k])
		if args[k].lo then e("\tmovq\t" .. args[k].lo .. ", " .. loReg[k]) end
	end
	restore()
	e("\tjmpq\t*%rax")
	-- generic (buffer) call
	e(L .. "_generic:")
	for k = 1, sigDesc.argWords do
		e("\tmovq\t" .. args[k].iv .. ", " .. tostring(128 + 8 * (k - 1)) .. "(" .. myth .. ")")
		if args[k].lo then
			e("\tmovq\t" .. args[k].lo .. ", " .. tostring(384 + 8 * (k - 1)) .. "(" .. myth .. ")")
		else
			e("\tmovq\t$0, " .. tostring(384 + 8 * (k - 1)) .. "(" .. myth .. ")")
		end
	end
	e("\tmovl\t$" .. tostring(argBytes) .. ", %edx")
	e("\tmovq\t" .. myth .. ", %rdi")
	e("\tcallq\t*8(%rsi)")                            -- generic entrypoint
	e("\ttestb\t$1, %al")
	e("\tjne\t" .. L .. "_exc")
	if sigDesc.retWords >= 1 then
		e("\tcmpq\t$" .. tostring(retBytes - 1) .. ", %rdx")
		e("\tjbe\t" .. L .. "_retfail")
		e("\tmovq\t128(" .. myth .. "), %rdx")
		if sigDesc.retIsPtr then e("\tmovq\t384(" .. myth .. "), %rcx") end
	end
	e(L .. "_done:")
	restore()
	e("\tretq")
	e(L .. "_exc:")
	e("\tjmp\t" .. L .. "_done")
	e(L .. "_fail:")
	e("\tmovq\t%rax, %rdi")
	e("\tcallq\tfilc_check_function_call_fail@PLT")
	if sigDesc.retWords >= 1 then
		e(L .. "_retfail:")
		e("\tmovl\t$" .. tostring(retBytes) .. ", %esi")
		e("\tmovq\t%rdx, %rdi")
		e("\txorl\t%edx, %edx")
		e("\tcallq\tfilc_cc_rets_check_failure@PLT")
	end
	e("\t.size\t" .. sym .. ", .-" .. sym)
	e("")
end

return glue_x86
