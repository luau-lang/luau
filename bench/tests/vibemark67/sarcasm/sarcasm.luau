--!nonstrict
-- sarcasm: SAfe Runtime Capability-enforced Assembler. Rewrites annotated Yolo-C ARM64
-- assembly into memory-safe, Fil-C-linkable assembly, then (by default) invokes `as`.

-- local fs = require("@lute/fs")
-- local process = require("@lute/process")

-- Shared modules.
local sig = require("./sig")
local transform = require("./transform")
local regalloc = require("./regalloc")
local emit = require("./emit")
local build = require("./build")
local detect = require("./detect")

-- Per-architecture module pairs (arm64_* / x86_64_*).
local arm64_parse = require("./arm64_parse")
local x86_64_parse = require("./x86_64_parse")
local arm64_glue = require("./arm64_glue")
local x86_64_glue = require("./x86_64_glue")
local arm64_codegen = require("./arm64_codegen")
local x86_64_codegen = require("./x86_64_codegen")
local arm64_frame = require("./arm64_frame")
local x86_64_frame = require("./x86_64_frame")
local arm64_render = require("./arm64_render")
local x86_64_render = require("./x86_64_render")

local sarcasm = {}

-- Keys of a table in ascending order. Used wherever iteration order over a set/map would
-- otherwise leak the VM's hash order into emitted assembly (spill reload/store ordering,
-- callsite-thunk ordering). Handles mixed key types defensively.
local function sortedKeys(t)
	local out = {}
	for k in t do out[#out + 1] = k end
	table.sort(out, function(a, b)
		local ta, tb = type(a), type(b)
		if ta ~= tb then return ta < tb end
		return a < b
	end)
	return out
end

-- Per-architecture backend bundle. `spillReload`/`spillStore` build the load/store IR for
-- a spilled temp at frame offset `off`; `emitFip` renders (x86 always emits AT&T output).
local function makeTarget(arch, syntax)
	if arch == "x86_64" then
		local backend = { cg = x86_64_codegen, frame = x86_64_frame }
		return {
			arch = "x86_64", syntax = syntax,
			transformFunction = function(nm, sd, body) return transform.transformFunction(nm, sd, body, backend) end,
			emitFip = function(out, tr, cr) return emit.emitFip(out, tr, cr, x86_64_render, "att") end,
			glue = x86_64_glue,
			allocatable = x86_64_codegen.ALLOCATABLE,
			spillReload = function(f, off)
				local n = build.insn("mov", { build.rt(f), build.memx(build.rp(4), { dispVal = off ~= 0 and off or nil }) }, { defs = {f} })
				n.spillReload = true; n.spillOff = off; n.spillTemp = f; return n
			end,
			spillStore = function(f, off)
				local n = build.insn("mov", { build.memx(build.rp(4), { dispVal = off ~= 0 and off or nil }), build.rt(f) }, { uses = {f} })
				n.spillStore = true; n.spillOff = off; n.spillTemp = f; return n
			end,
		}
	end
	local backend = { cg = arm64_codegen, frame = arm64_frame }
	return {
		arch = "arm64", syntax = nil,
		transformFunction = function(nm, sd, body) return transform.transformFunction(nm, sd, body, backend) end,
		emitFip = function(out, tr, cr) return emit.emitFip(out, tr, cr, arm64_render, nil) end,
		glue = arm64_glue,
		allocatable = arm64_codegen.ALLOCATABLE,
		spillReload = function(f, off)
			local n = build.insn("ldr", { build.rt(f), build.mem(build.sp(), off) }, { defs = {f} })
			n.spillReload = true; n.spillOff = off; n.spillTemp = f; return n
		end,
		spillStore = function(f, off)
			local n = build.insn("str", { build.rt(f), build.mem(build.sp(), off) }, { uses = {f} })
			n.spillStore = true; n.spillOff = off; n.spillTemp = f; return n
		end,
	}
end

-- Split parsed statements into functions. A function starts at a label carrying a
-- `;! signature` annotation and ends at its `.size NAME, .-NAME` directive. Every
-- symbol declared as a function (`.type NAME, %function` / `@function`) that is defined
-- here (has a label) MUST carry a signature annotation; otherwise we cannot make it
-- memory-safe, so it is a compile-time error rather than a silently dropped function.
-- Returns { functions = { {name, sigAnn, body={stmts}} }, consumed }.
local function splitFunctions(stmts)
	-- pass 1: collect declared function symbols
	local funcSyms = {}
	for _, st in stmts do
		if st.kind == "directive" and st.name == ".type" then
			local nm, kind = string.match(st.args, "^%s*([%w_.$]+)%s*,%s*(.+)$")
			if nm and kind and string.find(kind, "function", 1, true) then
				funcSyms[nm] = true
			end
		end
	end

	-- pass 2: split into functions, enforcing that declared functions are annotated
	local functions = {}
	local consumed = {}
	local cur = nil
	for idx, st in stmts do
		if st.kind == "label" then
			local hasSig = st.annotation ~= nil and string.find(st.annotation, "%(") ~= nil
			if hasSig then
				cur = { name = st.name, sigAnn = st.annotation, body = { st }, startIdx = idx }
				table.insert(functions, cur)
				consumed[idx] = cur
			elseif funcSyms[st.name] then
				error("sarcasm: function '" .. st.name .. "' has no ;! signature annotation "
					.. "(add one to its label, e.g. `" .. st.name .. ": ;! int(ptr)`)")
			elseif cur then
				table.insert(cur.body, st)
				consumed[idx] = cur
			end
		elseif cur then
			table.insert(cur.body, st)
			consumed[idx] = cur
			if st.kind == "directive" and st.name == ".size" then
				cur = nil
			end
		end
	end
	return functions, consumed
end
sarcasm.splitFunctions = splitFunctions

-- Build the regalloc instruction array from transform IR nodes.
local function buildRaInsns(nodes)
	local ra = {}
	for _, node in nodes do
		if node.kind == "label" then
			table.insert(ra, { defs = {}, uses = {}, isLabelDef = node.name, fallsThrough = true })
		elseif node.kind == "insn" then
			local control = node.control or (node.c and node.c.control) or "none"
			local targets = node.targets or (node.c and node.c.targets)
			local fallsThrough = node.fallsThrough
			if fallsThrough == nil then fallsThrough = node.c and node.c.fallsThrough end
			if fallsThrough == nil then fallsThrough = true end
			table.insert(ra, {
				defs = node.defTemps or {},
				uses = node.useTemps or {},
				move = node.move,
				targets = targets,
				fallsThrough = fallsThrough,
			})
		elseif node.kind == "epilogue" then
			table.insert(ra, { defs = {}, uses = node.useTemps or {}, fallsThrough = true })
		elseif node.kind == "rootstore" then
			table.insert(ra, { defs = {}, uses = node.useTemps or {}, fallsThrough = true })
		elseif node.kind == "prologue" then
			table.insert(ra, { defs = {}, uses = {}, fallsThrough = true })
		else
			table.insert(ra, { defs = {}, uses = {}, fallsThrough = true })
		end
	end
	return ra
end

-- Redirect every reference to a spilled temp in `node` to its fresh replacement.
local function remapNodeTemps(node, remap)
	local function replList(list)
		if list then for i, t in list do if remap[t] then list[i] = remap[t] end end end
	end
	replList(node.defTemps)
	replList(node.useTemps)
	if node.move then
		if remap[node.move.dst] then node.move.dst = remap[node.move.dst] end
		if remap[node.move.src] then node.move.src = remap[node.move.src] end
	end
	local function remapOp(op)
		if not op then return end
		if op.kind == "reg" then
			if op.tempRef and remap[op.tempRef] then op.tempRef = remap[op.tempRef] end
			if node.opTemp and node.opTemp[op] and remap[node.opTemp[op]] then
				node.opTemp[op] = remap[node.opTemp[op]]
			end
		elseif op.kind == "mem" then
			remapOp(op.base); remapOp(op.index)
		end
	end
	local ops = node.stmt and node.stmt.operands or node.operands
	if ops then for _, op in ops do remapOp(op) end end
end

-- Rewrite spilled temps: around each def/use of a spilled temp, insert a store/reload
-- through a fresh short-lived temp (tagged so the emitter can eliminate redundant
-- reloads and size the frame). Returns the new node list.
local function rewriteSpills(nodes, spilledSet, slotOf, allocTemp, spillBase, target)
	local out = {}
	for _, node in nodes do
		if node.kind ~= "insn" then
			table.insert(out, node)
			continue
		end
		local remap, isUse, isDef = {}, {}, {}
		for _, t in (node.useTemps or {}) do
			if spilledSet[t] then remap[t] = remap[t] or allocTemp(); isUse[t] = true end
		end
		for _, t in (node.defTemps or {}) do
			if spilledSet[t] then remap[t] = remap[t] or allocTemp(); isDef[t] = true end
		end
		if not next(remap) then
			table.insert(out, node)
			continue
		end
		for _, t in sortedKeys(remap) do
			if isUse[t] then
				table.insert(out, target.spillReload(remap[t], spillBase + 8 * slotOf[t]))
			end
		end
		remapNodeTemps(node, remap)
		table.insert(out, node)
		for _, t in sortedKeys(remap) do
			if isDef[t] then
				table.insert(out, target.spillStore(remap[t], spillBase + 8 * slotOf[t]))
			end
		end
	end
	return out
end

-- Pack spill slots by coloring their interference graph: two spilled values whose live
-- ranges never overlap can share one stack offset. Without this, every spilled temp gets a
-- unique slot and frames balloon (e.g. large-arm64 needed ~7.7 KB). We build a
-- slot-variable view of the final code (a spill store defines its slot, a reload uses it),
-- run the same backward liveness the register allocator uses, then greedily color slots
-- (a slot's store interferes with everything live across it). Rewrites each spill node's
-- offset and returns the packed slot count.
local function packSpillSlots(tr, spillBase, numSlots, target)
	if numSlots <= 1 then return numSlots end
	local function slotIdOf(off) return math.floor((off - spillBase) / 8) end

	-- slot-variable insns parallel to fipNodes, preserving the CFG (labels + control flow).
	local slotInsns = {}
	for _, node in tr.fipNodes do
		if node.kind == "label" then
			table.insert(slotInsns, { defs = {}, uses = {}, isLabelDef = node.name, fallsThrough = true })
		elseif node.kind == "insn" then
			local control = node.control or (node.c and node.c.control) or "none"
			local targets = node.targets or (node.c and node.c.targets)
			local ft = node.fallsThrough
			if ft == nil then ft = node.c and node.c.fallsThrough end
			if ft == nil then ft = true end
			local defs, uses = {}, {}
			if node.spillStore then defs = { slotIdOf(node.spillOff) } end
			if node.spillReload then uses = { slotIdOf(node.spillOff) } end
			table.insert(slotInsns, { defs = defs, uses = uses, targets = targets, fallsThrough = ft })
		else
			table.insert(slotInsns, { defs = {}, uses = {}, fallsThrough = true })
		end
	end

	local liveOut = regalloc.liveOut(slotInsns)
	-- interference: a slot's store (its only def point) interferes with every slot live
	-- across that store. This def-to-live-out construction is the standard, complete
	-- interference relation (the register allocator uses the same one): any two slots that
	-- are ever simultaneously live get an edge at whichever's store sees the other live. It
	-- only does work at spill stores, so it's linear in the spill traffic (an all-pairs
	-- build would be quadratic in the live set at every instruction).
	local adj = {}
	for s = 0, numSlots - 1 do adj[s] = {} end
	for i, ins in slotInsns do
		if ins.defs[1] ~= nil then
			local lo = liveOut[i]
			if lo then
				for _, d in ins.defs do
					for t in lo do if t ~= d then adj[d][t] = true; adj[t][d] = true end end
				end
			end
		end
	end
	-- greedy graph coloring (colors = compact slot offsets)
	local color = {}
	local ncolors = 0
	for s = 0, numSlots - 1 do
		local used = {}
		for t in adj[s] do if color[t] ~= nil then used[color[t]] = true end end
		local c = 0
		while used[c] do c += 1 end
		color[s] = c
		if c + 1 > ncolors then ncolors = c + 1 end
	end
	-- Rewrite spill nodes to their packed offset. The offset lives inside the memory
	-- operand (target-specific shape), so rebuild the node via the target constructor rather
	-- than poking a field; that keeps the rendered address and the reload-elim key in sync.
	for i, node in tr.fipNodes do
		if node.kind == "insn" and node.spillReload then
			tr.fipNodes[i] = target.spillReload(node.spillTemp, spillBase + 8 * color[slotIdOf(node.spillOff)])
		elseif node.kind == "insn" and node.spillStore then
			tr.fipNodes[i] = target.spillStore(node.spillTemp, spillBase + 8 * color[slotIdOf(node.spillOff)])
		end
	end
	return ncolors
end

-- Color the function, spilling to the stack and re-running IRC until it converges.
-- Sets tr.numSpillSlots and returns the final coloring.
local function allocateWithSpilling(tr, target)
	local nextTemp = tr.nextTemp or 0
	local function allocTemp()
		nextTemp += 1
		tr.temps[nextTemp] = { spillCost = 1e9 }  -- reload temps are short-lived; never re-spill
		return nextTemp
	end
	local slotOf, numSlots = {}, 0
	local spillBase = 16 + 8 * (tr.numRoots or 0)
	for _ = 1, 32 do
		-- spill-cost heuristic: cost ~ number of def/use occurrences, so IRC prefers
		-- spilling rarely-used temps (a value used once is cheap to reload; a value used
		-- many times, like a base pointer, is kept in a register). noSpill temps keep 1e9.
		local cnt = {}
		for _, node in tr.fipNodes do
			for _, t in (node.defTemps or {}) do cnt[t] = (cnt[t] or 0) + 1 end
			for _, t in (node.useTemps or {}) do cnt[t] = (cnt[t] or 0) + 1 end
		end
		for t, d in tr.temps do
			if (d.spillCost or 0) < 1e9 then d.spillCost = math.max(cnt[t] or 1, 1) end
		end
		local cr = regalloc.color(buildRaInsns(tr.fipNodes), tr.temps, { allocatable = target.allocatable })
		if #cr.spilled == 0 then
			-- allocation converged; pack spill slots (reuse offsets for non-overlapping slots)
			tr.numSpillSlots = packSpillSlots(tr, spillBase, numSlots, target)
			return cr
		end
		local spilledSet = {}
		for _, t in cr.spilled do
			spilledSet[t] = true
			if slotOf[t] == nil then slotOf[t] = numSlots; numSlots += 1 end
		end
		tr.fipNodes = rewriteSpills(tr.fipNodes, spilledSet, slotOf, allocTemp, spillBase, target)
	end
	error("sarcasm: register allocation did not converge (excessive register pressure)")
end

-- Compile one parsed input into output assembly text. `debug` (from -g) makes origins
-- carry source file/line/column (pointing at the input assembly) for runtime diagnostics.
function sarcasm.compile(stmts, fileDirective, debug, target)
	target = target or makeTarget("arm64", nil)
	local glue = target.glue
	local functions = splitFunctions(stmts)
	local out = {}
	if fileDirective then table.insert(out, "\t.file\t\"" .. fileDirective .. "\"") end
	table.insert(out, "\t.text")

	local calledExterns = {}   -- sig-name -> desc, for callsite thunks
	local definedFI = {}       -- FI symbols defined here (skip resolver thunks for these)

	for _, fn in functions do
		local sigDesc = sig.parseSignature(fn.sigAnn)
		-- body = statements after the function label up to (not incl) .size
		local body = {}
		for _, st in fn.body do table.insert(body, st) end
		local tr = target.transformFunction(fn.name, sigDesc, body)

		-- allocate registers (spilling to the stack + re-running IRC as needed)
		local colorResult = allocateWithSpilling(tr, target)

		-- source-location info for -g origins (points at the input assembly)
		local fnDbg = debug and { filename = fileDirective or "?", funcLine = tr.funcLine or 0 } or nil

		-- emit getter, FIP body, generic thunk, FO/origins, access origins, alias
		glue.getter(out, fn.name)
		target.emitFip(out, tr, colorResult)
		glue.genericThunk(out, sigDesc.signature, sigDesc.argWords, sigDesc.retWords, sigDesc.retIsPtr)
		glue.foAndOrigins(out, fn.name, sigDesc.signature, tr.numRoots, fnDbg)
		for _, ao in tr.accessOrigins do
			glue.accessOrigin(out, ao.sym, ao.size, ao.align, fn.name, ao.write,
				debug and { line = ao.line or 0 } or nil)
		end
		glue.alias(out, fn.name, sigDesc.signature)
		definedFI["pizlonatedFI" .. sigDesc.signature .. "_" .. fn.name] = true

		for _, ext in tr.calledExterns do
			calledExterns[ext.sigName] = ext
		end
	end

	-- Emit weak/hidden callsite resolver thunks for called externals (deduped, and only
	-- when we don't define the same FI symbol locally).
	for _, sigName in sortedKeys(calledExterns) do
		if not definedFI[sigName] then
			glue.callsiteThunk(out, calledExterns[sigName].name, calledExterns[sigName].sigDesc)
		end
	end

	table.insert(out, "\t.section\t\".note.GNU-stack\",\"\",@progbits")
	return table.concat(out, "\n") .. "\n"
end

local PROGNAME = "sarcasm"
local VERSION = "0.1.0"

local USAGE = "usage: " .. PROGNAME .. " [-o <file>] [-g] [-S|--no-assemble] [--as <command>] <input.s>"

local HELP = table.concat({
	PROGNAME .. " " .. VERSION .. " — SAfe Runtime Capability-enforced Assembler",
	"",
	"Rewrites annotated Yolo-C ARM64 assembly into memory-safe, Fil-C-linkable",
	"assembly, then assembles it with `as` (a drop-in-ish assembler replacement).",
	"",
	USAGE,
	"",
	"Options:",
	"  -o <file>          Output object file (default: <input>.o).",
	"                     With -S, the assembly output path (default: <input>.yolo.s).",
	"  -g                 Emit source file/line/column debug info in Fil-C origins",
	"                     (points at the input assembly) for better runtime diagnostics.",
	"  -S, --no-assemble  Emit transformed assembly only; do not run the assembler.",
	"  --as <command>     Assembler to invoke in the default mode (default: as).",
	"      --version      Print version information and exit.",
	"  -h, --help         Print this help and exit.",
	"",
	"Input is ARM64 assembly annotated with ;! directives, e.g.:",
	"  hash:  ;! unsigned(ptr)        (function label: Fil-C signature)",
	"  ldr x1, [x0]  ;! load ptr      (pointer load from memory)",
	"  str x1, [x0]  ;! store ptr     (pointer store to memory)",
	"  bl foo        ;! int(ptr, size_t)   (call: callee signature)",
	"",
	"Only ARM64 is supported. Assembly that cannot be proven safe is rejected.",
}, "\n")

-- Report a command-line usage error (clean message, no stack trace); exit code 2.
local function usageError(msg: string): number
	print(PROGNAME .. ": error: " .. msg)
	print("Try '" .. PROGNAME .. " --help' for more information.")
	return 2
end

-- Report a processing diagnostic (clean message, no stack trace); exit code 1.
local function diag(msg: string): number
	print(PROGNAME .. ": " .. msg)
	return 1
end

-- CLI: as-like. Args: [-o OUT] [-S|--no-assemble] [--as CMD] INPUT.s
function sarcasm.main(argv)
	local input, output, noAssemble, asCmd, debug = nil, nil, false, "as", false
	local i = 2
	while i <= #argv do
		local a = argv[i]
		if a == "-h" or a == "--help" then
			print(HELP); return 0
		elseif a == "--version" then
			print(PROGNAME .. " " .. VERSION); return 0
		elseif a == "-o" then
			i += 1; output = argv[i]
			if not output then return usageError("option '-o' requires an argument") end
		elseif a == "-g" then
			debug = true
		elseif a == "-S" or a == "--no-assemble" then
			noAssemble = true
		elseif a == "--as" then
			i += 1; asCmd = argv[i]
			if not asCmd then return usageError("option '--as' requires an argument") end
		elseif string.sub(a, 1, 1) == "-" then
			return usageError("unrecognized option '" .. a .. "'")
		else
			if input then
				return usageError("more than one input file given ('" .. input .. "' and '" .. a .. "')")
			end
			input = a
		end
		i += 1
	end
	if not input then
		print(USAGE)
		print("Try '" .. PROGNAME .. " --help' for more information.")
		return 2
	end
	if not fs.exists(input) then
		return diag(input .. ": no such file")
	end

	local text = fs.read(fs.open(input, "r"))
	local arch, syntax = detect.detect(text)
	local target = makeTarget(arch, syntax)
	local stmts = (arch == "x86_64") and x86_64_parse.parseString(text, syntax) or arm64_parse.parseString(text)
	local fileName = string.match(input, "([^/]+)$")
	local ok, asm = pcall(sarcasm.compile, stmts, fileName, debug, target)
	if not ok then
		local msg = tostring(asm)
		-- Errors we raise deliberately are prefixed "sarcasm:" and are user-facing:
		-- present them cleanly. Anything else is an unexpected internal error, which we
		-- re-raise so the underlying stack trace is preserved for debugging.
		local idx = string.find(msg, "sarcasm:", 1, true)
		if idx then
			return diag(input .. ": " .. string.sub(msg, idx + 8):gsub("^%s+", ""))
		end
		error(msg, 0)
	end

	local function stripExt(p) return (string.gsub(p, "%.[%w]+$", "")) end

	if noAssemble then
		local outFile = output or (stripExt(input) .. ".yolo.s")
		fs.write(fs.open(outFile, "w+"), asm)
		return 0
	end

	-- default: write temp .yolo.s next to the .o then run `as` to produce the object.
	local objOut = output or (stripExt(input) .. ".o")
	local tmpAsm = stripExt(objOut) .. ".yolo.s"
	fs.write(fs.open(tmpAsm, "w+"), asm)
	local res = process.run({ asCmd, tmpAsm, "-o", objOut })
	if not res.ok then
		print(res.stderr)
		return res.exitcode ~= 0 and res.exitcode or 1
	end
	return 0
end

function sarcasm.bench(fileName, input, output)
	local arch, syntax = detect.detect(input)
	local target = makeTarget(arch, syntax)
	local stmts = (arch == "x86_64") and x86_64_parse.parseString(input, syntax) or arm64_parse.parseString(input)
	local ok, asm = pcall(sarcasm.compile, stmts, fileName, true, target)
        if not ok then
                error("Unexpected error running benchmark")
        end
        if asm ~= output then
                error("Unexpected output: " .. asm)
        end
end

return sarcasm
