--!nonstrict
-- Constructors for synthesized IR: operands that reference temps or fixed physical
-- registers, and instruction nodes carrying explicit def/use temp lists. These nodes
-- render through emit.renderInsn (which understands op.tempRef / op.fixedNum) and feed
-- regalloc (via node.defTemps / node.useTemps / node.move / node.control).

local build = {}

-- Operand: a temp register of the given width (64 unless noted).
function build.rt(tempId, width)
	return { kind = "reg", tempRef = tempId, width = width or 64, rc = "gpr" }
end

-- Operand: a fixed physical register by number (e.g. sp = -2, xzr = -1).
function build.rp(num, width)
	return { kind = "reg", fixedNum = num, width = width or 64, rc = "gpr" }
end

function build.sp() return { kind = "reg", fixedNum = -2, width = 64, rc = "sp", name = "sp" } end
function build.xzr() return { kind = "reg", fixedNum = -1, width = 64, rc = "zr", name = "xzr" } end
function build.wzr() return { kind = "reg", fixedNum = -1, width = 32, rc = "zr", name = "wzr" } end

function build.imm(n) return { kind = "imm", text = tostring(n) } end
function build.sym(name, reloc) return { kind = "sym", text = name, reloc = reloc } end

-- Memory operand builder. base is an operand; offset is a number (immediate) or nil.
function build.mem(base, offset, opts)
	opts = opts or {}
	local m = { kind = "mem", base = base, writeback = opts.writeback }
	if offset ~= nil then
		if type(offset) == "number" then
			m.index = { kind = "imm", text = tostring(offset), value = offset }
		else
			m.index = offset
		end
	end
	if opts.shift then m.shift = opts.shift end
	if opts.postIndex ~= nil then
		m.writeback = "post"
		m.index = { kind = "imm", text = tostring(opts.postIndex), value = opts.postIndex }
	end
	return m
end

-- x86 memory operand: base + index*scale + disp.  `opts` = { index=<op>, scale=1/2/4/8,
-- dispVal=<num>, dispSym=<str>, dispReloc=<"plt"/"gotpcrel"/"rip">, rip=<bool> }.
function build.memx(base, opts)
	opts = opts or {}
	return { kind = "mem", x86 = true, base = base, index = opts.index, scale = opts.scale or 1,
		dispVal = opts.dispVal, dispSym = opts.dispSym, dispReloc = opts.dispReloc }
end

-- Attach a shift/extend to an operand (returns the operand).
function build.withShift(op, shOp, amount)
	op.shift = { op = shOp, amount = amount, amountText = amount ~= nil and tostring(amount) or nil }
	return op
end

-- Instruction node. `attrs` may include defs, uses, move, control, targets, fallsThrough.
function build.insn(mnemonic, operands, attrs)
	attrs = attrs or {}
	return {
		kind = "insn",
		synth = true,
		mnemonic = mnemonic,
		operands = operands or {},
		defTemps = attrs.defs or {},
		useTemps = attrs.uses or {},
		move = attrs.move,
		control = attrs.control or "none",
		targets = attrs.targets,
		fallsThrough = attrs.fallsThrough,
	}
end

-- A raw text line (directive/label emitted verbatim; no temps).
function build.raw(text)
	return { kind = "raw", text = text }
end

function build.label(name)
	return { kind = "label", name = name, synth = true }
end

return build
