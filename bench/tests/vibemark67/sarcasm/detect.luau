--!nonstrict
-- Auto-detect the target architecture (arm64 vs x86_64) and, for x86_64, the assembly
-- syntax (att vs intel) from the input assembly text.

local detect = {}

-- Returns arch ("arm64"|"x86_64"), syntax ("att"|"intel"|nil).
function detect.detect(text)
	-- Strong, unambiguous markers first.
	if string.find(text, "%.intel_syntax") then return "x86_64", "intel" end
	if string.find(text, "%.att_syntax") then return "x86_64", "att" end
	if string.find(text, "%.arch%s+armv8") then return "arm64", nil end

	-- AT&T uses % register prefixes; x86 also has endbr64 / RIP-relative.
	if string.find(text, "%%r[a-z0-9]") or string.find(text, "%%e[a-z][a-z]")
		or string.find(text, "%%rip") or string.find(text, "endbr64", 1, true) then
		return "x86_64", "att"
	end

	-- Intel without a .intel_syntax directive: bare 64-bit x86 registers or PTR/[] memory.
	if string.find(text, " PTR ") or string.find(text, "%[") then
		-- brackets alone aren't proof (ARM uses [x0]); require an x86 register token too.
		if string.find(text, "[ \t,%[]r[abcds][xip][ \t,%]%+%-*]")
			or string.find(text, "[ \t,%[]r%d%d?[ \t,%]%+%-*]")
			or string.find(text, " PTR ") then
			return "x86_64", "intel"
		end
	end
	if string.find(text, "[ \t,]r[abcds][xip][ \t,\n]") or string.find(text, "[ \t,]e[abcds][xip][ \t,\n]") then
		return "x86_64", "intel"
	end

	-- Default: ARM64 (x0..x30 / w0.. / sp / xzr).
	return "arm64", nil
end

return detect
