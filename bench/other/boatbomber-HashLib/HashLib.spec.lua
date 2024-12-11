local function describe(phrase, callback) end
local function it(phrase, callback) end
local function expect(value) end

return function()
	local HashLib = require(script.Parent)
	local sha256 = HashLib.sha256

	describe("HashLib.sha256", function()
		it("should properly encode strings", function()
			expect(sha256("abc").to.equal("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"))
			expect(
				sha256("The quick brown fox jumps over the lazy dog").to.equal(
					"d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592"
				)
			)
			expect(sha256("123456").to.equal("8d969eef6ecad3c29a3a629280e686cf0c3f5d5a86aff3ca12020c923adc6c92"))
		end)

		it("should create a private closure that works", function()
			local AppendNextChunk = sha256()
			AppendNextChunk("The quick brown fox")
			AppendNextChunk(" jumps ")
			AppendNextChunk("") -- chunk may be an empty string
			AppendNextChunk("over the lazy dog")
			expect(AppendNextChunk()).to.equal("d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592")
		end)

		it("should allow the private closure to work if called twice", function()
			local AppendNextChunk = sha256()
			AppendNextChunk("The quick brown fox")
			AppendNextChunk(" jumps ")
			AppendNextChunk("") -- chunk may be an empty string
			AppendNextChunk("over the lazy dog")
			AppendNextChunk()
			expect(AppendNextChunk()).to.equal("d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592")
		end)
	end)
end
