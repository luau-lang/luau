-- --bench-args: --fflags=DebugLuauUserDefinedClasses,DebugLuauUserDefinedClassesRuntime
local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../bench_support")

local RANKS = "12345678"
local FILES = "abcdefgh"
local PieceSymbols = "PpRrNnBbQqKk"
local UnicodePieces = {"♙", "♟", "♖", "♜", "♘", "♞", "♗", "♝", "♕", "♛", "♔", "♚"}
local StartingFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

local function popcnt32(i)
	i = i - bit32.band(bit32.rshift(i,1), 0x55555555)
	i = bit32.band(i, 0x33333333) + bit32.band(bit32.rshift(i,2), 0x33333333)
	return bit32.rshift(bit32.band(i + bit32.rshift(i,4), 0x0F0F0F0F) * 0x01010101, 24)
end

--
-- Utils
-- 

local function square(s)
	return RANKS:find(s:sub(2,2)) * 8 + FILES:find(s:sub(1,1)) - 9
end

local function squareName(n)
	local file = n % 8
	local rank = (n-file)/8
	return FILES:sub(file+1,file+1) .. RANKS:sub(rank+1,rank+1)
end

local function moveName(v )  
	local from = bit32.extract(v, 6, 6)
	local to = bit32.extract(v, 0, 6)
	local piece = bit32.extract(v, 20, 4)
	local captured = bit32.extract(v, 25, 4)

	local move = PieceSymbols:sub(piece,piece) .. ' ' .. squareName(from) .. (captured ~= 0 and 'x' or '-') .. squareName(to)
 
	if bit32.extract(v,14) == 1 then
		if to > from then
			return "O-O"
		else
			return "O-O-O"
		end
	end

	local promote = bit32.extract(v,15,4)
	if promote ~= 0 then
		move = move .. "=" .. PieceSymbols:sub(promote,promote)
	end
	return move
end

local function ucimove(m)
	local mm = squareName(bit32.extract(m, 6, 6)) .. squareName(bit32.extract(m, 0, 6))
	local promote = bit32.extract(m,15,4)
	if promote > 0 then
		mm = mm .. PieceSymbols:sub(promote,promote):lower()
	end
	return mm
end

local _utils = {squareName, moveName}

-- @hgoldstein implementation notes
-- * Fairly tedious to rewrite all of the methods: consider adding codemod.
-- * We don't support static data, that is moved to locals which may unintentionally boost perf.
-- * Board used itself as both a hashtable and an array: what kind of benefits do we get from
--   this split? Any?
-- * There's a lot of static data intertwined with functionality.

--
-- Bitboards
--

local BITBOARD_ZERO
local BITBOARD_FULL

local RightMasks
local LeftMasks
local Rank1
local Rank3
local Rank6
local Rank8
local FileA
local FileB
local FileC
local FileD
local FileE
local FileF
local FileG
local FileH

class Bitboard
	public l: number
	public h: number

	function toString(self)
		local out = {}
		local src = self.h
		for x=7,0,-1 do
			table.insert(out, RANKS:sub(x+1,x+1))
			table.insert(out, " ")
			local bit = bit32.lshift(1,(x%4) * 8)
			for x=0,7 do
				if bit32.band(src, bit) ~= 0 then
					table.insert(out, "x ")
				else
					table.insert(out, "- ")
				end
				bit = bit32.lshift(bit, 1)
			end
			if x == 4 then
				src = self.l
			end
			table.insert(out, "\n")
		end
		table.insert(out, '  ' .. FILES:gsub('.', '%1 ') .. '\n')
		table.insert(out, '#: ' .. self:popcnt() .. "\tl:" .. self.l .. "\th:" .. self.h)
		return table.concat(out)
	end

	function from(l, h)
		return Bitboard { l = l, h = h }
	end

	function up(self)
		return self:lshift(8)
	end

	function down(self)
		return self:rshift(8)
	end

	function right(self)
		return self:band(FileH:inverse()):lshift(1)
	end

	function left(self)
		return self:band(FileA:inverse()):rshift(1)
	end

	function move(self, x,y)
		local out = self

		if x < 0 then out = out:bandnot(RightMasks[-x]):lshift(-x) end
		if x > 0 then out = out:bandnot(LeftMasks[x]):rshift(x) end

		if y < 0 then out = out:rshift(-8 * y) end
		if y > 0 then out = out:lshift(8 * y) end
		return out
	end

	function popcnt(self)
		return popcnt32(self.l) + popcnt32(self.h)
	end

	function band(self, other)
		return Bitboard.from(bit32.band(self.l,other.l), bit32.band(self.h, other.h))
	end

	function bandnot(self, other)
		return Bitboard.from(bit32.band(self.l,bit32.bnot(other.l)), bit32.band(self.h, bit32.bnot(other.h)))
	end

	function bandempty(self, other)
		return bit32.band(self.l,other.l) == 0 and bit32.band(self.h, other.h) == 0
	end

	function bor(self, other)
		return Bitboard.from(bit32.bor(self.l,other.l), bit32.bor(self.h, other.h))
	end

	function bxor(self, other)
		return Bitboard.from(bit32.bxor(self.l,other.l), bit32.bxor(self.h, other.h))
	end

	function inverse(self)
		return Bitboard.from(bit32.bxor(self.l,0xFFFFFFFF), bit32.bxor(self.h, 0xFFFFFFFF))
	end

	function empty(self)
		return self.h == 0 and self.l == 0
	end

	function ctz(self)
		local result = bit32.countrz(self.l)
		if result == 32 then
			return bit32.countrz(self.h) + 32
		else
			return result
		end
	end

	function ctzafter(self, start)
		local masked = self:band(BITBOARD_FULL:lshift(start+1))
		return masked:ctz()
	end

	function lshift(self, amt)
		assert(amt >= 0)
		if amt == 0 then return self end

		if amt > 31 then
			return Bitboard.from(0, bit32.lshift(self.l, amt-32))
		end

		local l = bit32.lshift(self.l, amt)
		local h = bit32.bor(
			bit32.lshift(self.h, amt),
			bit32.extract(self.l, 32-amt, amt)
		)
		return Bitboard.from(l, h)
	end

	function rshift(self, amt)
		assert(amt >= 0)
		if amt == 0 then return self end
		local h = bit32.rshift(self.h, amt)
		local l = bit32.bor(
			bit32.rshift(self.l, amt),
			bit32.lshift(bit32.extract(self.h, 0, amt), 32-amt)
		)
		return Bitboard.from(l, h)
	end

	function index(self, i)
		if i > 31 then
			return bit32.extract(self.h, i - 32)
		else
			return bit32.extract(self.l, i)
		end
	end

	function set(self, i, v)
		if i > 31 then
			return Bitboard.from(self.l, bit32.replace(self.h, v, i - 32))
		else
			return Bitboard.from(bit32.replace(self.l, v, i), self.h)
		end
	end

	function isolate(self, i)
		return self:band(Bitboard.some(i))
	end

	function some(idx)
		return BITBOARD_ZERO:set(idx, 1)
	end

end

BITBOARD_ZERO = Bitboard.from(0,0)
BITBOARD_FULL = Bitboard.from(0xFFFFFFFF, 0xFFFFFFFF)

Rank1 = Bitboard.from(0x000000FF, 0)
Rank3 = Bitboard.from(0x00FF0000, 0)
Rank6 = Bitboard.from(0, 0x0000FF00)
Rank8 = Bitboard.from(0, 0xFF000000)
FileA = Bitboard.from(0x01010101, 0x01010101)
FileB = Bitboard.from(0x02020202, 0x02020202)
FileC = Bitboard.from(0x04040404, 0x04040404)
FileD = Bitboard.from(0x08080808, 0x08080808)
FileE = Bitboard.from(0x10101010, 0x10101010)
FileF = Bitboard.from(0x20202020, 0x20202020)
FileG = Bitboard.from(0x40404040, 0x40404040)
FileH = Bitboard.from(0x80808080, 0x80808080)

-- These masks are filled out below for all files
RightMasks = {FileH}
LeftMasks = {FileA}

for i=2,8 do
	RightMasks[i] = RightMasks[i-1]:rshift(1):bor(FileH)
	LeftMasks[i] = LeftMasks[i-1]:lshift(1):bor(FileA)
end

--
-- Board
--

local ROOK_SLIDES = {{1,0}, {-1,0}, {0,1}, {0,-1}}
local BISHOP_SLIDES = {{1,1}, {-1,1}, {1,-1}, {-1,-1}}
local QUEEN_SLIDES = {{1,0}, {-1,0}, {0,1}, {0,-1}, {1,1}, {-1,1}, {1,-1}, {-1,-1}}
local KNIGHT_MOVES = {{2,1}, {2,-1}, {-2,1}, {-2,-1}, {1,2}, {1,-2}, {-1,2}, {-1,-2}}

class Board

	-- Spellcheck?
	public ocupied: Bitboard
	public white: Bitboard
	public black: Bitboard
	public unocupied: Bitboard
	public ep: Bitboard
	public castle: Bitboard
	public toMove: number
	public hm: number
	public moves: number
	public material: number
	public state: { [number]: Bitboard }

	function new()
		return Board {
			ocupied = BITBOARD_ZERO,
			white = BITBOARD_ZERO,
			black = BITBOARD_ZERO,
			unocupied = BITBOARD_FULL,
			ep = BITBOARD_ZERO,
			castle = BITBOARD_ZERO,
			toMove = 1,
			hm = 0,
			moves = 0,
			material = 0,
			state = table.create(12, BITBOARD_ZERO)
		}
	end

	function fromFen(fen)
		local b = Board.new()
		local i = 0
		local rank = 7
		local file = 0

		while true do
			i = i + 1
			local p = fen:sub(i,i)
			if p == '/' then
				rank = rank - 1
				file = 0
			elseif tonumber(p) ~= nil then
				file = file + tonumber(p)
			else
				local pidx = PieceSymbols:find(p)
				if pidx == nil then break end
				b.state[pidx] = b.state[pidx]:set(rank*8+file, 1)
				file = file + 1
			end
		end

		local move, castle, ep, hm, m = string.match(fen, "^ ([bw]) ([KQkq-]*) ([a-h-][0-9]?) (%d*) (%d*)", i)
		if move == nil then print(fen:sub(i)) end
		b.toMove = move == 'w' and 1 or 2

		if ep ~= "-" then
			b.ep = Bitboard.some(square(ep))
		end

		if castle ~= "-" then
			local oo = BITBOARD_ZERO
			if castle:find("K") then
				oo = oo:set(7, 1)
			end
			if castle:find("Q") then
				oo = oo:set(0, 1)
			end
			if castle:find("k") then
				oo = oo:set(63, 1)
			end
			if castle:find("q") then
				oo = oo:set(56, 1)
			end

			b.castle = oo
		end

		b.hm = hm
		b.moves = m

		b:updateCache()
		return b

	end

	function index(self, idx)
		if self.white:index(idx) == 1 then
			for p=1,12,2 do
				if self.state[p]:index(idx) == 1 then
					return p
				end
			end
		else
			for p=2,12,2 do
				if self.state[p]:index(idx) == 1 then
					return p
				end
			end
		end

		return 0
	end

	function updateCache(self)
		for i=1,11,2 do
			self.white = self.white:bor(self.state[i])
			self.black = self.black:bor(self.state[i+1])
		end

		self.ocupied = self.black:bor(self.white)
		self.unocupied = self.ocupied:inverse()
		self.material =
			100*self.state[1]:popcnt() - 100*self.state[2]:popcnt() +
			500*self.state[3]:popcnt() - 500*self.state[4]:popcnt() +
			300*self.state[5]:popcnt() - 300*self.state[6]:popcnt() +
			300*self.state[7]:popcnt() - 300*self.state[8]:popcnt() +
			900*self.state[9]:popcnt() - 900*self.state[10]:popcnt()

	end

	function fen(self)
		local out = {}
		local s = 0
		local idx = 56
		for i=0,63 do
			if i % 8 == 0 and i > 0 then
				idx = idx - 16
				if s > 0 then
					table.insert(out, '' .. s)
					s = 0
				end
				table.insert(out, '/')
			end
			local p = self:index(idx)
			if p == 0 then
				s = s + 1
			else
				if s > 0 then
					table.insert(out, '' .. s)
					s = 0
				end
				table.insert(out, PieceSymbols:sub(p,p))
			end

			idx = idx + 1
		end
		if s > 0 then
			table.insert(out, '' .. s)
		end

		table.insert(out, self.toMove == 1 and ' w ' or ' b ')
		if self.castle:empty() then
			table.insert(out, '-')
		else
			if self.castle:index(7) == 1 then table.insert(out, 'K') end
			if self.castle:index(0) == 1 then table.insert(out, 'Q') end
			if self.castle:index(63) == 1 then table.insert(out, 'k') end
			if self.castle:index(56) == 1 then table.insert(out, 'q') end
		end

		table.insert(out, ' ')
		if self.ep:empty() then
			table.insert(out, '-')
		else
			table.insert(out, squareName(self.ep:ctz()))
		end

		table.insert(out, ' ' .. self.hm)
		table.insert(out, ' ' .. self.moves)

		return table.concat(out)
	end

	function pmoves(self, idx)
		return self:generate(idx)
	end

	function pcaptures(self, idx)
		return self:generate(idx):band(self.ocupied)
	end

	function generate(self, idx)
		local piece = self:index(idx)
		local r = Bitboard.some(idx)
		local out = BITBOARD_ZERO
		local type = bit32.rshift(piece - 1, 1)
		local cancapture = piece % 2 == 1 and self.black or self.white

		if piece == 0 then return BITBOARD_ZERO end

		if type == 0 then
			-- Pawn
			local d = -(piece*2 - 3)
			local movetwo = piece == 1 and Rank3 or Rank6

			out = out:bor(r:move(0,d):band(self.unocupied))
			out = out:bor(out:band(movetwo):move(0,d):band(self.unocupied))

			local captures = r:move(0,d)
			captures = captures:right():bor(captures:left())

			if not captures:bandempty(self.ep) then
				out = out:bor(self.ep)
			end

			captures = captures:band(cancapture)
			out = out:bor(captures)

			return out
		elseif type == 5 then
			-- King
			for x=-1,1,1 do
				for y = -1,1,1 do
					local w = r:move(x,y)
					if self.ocupied:bandempty(w) then
						out = out:bor(w)
					else
						if not cancapture:bandempty(w) then
							out = out:bor(w)
						end
					end
				end
			end
		elseif type == 2 then
			-- Knight
			for _,j in ipairs(KNIGHT_MOVES) do
				local w = r:move(j[1],j[2])

				if self.ocupied:bandempty(w) then
					out = out:bor(w)
				else
					if not cancapture:bandempty(w) then
						out = out:bor(w)
					end
				end
			end
		else
			-- Sliders (Rook, Bishop, Queen)
			local slides
			if type == 1 then
				slides = ROOK_SLIDES
			elseif type == 3 then
				slides = BISHOP_SLIDES
			else
				slides = QUEEN_SLIDES
			end

			for _, op in ipairs(slides) do
				local w = r
				for i=1,7 do
					w = w:move(op[1], op[2])
					if w:empty() then break end

					if self.ocupied:bandempty(w) then
						out = out:bor(w)
					else
						if not cancapture:bandempty(w) then
							out = out:bor(w)
						end
						break
					end
				end
			end
		end


		return out
	end

-- 0-5 - From Square
-- 6-11 - To Square
-- 12 - is Check
-- 13 - Is EnPassent
-- 14 - Is Castle
-- 15-19 - Promotion Piece
-- 20-24 - Moved Pice
-- 25-29 - Captured Piece


	function toString(self, mark)
		local out = {}
		for x=8,1,-1 do
			table.insert(out, RANKS:sub(x,x) .. " ")
		
			for y=1,8 do
				local n = 8*x+y-9
				local i = self:index(n)
				if i == 0 then
					table.insert(out, '-')
				else
					-- out = out .. PieceSymbols:sub(i,i)
					table.insert(out, UnicodePieces[i])
				end
				if mark ~= nil and mark:index(n) ~= 0 then
					table.insert(out, ')')
				elseif mark ~= nil and n < 63 and y < 8 and mark:index(n+1) ~= 0 then
					table.insert(out, '(')
				else
					table.insert(out, ' ')
				end
			end
			
			table.insert(out, "\n")
		end
		table.insert(out, '  '  .. FILES:gsub('.', '%1 ') .. '\n')
		table.insert(out, (self.toMove == 1 and "White" or "Black") .. ' e:' .. (self.material/100) .. "\n")
		return table.concat(out)
	end

	function moveList(self)
		local tm = self.toMove == 1 and self.white or self.black
		local castle_rank = self.toMove == 1 and Rank1 or Rank8
		local out = {}
		local function emit(id)
			if not self:applyMove(id):illegalyChecked() then
				table.insert(out, id)
			end
		end

		local cr = tm:band(self.castle):band(castle_rank)
		if not cr:empty() then
			local p = self.toMove == 1 and 11 or 12
			local tcolor = self.toMove == 1 and self.black or self.white
			local kidx = self.state[p]:ctz()


			local castle = bit32.replace(0, p, 20, 4)
			castle = bit32.replace(castle, kidx, 6, 6)
			castle = bit32.replace(castle, 1, 14)


			local mustbeemptyl = LeftMasks[4]:bxor(FileA):band(castle_rank)
			local cantbethreatened = FileD:bor(FileC):band(castle_rank):bor(self.state[p])
			if
				not cr:bandempty(FileA) and
				mustbeemptyl:bandempty(self.ocupied) and
				not self:isSquareThreatened(cantbethreatened, tcolor)
			then
				emit(bit32.replace(castle, kidx - 2, 0, 6))
			end


			local mustbeemptyr = RightMasks[3]:bxor(FileH):band(castle_rank)
			if
				not cr:bandempty(FileH) and
				mustbeemptyr:bandempty(self.ocupied) and
				not self:isSquareThreatened(mustbeemptyr:bor(self.state[p]), tcolor)
			then
				emit(bit32.replace(castle, kidx + 2, 0, 6))
			end
		end

		local sq = tm:ctz()
		repeat
			local p = self:index(sq)
			local moves = self:pmoves(sq)

			while not moves:empty() do
				local m = moves:ctz()
				moves = moves:set(m, 0)
				local id = bit32.replace(m, sq, 6, 6)
				id = bit32.replace(id, p, 20, 4)
				local mbb = Bitboard.some(m)
				if not self.ocupied:bandempty(mbb) then
					id = bit32.replace(id, self:index(m), 25, 4)
				end

				-- Check if pawn needs to be promoted
				if p == 1 and m >= 8*7 then
					for i=3,9,2 do
						emit(bit32.replace(id, i, 15, 4))
					end
				elseif p == 2 and m < 8 then
					for i=4,10,2 do
						emit(bit32.replace(id, i, 15, 4))
					end
				else
					emit(id)
				end	
			end
			sq = tm:ctzafter(sq)
		until sq == 64
		return out
	end

	function illegalyChecked(self)
		local target = self.toMove == 1 and self.state[PieceSymbols:find("k")] or self.state[PieceSymbols:find("K")]
		return self:isSquareThreatened(target, self.toMove == 1 and self.white or self.black)
	end

	function isSquareThreatened(self, target, color)
		local tm = color
		local sq = tm:ctz()
		repeat
			local moves = self:pmoves(sq)
			if not moves:bandempty(target) then
				return true
			end
			sq = color:ctzafter(sq)
		until sq == 64
		return false
	end

	function perft(self, depth)
		if depth == 0 then return 1 end
		if depth == 1 then 
			return #self:moveList()
		end
		local result = 0
		for k,m in ipairs(self:moveList()) do
			local c = self:applyMove(m):perft(depth - 1)
			if c == 0 then
				-- Perft only counts leaf nodes at target depth
				-- result = result + 1
			else
				result = result + c
			end
		end
		return result
	end


	function applyMove(self, move)
		local out = Board.new()
		table.move(self.state, 1, 12, 1, out.state)
		local from = bit32.extract(move, 6, 6)
		local to = bit32.extract(move, 0, 6)
		local promote = bit32.extract(move, 15, 4)
		local piece = self:index(from)
		local captured = self:index(to)
		local tom = Bitboard.some(to)
		local isCastle = bit32.extract(move, 14)

		if piece % 2 == 0 then
			out.moves = self.moves + 1
		end

		if captured == 1 or piece < 3 then
			out.hm = 0
		else
			out.hm = self.hm + 1
		end
		out.castle = self.castle
		out.toMove = self.toMove == 1 and 2 or 1

		if isCastle == 1 then
			local rank = piece == 11 and Rank1 or Rank8
			local colorOffset = piece - 11

			out.state[3 + colorOffset] = out.state[3 + colorOffset]:bandnot(from < to and FileH or FileA) 
			out.state[3 + colorOffset] = out.state[3 + colorOffset]:bor((from < to and FileF or FileD):band(rank)) 

			out.state[piece] = (from < to and FileG or FileC):band(rank)
			out.castle = out.castle:bandnot(rank)
			out:updateCache()
			return out
		end

		if piece < 3 then
			local dist = math.abs(to - from)
			-- Pawn moved two squares, set ep square
			if dist == 16 then
				out.ep = Bitboard.some((from + to) / 2)
			end

			-- Remove enpasent capture
			if not tom:bandempty(self.ep) then
				if piece == 1 then
					out.state[2] = out.state[2]:bandnot(self.ep:down())
				end
				if piece == 2 then
					out.state[1] = out.state[1]:bandnot(self.ep:up())
				end
			end
		end

		if piece == 3 or piece == 4 then
			out.castle = out.castle:set(from, 0)
		end

		if piece > 10 then
			local rank = piece == 11 and Rank1 or Rank8
			out.castle = out.castle:bandnot(rank)
		end

		out.state[piece] = out.state[piece]:set(from, 0)
		if promote == 0 then
			out.state[piece] = out.state[piece]:set(to, 1)
		else
			out.state[promote] = out.state[promote]:set(to, 1)
		end
		if captured ~= 0 then
			out.state[captured] = out.state[captured]:set(to, 0)
		end

		out:updateCache()
		return out
	end

end

--
-- Main
--

local failures = 0
local function test(fen, ply, target)
	local b = Board.fromFen(fen)
	if b:fen() ~= fen then
		print("FEN MISMATCH", fen, b:fen())
		failures = failures + 1
		return
	end

	local found = b:perft(ply)
	if found ~= target then
		print(fen, "Found", found, "target", target)
		failures = failures + 1
		for k,v in pairs(b:moveList()) do
			print(ucimove(v) .. ': ' .. (ply > 1 and b:applyMove(v):perft(ply-1) or '1'))
		end
		--error("Test Failure")
	else
		print("OK", found, fen)
	end
end

-- From https://www.chessprogramming.org/Perft_Results
-- If interpreter, computers, or algorithm gets too fast
-- feel free to go deeper

local testCases = {}
local function addTest(...) table.insert(testCases, {...}) end

addTest(StartingFen, 2, 400)
addTest("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 0", 1, 48)
addTest("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 0", 2, 191)
addTest("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 2, 264)
addTest("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 1, 44)
addTest("r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10", 1, 46)


local function chess()
	for k,v in ipairs(testCases) do
		test(v[1],v[2],v[3])
	end

	if failures > 0 then
		error("Test Failure")
	end
end

bench.runCode(chess, "chess with classes")
