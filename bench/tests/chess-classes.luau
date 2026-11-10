-- --bench-args: --fflags=DebugLuauUserDefinedClasses,DebugLuauUserDefinedClassesRuntime,LuauEmitCallFeedback
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
	private _l: number
	private _h: number

	function toString(self)
		local out = {}
		local src = self._h
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
				src = self._l
			end
			table.insert(out, "\n")
		end
		table.insert(out, '  ' .. FILES:gsub('.', '%1 ') .. '\n')
		table.insert(out, '#: ' .. self:popcnt() .. "\tl:" .. self._l .. "\th:" .. self._h)
		return table.concat(out)
	end

	function __init(self, l, h)
		self._l = l
		self._h = h
	end

	function from(l, h)
		return Bitboard(l, h)
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
		return popcnt32(self._l) + popcnt32(self._h)
	end

	function band(self, other)
		return Bitboard.from(bit32.band(self._l,other._l), bit32.band(self._h, other._h))
	end

	function bandnot(self, other)
		return Bitboard.from(bit32.band(self._l,bit32.bnot(other._l)), bit32.band(self._h, bit32.bnot(other._h)))
	end

	function bandempty(self, other)
		return bit32.band(self._l,other._l) == 0 and bit32.band(self._h, other._h) == 0
	end

	function bor(self, other)
		return Bitboard.from(bit32.bor(self._l,other._l), bit32.bor(self._h, other._h))
	end

	function bxor(self, other)
		return Bitboard.from(bit32.bxor(self._l,other._l), bit32.bxor(self._h, other._h))
	end

	function inverse(self)
		return Bitboard.from(bit32.bxor(self._l,0xFFFFFFFF), bit32.bxor(self._h, 0xFFFFFFFF))
	end

	function empty(self)
		return self._h == 0 and self._l == 0
	end

	function ctz(self)
		local result = bit32.countrz(self._l)
		if result == 32 then
			return bit32.countrz(self._h) + 32
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
			return Bitboard.from(0, bit32.lshift(self._l, amt-32))
		end

		local l = bit32.lshift(self._l, amt)
		local h = bit32.bor(
			bit32.lshift(self._h, amt),
			bit32.extract(self._l, 32-amt, amt)
		)
		return Bitboard.from(l, h)
	end

	function rshift(self, amt)
		assert(amt >= 0)
		if amt == 0 then return self end
		local h = bit32.rshift(self._h, amt)
		local l = bit32.bor(
			bit32.rshift(self._l, amt),
			bit32.lshift(bit32.extract(self._h, 0, amt), 32-amt)
		)
		return Bitboard.from(l, h)
	end

	function index(self, i)
		if i > 31 then
			return bit32.extract(self._h, i - 32)
		else
			return bit32.extract(self._l, i)
		end
	end

	function set(self, i, v)
		if i > 31 then
			return Bitboard.from(self._l, bit32.replace(self._h, v, i - 32))
		else
			return Bitboard.from(bit32.replace(self._l, v, i), self._h)
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

	private _occupied: Bitboard
	private _white: Bitboard
	private _black: Bitboard
	private _unoccupied: Bitboard
	private _ep: Bitboard
	private _castle: Bitboard
	private _toMove: number
	private _hm: number
	private _moves: number
	private _material: number
	private _state: { [number]: Bitboard }

	function __init(self)
		self._occupied = BITBOARD_ZERO
		self._white = BITBOARD_ZERO
		self._black = BITBOARD_ZERO
		self._unoccupied = BITBOARD_FULL
		self._ep = BITBOARD_ZERO
		self._castle = BITBOARD_ZERO
		self._toMove = 1
		self._hm = 0
		self._moves = 0
		self._material = 0
		self._state = table.create(12, BITBOARD_ZERO)
	end

	function fromFen(fen)
		local b = Board()
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
				b._state[pidx] = b._state[pidx]:set(rank*8+file, 1)
				file = file + 1
			end
		end

		local move, castle, ep, hm, m = string.match(fen, "^ ([bw]) ([KQkq-]*) ([a-h-][0-9]?) (%d*) (%d*)", i)
		if move == nil then print(fen:sub(i)) end
		b._toMove = move == 'w' and 1 or 2

		if ep ~= "-" then
			b._ep = Bitboard.some(square(ep))
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

			b._castle = oo
		end

		b._hm = hm
		b._moves = m

		b:_updateCache()
		return b

	end

	private function _index(self, idx)
		if self._white:index(idx) == 1 then
			for p=1,12,2 do
				if self._state[p]:index(idx) == 1 then
					return p
				end
			end
		else
			for p=2,12,2 do
				if self._state[p]:index(idx) == 1 then
					return p
				end
			end
		end

		return 0
	end

	private function _updateCache(self)
		for i=1,11,2 do
			self._white = self._white:bor(self._state[i])
			self._black = self._black:bor(self._state[i+1])
		end

		self._occupied = self._black:bor(self._white)
		self._unoccupied = self._occupied:inverse()
		self._material =
			100*self._state[1]:popcnt() - 100*self._state[2]:popcnt() +
			500*self._state[3]:popcnt() - 500*self._state[4]:popcnt() +
			300*self._state[5]:popcnt() - 300*self._state[6]:popcnt() +
			300*self._state[7]:popcnt() - 300*self._state[8]:popcnt() +
			900*self._state[9]:popcnt() - 900*self._state[10]:popcnt()

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
			local p = self:_index(idx)
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

		table.insert(out, self._toMove == 1 and ' w ' or ' b ')
		if self._castle:empty() then
			table.insert(out, '-')
		else
			if self._castle:index(7) == 1 then table.insert(out, 'K') end
			if self._castle:index(0) == 1 then table.insert(out, 'Q') end
			if self._castle:index(63) == 1 then table.insert(out, 'k') end
			if self._castle:index(56) == 1 then table.insert(out, 'q') end
		end

		table.insert(out, ' ')
		if self._ep:empty() then
			table.insert(out, '-')
		else
			table.insert(out, squareName(self._ep:ctz()))
		end

		table.insert(out, ' ' .. self._hm)
		table.insert(out, ' ' .. self._moves)

		return table.concat(out)
	end

	private function _pmoves(self, idx)
		return self:_generate(idx)
	end

	private function _pcaptures(self, idx)
		return self:_generate(idx):band(self._occupied)
	end

	private function _generate(self, idx)
		local piece = self:_index(idx)
		local r = Bitboard.some(idx)
		local out = BITBOARD_ZERO
		local type = bit32.rshift(piece - 1, 1)
		local cancapture = piece % 2 == 1 and self._black or self._white

		if piece == 0 then return BITBOARD_ZERO end

		if type == 0 then
			-- Pawn
			local d = -(piece*2 - 3)
			local movetwo = piece == 1 and Rank3 or Rank6

			out = out:bor(r:move(0,d):band(self._unoccupied))
			out = out:bor(out:band(movetwo):move(0,d):band(self._unoccupied))

			local captures = r:move(0,d)
			captures = captures:right():bor(captures:left())

			if not captures:bandempty(self._ep) then
				out = out:bor(self._ep)
			end

			captures = captures:band(cancapture)
			out = out:bor(captures)

			return out
		elseif type == 5 then
			-- King
			for x=-1,1,1 do
				for y = -1,1,1 do
					local w = r:move(x,y)
					if self._occupied:bandempty(w) then
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

				if self._occupied:bandempty(w) then
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

					if self._occupied:bandempty(w) then
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
				local i = self:_index(n)
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
		table.insert(out, (self._toMove == 1 and "White" or "Black") .. ' e:' .. (self._material/100) .. "\n")
		return table.concat(out)
	end

	function moveList(self)
		local tm = self._toMove == 1 and self._white or self._black
		local castle_rank = self._toMove == 1 and Rank1 or Rank8
		local out = {}
		local function emit(id)
			if not self:applyMove(id):_illegalyChecked() then
				table.insert(out, id)
			end
		end

		local cr = tm:band(self._castle):band(castle_rank)
		if not cr:empty() then
			local p = self._toMove == 1 and 11 or 12
			local tcolor = self._toMove == 1 and self._black or self._white
			local kidx = self._state[p]:ctz()


			local castle = bit32.replace(0, p, 20, 4)
			castle = bit32.replace(castle, kidx, 6, 6)
			castle = bit32.replace(castle, 1, 14)


			local mustbeemptyl = LeftMasks[4]:bxor(FileA):band(castle_rank)
			local cantbethreatened = FileD:bor(FileC):band(castle_rank):bor(self._state[p])
			if
				not cr:bandempty(FileA) and
				mustbeemptyl:bandempty(self._occupied) and
				not self:_isSquareThreatened(cantbethreatened, tcolor)
			then
				emit(bit32.replace(castle, kidx - 2, 0, 6))
			end


			local mustbeemptyr = RightMasks[3]:bxor(FileH):band(castle_rank)
			if
				not cr:bandempty(FileH) and
				mustbeemptyr:bandempty(self._occupied) and
				not self:_isSquareThreatened(mustbeemptyr:bor(self._state[p]), tcolor)
			then
				emit(bit32.replace(castle, kidx + 2, 0, 6))
			end
		end

		local sq = tm:ctz()
		repeat
			local p = self:_index(sq)
			local moves = self:_pmoves(sq)

			while not moves:empty() do
				local m = moves:ctz()
				moves = moves:set(m, 0)
				local id = bit32.replace(m, sq, 6, 6)
				id = bit32.replace(id, p, 20, 4)
				local mbb = Bitboard.some(m)
				if not self._occupied:bandempty(mbb) then
					id = bit32.replace(id, self:_index(m), 25, 4)
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

	private function _illegalyChecked(self)
		local target = self._toMove == 1 and self._state[PieceSymbols:find("k")] or self._state[PieceSymbols:find("K")]
		return self:_isSquareThreatened(target, self._toMove == 1 and self._white or self._black)
	end

	private function _isSquareThreatened(self, target, color)
		local tm = color
		local sq = tm:ctz()
		repeat
			local moves = self:_pmoves(sq)
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
		local out = Board()
		table.move(self._state, 1, 12, 1, out._state)
		local from = bit32.extract(move, 6, 6)
		local to = bit32.extract(move, 0, 6)
		local promote = bit32.extract(move, 15, 4)
		local piece = self:_index(from)
		local captured = self:_index(to)
		local tom = Bitboard.some(to)
		local isCastle = bit32.extract(move, 14)

		if piece % 2 == 0 then
			out._moves = self._moves + 1
		end

		if captured == 1 or piece < 3 then
			out._hm = 0
		else
			out._hm = self._hm + 1
		end
		out._castle = self._castle
		out._toMove = self._toMove == 1 and 2 or 1

		if isCastle == 1 then
			local rank = piece == 11 and Rank1 or Rank8
			local colorOffset = piece - 11

			out._state[3 + colorOffset] = out._state[3 + colorOffset]:bandnot(from < to and FileH or FileA) 
			out._state[3 + colorOffset] = out._state[3 + colorOffset]:bor((from < to and FileF or FileD):band(rank)) 

			out._state[piece] = (from < to and FileG or FileC):band(rank)
			out._castle = out._castle:bandnot(rank)
			out:_updateCache()
			return out
		end

		if piece < 3 then
			local dist = math.abs(to - from)
			-- Pawn moved two squares, set ep square
			if dist == 16 then
				out._ep = Bitboard.some((from + to) / 2)
			end

			-- Remove enpasent capture
			if not tom:bandempty(self._ep) then
				if piece == 1 then
					out._state[2] = out._state[2]:bandnot(self._ep:down())
				end
				if piece == 2 then
					out._state[1] = out._state[1]:bandnot(self._ep:up())
				end
			end
		end

		if piece == 3 or piece == 4 then
			out._castle = out._castle:set(from, 0)
		end

		if piece > 10 then
			local rank = piece == 11 and Rank1 or Rank8
			out._castle = out._castle:bandnot(rank)
		end

		out._state[piece] = out._state[piece]:set(from, 0)
		if promote == 0 then
			out._state[piece] = out._state[piece]:set(to, 1)
		else
			out._state[promote] = out._state[promote]:set(to, 1)
		end
		if captured ~= 0 then
			out._state[captured] = out._state[captured]:set(to, 0)
		end

		out:_updateCache()
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
