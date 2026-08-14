-- Chess board representation and utilities

local Board = {}
Board.__index = Board

export type Board = typeof(setmetatable({} :: {
    squares: {number},
    whiteToMove: boolean,
    castling: number, -- bitmask: WK=1, WQ=2, BK=4, BQ=8
    epSquare: number, -- 0 if none
    halfmoveClock: number,
    fullmoveNumber: number,
    zobrist: number,
}, Board))

local EMPTY = 0
local PAWN = 1
local KNIGHT = 2
local BISHOP = 3
local ROOK = 4
local QUEEN = 5
local KING = 6
local WHITE = 8
local BLACK = 16

local WK_CASTLE = 1
local WQ_CASTLE = 2
local BK_CASTLE = 4
local BQ_CASTLE = 8

local zobristPiece: {{number}} = {}
local zobristCastle: {number} = {}
local zobristEp: {number} = {}
local zobristSide: number = 0

local function pseudoRandom(seed: number): (number, number)
    seed = bit32.bxor(seed, bit32.lshift(seed, 13))
    seed = bit32.bxor(seed, bit32.rshift(seed, 17))
    seed = bit32.bxor(seed, bit32.lshift(seed, 5))
    local val = bit32.band(seed, 0x7FFFFFFF)
    return val, seed
end

local function initZobrist()
    local seed = 1070372
    for sq = 1, 64 do
        zobristPiece[sq] = {}
        for piece = 1, 31 do
            local val
            val, seed = pseudoRandom(seed)
            zobristPiece[sq][piece] = val
        end
    end
    for i = 1, 16 do
        local val
        val, seed = pseudoRandom(seed)
        zobristCastle[i] = val
    end
    for i = 1, 64 do
        local val
        val, seed = pseudoRandom(seed)
        zobristEp[i] = val
    end
    local val
    val, seed = pseudoRandom(seed)
    zobristSide = val
end

initZobrist()

function Board.new(): Board
    local self = setmetatable({}, Board)
    self.squares = table.create(64, EMPTY)
    self.whiteToMove = true
    self.castling = 0
    self.epSquare = 0
    self.halfmoveClock = 0
    self.fullmoveNumber = 1
    self.zobrist = 0
    return self
end

function Board.startPos(): Board
    local b = Board.new()
    local backRank = {ROOK, KNIGHT, BISHOP, QUEEN, KING, BISHOP, KNIGHT, ROOK}
    for i = 1, 8 do
        b.squares[i] = bit32.bor(WHITE, backRank[i])
        b.squares[8 + i] = bit32.bor(WHITE, PAWN)
        b.squares[48 + i] = bit32.bor(BLACK, PAWN)
        b.squares[56 + i] = bit32.bor(BLACK, backRank[i])
    end
    b.castling = bit32.bor(WK_CASTLE, WQ_CASTLE, BK_CASTLE, BQ_CASTLE)
    b:computeZobrist()
    return b
end

function Board.computeZobrist(self: Board)
    local h = 0
    for sq = 1, 64 do
        local p = self.squares[sq]
        if p ~= EMPTY then
            h = bit32.bxor(h, zobristPiece[sq][p])
        end
    end
    if self.castling > 0 then
        h = bit32.bxor(h, zobristCastle[self.castling + 1])
    end
    if self.epSquare > 0 then
        h = bit32.bxor(h, zobristEp[self.epSquare])
    end
    if not self.whiteToMove then
        h = bit32.bxor(h, zobristSide)
    end
    self.zobrist = h
end

function Board.clone(self: Board): Board
    local b = Board.new()
    table.move(self.squares, 1, 64, 1, b.squares)
    b.whiteToMove = self.whiteToMove
    b.castling = self.castling
    b.epSquare = self.epSquare
    b.halfmoveClock = self.halfmoveClock
    b.fullmoveNumber = self.fullmoveNumber
    b.zobrist = self.zobrist
    return b
end

function Board.pieceAt(self: Board, sq: number): number
    return self.squares[sq]
end

function Board.colorAt(self: Board, sq: number): number
    return bit32.band(self.squares[sq], 24)
end

function Board.typeAt(self: Board, sq: number): number
    return bit32.band(self.squares[sq], 7)
end

function Board.file(sq: number): number
    return ((sq - 1) % 8) + 1
end

function Board.rank(sq: number): number
    return math.floor((sq - 1) / 8) + 1
end

function Board.sqFromFileRank(file: number, rank: number): number
    return (rank - 1) * 8 + file
end

function Board.friendlyColor(self: Board): number
    return if self.whiteToMove then WHITE else BLACK
end

function Board.enemyColor(self: Board): number
    return if self.whiteToMove then BLACK else WHITE
end

function Board.findKing(self: Board, color: number): number
    local target = bit32.bor(color, KING)
    for sq = 1, 64 do
        if self.squares[sq] == target then
            return sq
        end
    end
    return 0
end

return {
    Board = Board,
    EMPTY = EMPTY,
    PAWN = PAWN,
    KNIGHT = KNIGHT,
    BISHOP = BISHOP,
    ROOK = ROOK,
    QUEEN = QUEEN,
    KING = KING,
    WHITE = WHITE,
    BLACK = BLACK,
    WK_CASTLE = WK_CASTLE,
    WQ_CASTLE = WQ_CASTLE,
    BK_CASTLE = BK_CASTLE,
    BQ_CASTLE = BQ_CASTLE,
    zobristPiece = zobristPiece,
    zobristCastle = zobristCastle,
    zobristEp = zobristEp,
    zobristSide = zobristSide,
}
