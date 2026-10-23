-- Move generation for chess

local boardMod = require("./board")
local Board = boardMod.Board
local EMPTY = boardMod.EMPTY
local PAWN = boardMod.PAWN
local KNIGHT = boardMod.KNIGHT
local BISHOP = boardMod.BISHOP
local ROOK = boardMod.ROOK
local QUEEN = boardMod.QUEEN
local KING = boardMod.KING
local WHITE = boardMod.WHITE
local BLACK = boardMod.BLACK
local WK_CASTLE = boardMod.WK_CASTLE
local WQ_CASTLE = boardMod.WQ_CASTLE
local BK_CASTLE = boardMod.BK_CASTLE
local BQ_CASTLE = boardMod.BQ_CASTLE

export type Move = {
    from: number,
    to: number,
    promotion: number,
    capture: number,
    flags: number, -- 1=castle, 2=ep, 4=double push
}

local FLAG_CASTLE = 1
local FLAG_EP = 2
local FLAG_DOUBLE = 4

local knightOffsets = {-17, -15, -10, -6, 6, 10, 15, 17}
local bishopDirs = {-9, -7, 7, 9}
local rookDirs = {-8, -1, 1, 8}
local queenDirs = {-9, -8, -7, -1, 1, 7, 8, 9}
local kingDirs = {-9, -8, -7, -1, 1, 7, 8, 9}

local function isOnBoard(sq: number): boolean
    return sq >= 1 and sq <= 64
end

local function sameRankOrValid(from: number, to: number, dir: number): boolean
    local fromFile = ((from - 1) % 8) + 1
    local toFile = ((to - 1) % 8) + 1
    local fileDiff = math.abs(toFile - fromFile)
    if dir == -1 or dir == 1 then
        return fileDiff == 1
    end
    if dir == -8 or dir == 8 then
        return fileDiff == 0
    end
    return fileDiff <= 1
end

local function knightValid(from: number, to: number): boolean
    if not isOnBoard(to) then return false end
    local fromFile = ((from - 1) % 8) + 1
    local fromRank = math.floor((from - 1) / 8) + 1
    local toFile = ((to - 1) % 8) + 1
    local toRank = math.floor((to - 1) / 8) + 1
    local fd = math.abs(toFile - fromFile)
    local rd = math.abs(toRank - fromRank)
    return (fd == 1 and rd == 2) or (fd == 2 and rd == 1)
end

local function addMove(moves: {Move}, from: number, to: number, capture: number, promotion: number, flags: number)
    table.insert(moves, {
        from = from,
        to = to,
        promotion = promotion,
        capture = capture,
        flags = flags,
    })
end

local function generatePawnMoves(board: boardMod.Board, moves: {Move}, sq: number, color: number)
    local dir = if color == WHITE then 8 else -8
    local startRank = if color == WHITE then 2 else 7
    local promoRank = if color == WHITE then 8 else 1
    local enemy = if color == WHITE then BLACK else WHITE
    local rank = math.floor((sq - 1) / 8) + 1
    local file = ((sq - 1) % 8) + 1

    local forward = sq + dir
    if isOnBoard(forward) and board.squares[forward] == EMPTY then
        local toRank = math.floor((forward - 1) / 8) + 1
        if toRank == promoRank then
            for _, promo in {QUEEN, ROOK, BISHOP, KNIGHT} do
                addMove(moves, sq, forward, EMPTY, promo, 0)
            end
        else
            addMove(moves, sq, forward, EMPTY, 0, 0)
            if rank == startRank then
                local double = sq + dir * 2
                if board.squares[double] == EMPTY then
                    addMove(moves, sq, double, EMPTY, 0, FLAG_DOUBLE)
                end
            end
        end
    end

    for _, captDir in {dir - 1, dir + 1} do
        local target = sq + captDir
        if isOnBoard(target) then
            local tf = ((target - 1) % 8) + 1
            if math.abs(tf - file) == 1 then
                local piece = board.squares[target]
                if piece ~= EMPTY and bit32.band(piece, 24) == enemy then
                    local toRank = math.floor((target - 1) / 8) + 1
                    if toRank == promoRank then
                        for _, promo in {QUEEN, ROOK, BISHOP, KNIGHT} do
                            addMove(moves, sq, target, piece, promo, 0)
                        end
                    else
                        addMove(moves, sq, target, piece, 0, 0)
                    end
                elseif target == board.epSquare then
                    local epPawn = bit32.bor(enemy, PAWN)
                    addMove(moves, sq, target, epPawn, 0, FLAG_EP)
                end
            end
        end
    end
end

local function generateSlidingMoves(board: boardMod.Board, moves: {Move}, sq: number, color: number, dirs: {number})
    local enemy = if color == WHITE then BLACK else WHITE
    for _, dir in dirs do
        local current = sq
        while true do
            local next = current + dir
            if not isOnBoard(next) then break end
            if not sameRankOrValid(current, next, dir) then break end
            local piece = board.squares[next]
            if piece == EMPTY then
                addMove(moves, sq, next, EMPTY, 0, 0)
            elseif bit32.band(piece, 24) == enemy then
                addMove(moves, sq, next, piece, 0, 0)
                break
            else
                break
            end
            current = next
        end
    end
end

local function generateKnightMoves(board: boardMod.Board, moves: {Move}, sq: number, color: number)
    local enemy = if color == WHITE then BLACK else WHITE
    for _, offset in knightOffsets do
        local target = sq + offset
        if knightValid(sq, target) then
            local piece = board.squares[target]
            if piece == EMPTY then
                addMove(moves, sq, target, EMPTY, 0, 0)
            elseif bit32.band(piece, 24) == enemy then
                addMove(moves, sq, target, piece, 0, 0)
            end
        end
    end
end

local function generateKingMoves(board: boardMod.Board, moves: {Move}, sq: number, color: number)
    local enemy = if color == WHITE then BLACK else WHITE
    for _, dir in kingDirs do
        local target = sq + dir
        if isOnBoard(target) and sameRankOrValid(sq, target, dir) then
            local piece = board.squares[target]
            if piece == EMPTY then
                addMove(moves, sq, target, EMPTY, 0, 0)
            elseif bit32.band(piece, 24) == enemy then
                addMove(moves, sq, target, piece, 0, 0)
            end
        end
    end

    if color == WHITE then
        if bit32.band(board.castling, WK_CASTLE) ~= 0 then
            if board.squares[6] == EMPTY and board.squares[7] == EMPTY then
                addMove(moves, sq, 7, EMPTY, 0, FLAG_CASTLE)
            end
        end
        if bit32.band(board.castling, WQ_CASTLE) ~= 0 then
            if board.squares[4] == EMPTY and board.squares[3] == EMPTY and board.squares[2] == EMPTY then
                addMove(moves, sq, 3, EMPTY, 0, FLAG_CASTLE)
            end
        end
    else
        if bit32.band(board.castling, BK_CASTLE) ~= 0 then
            if board.squares[62] == EMPTY and board.squares[63] == EMPTY then
                addMove(moves, sq, 63, EMPTY, 0, FLAG_CASTLE)
            end
        end
        if bit32.band(board.castling, BQ_CASTLE) ~= 0 then
            if board.squares[60] == EMPTY and board.squares[59] == EMPTY and board.squares[58] == EMPTY then
                addMove(moves, sq, 59, EMPTY, 0, FLAG_CASTLE)
            end
        end
    end
end

local function generatePseudoLegalMoves(board: boardMod.Board): {Move}
    local moves: {Move} = {}
    local color = board:friendlyColor()
    for sq = 1, 64 do
        local piece = board.squares[sq]
        if piece ~= EMPTY and bit32.band(piece, 24) == color then
            local ptype = bit32.band(piece, 7)
            if ptype == PAWN then
                generatePawnMoves(board, moves, sq, color)
            elseif ptype == KNIGHT then
                generateKnightMoves(board, moves, sq, color)
            elseif ptype == BISHOP then
                generateSlidingMoves(board, moves, sq, color, bishopDirs)
            elseif ptype == ROOK then
                generateSlidingMoves(board, moves, sq, color, rookDirs)
            elseif ptype == QUEEN then
                generateSlidingMoves(board, moves, sq, color, queenDirs)
            elseif ptype == KING then
                generateKingMoves(board, moves, sq, color)
            end
        end
    end
    return moves
end

local function isSquareAttacked(board: boardMod.Board, sq: number, byColor: number): boolean
    local enemy = byColor

    for _, offset in knightOffsets do
        local target = sq + offset
        if knightValid(sq, target) then
            local piece = board.squares[target]
            if piece ~= EMPTY and bit32.band(piece, 24) == enemy and bit32.band(piece, 7) == KNIGHT then
                return true
            end
        end
    end

    for _, dir in bishopDirs do
        local current = sq
        while true do
            local next = current + dir
            if not isOnBoard(next) then break end
            if not sameRankOrValid(current, next, dir) then break end
            local piece = board.squares[next]
            if piece ~= EMPTY then
                if bit32.band(piece, 24) == enemy then
                    local pt = bit32.band(piece, 7)
                    if pt == BISHOP or pt == QUEEN then return true end
                end
                break
            end
            current = next
        end
    end

    for _, dir in rookDirs do
        local current = sq
        while true do
            local next = current + dir
            if not isOnBoard(next) then break end
            if not sameRankOrValid(current, next, dir) then break end
            local piece = board.squares[next]
            if piece ~= EMPTY then
                if bit32.band(piece, 24) == enemy then
                    local pt = bit32.band(piece, 7)
                    if pt == ROOK or pt == QUEEN then return true end
                end
                break
            end
            current = next
        end
    end

    for _, dir in kingDirs do
        local target = sq + dir
        if isOnBoard(target) and sameRankOrValid(sq, target, dir) then
            local piece = board.squares[target]
            if piece ~= EMPTY and bit32.band(piece, 24) == enemy and bit32.band(piece, 7) == KING then
                return true
            end
        end
    end

    local pawnDir = if enemy == WHITE then -8 else 8
    for _, captDir in {pawnDir - 1, pawnDir + 1} do
        local target = sq + captDir
        if isOnBoard(target) then
            local tf = ((target - 1) % 8) + 1
            local sf = ((sq - 1) % 8) + 1
            if math.abs(tf - sf) == 1 then
                local piece = board.squares[target]
                if piece ~= EMPTY and bit32.band(piece, 24) == enemy and bit32.band(piece, 7) == PAWN then
                    return true
                end
            end
        end
    end

    return false
end

local function makeMove(board: boardMod.Board, move: Move): boardMod.Board
    local b = board:clone()
    local piece = b.squares[move.from]
    local color = bit32.band(piece, 24)

    b.squares[move.from] = EMPTY
    b.squares[move.to] = piece

    if move.promotion ~= 0 then
        b.squares[move.to] = bit32.bor(color, move.promotion)
    end

    if bit32.band(move.flags, FLAG_EP) ~= 0 then
        local epPawnSq = move.to + (if color == WHITE then -8 else 8)
        b.squares[epPawnSq] = EMPTY
    end

    if bit32.band(move.flags, FLAG_CASTLE) ~= 0 then
        if move.to == 7 then
            b.squares[8] = EMPTY
            b.squares[6] = bit32.bor(WHITE, ROOK)
        elseif move.to == 3 then
            b.squares[1] = EMPTY
            b.squares[4] = bit32.bor(WHITE, ROOK)
        elseif move.to == 63 then
            b.squares[64] = EMPTY
            b.squares[62] = bit32.bor(BLACK, ROOK)
        elseif move.to == 59 then
            b.squares[57] = EMPTY
            b.squares[60] = bit32.bor(BLACK, ROOK)
        end
    end

    if bit32.band(move.flags, FLAG_DOUBLE) ~= 0 then
        b.epSquare = move.from + (if color == WHITE then 8 else -8)
    else
        b.epSquare = 0
    end

    if bit32.band(piece, 7) == KING then
        if color == WHITE then
            b.castling = bit32.band(b.castling, bit32.bnot(bit32.bor(WK_CASTLE, WQ_CASTLE)))
        else
            b.castling = bit32.band(b.castling, bit32.bnot(bit32.bor(BK_CASTLE, BQ_CASTLE)))
        end
    end
    if move.from == 1 or move.to == 1 then
        b.castling = bit32.band(b.castling, bit32.bnot(WQ_CASTLE))
    end
    if move.from == 8 or move.to == 8 then
        b.castling = bit32.band(b.castling, bit32.bnot(WK_CASTLE))
    end
    if move.from == 57 or move.to == 57 then
        b.castling = bit32.band(b.castling, bit32.bnot(BQ_CASTLE))
    end
    if move.from == 64 or move.to == 64 then
        b.castling = bit32.band(b.castling, bit32.bnot(BK_CASTLE))
    end

    if bit32.band(piece, 7) == PAWN or move.capture ~= EMPTY then
        b.halfmoveClock = 0
    else
        b.halfmoveClock = b.halfmoveClock + 1
    end

    if not b.whiteToMove then
        b.fullmoveNumber = b.fullmoveNumber + 1
    end
    b.whiteToMove = not b.whiteToMove
    b:computeZobrist()
    return b
end

local function generateLegalMoves(board: boardMod.Board): {Move}
    local pseudo = generatePseudoLegalMoves(board)
    local legal: {Move} = {}
    local color = board:friendlyColor()
    local enemy = board:enemyColor()

    for _, move in pseudo do
        if bit32.band(move.flags, FLAG_CASTLE) ~= 0 then
            local kingSq = move.from
            if isSquareAttacked(board, kingSq, enemy) then continue end
            local step = if move.to > move.from then 1 else -1
            local mid = kingSq + step
            if isSquareAttacked(board, mid, enemy) then continue end
            if isSquareAttacked(board, move.to, enemy) then continue end
        end

        local newBoard = makeMove(board, move)
        local kingSq = newBoard:findKing(color)
        if kingSq > 0 and not isSquareAttacked(newBoard, kingSq, enemy) then
            table.insert(legal, move)
        end
    end
    return legal
end

local function isInCheck(board: boardMod.Board): boolean
    local color = board:friendlyColor()
    local enemy = board:enemyColor()
    local kingSq = board:findKing(color)
    if kingSq == 0 then return true end
    return isSquareAttacked(board, kingSq, enemy)
end

return {
    generateLegalMoves = generateLegalMoves,
    generatePseudoLegalMoves = generatePseudoLegalMoves,
    isSquareAttacked = isSquareAttacked,
    isInCheck = isInCheck,
    makeMove = makeMove,
    FLAG_CASTLE = FLAG_CASTLE,
    FLAG_EP = FLAG_EP,
    FLAG_DOUBLE = FLAG_DOUBLE,
}
