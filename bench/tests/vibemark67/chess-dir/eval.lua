-- Chess position evaluation with piece-square tables

local boardMod = require("./board")
local EMPTY = boardMod.EMPTY
local PAWN = boardMod.PAWN
local KNIGHT = boardMod.KNIGHT
local BISHOP = boardMod.BISHOP
local ROOK = boardMod.ROOK
local QUEEN = boardMod.QUEEN
local KING = boardMod.KING
local WHITE = boardMod.WHITE
local BLACK = boardMod.BLACK

local pieceValues = {
    [PAWN] = 100,
    [KNIGHT] = 320,
    [BISHOP] = 330,
    [ROOK] = 500,
    [QUEEN] = 900,
    [KING] = 20000,
}

local pawnTable = {
     0,  0,  0,  0,  0,  0,  0,  0,
    50, 50, 50, 50, 50, 50, 50, 50,
    10, 10, 20, 30, 30, 20, 10, 10,
     5,  5, 10, 25, 25, 10,  5,  5,
     0,  0,  0, 20, 20,  0,  0,  0,
     5, -5,-10,  0,  0,-10, -5,  5,
     5, 10, 10,-20,-20, 10, 10,  5,
     0,  0,  0,  0,  0,  0,  0,  0,
}

local knightTable = {
    -50,-40,-30,-30,-30,-30,-40,-50,
    -40,-20,  0,  0,  0,  0,-20,-40,
    -30,  0, 10, 15, 15, 10,  0,-30,
    -30,  5, 15, 20, 20, 15,  5,-30,
    -30,  0, 15, 20, 20, 15,  0,-30,
    -30,  5, 10, 15, 15, 10,  5,-30,
    -40,-20,  0,  5,  5,  0,-20,-40,
    -50,-40,-30,-30,-30,-30,-40,-50,
}

local bishopTable = {
    -20,-10,-10,-10,-10,-10,-10,-20,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5, 10, 10,  5,  0,-10,
    -10,  5,  5, 10, 10,  5,  5,-10,
    -10,  0, 10, 10, 10, 10,  0,-10,
    -10, 10, 10, 10, 10, 10, 10,-10,
    -10,  5,  0,  0,  0,  0,  5,-10,
    -20,-10,-10,-10,-10,-10,-10,-20,
}

local rookTable = {
     0,  0,  0,  0,  0,  0,  0,  0,
     5, 10, 10, 10, 10, 10, 10,  5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
    -5,  0,  0,  0,  0,  0,  0, -5,
     0,  0,  0,  5,  5,  0,  0,  0,
}

local queenTable = {
    -20,-10,-10, -5, -5,-10,-10,-20,
    -10,  0,  0,  0,  0,  0,  0,-10,
    -10,  0,  5,  5,  5,  5,  0,-10,
     -5,  0,  5,  5,  5,  5,  0, -5,
      0,  0,  5,  5,  5,  5,  0, -5,
    -10,  5,  5,  5,  5,  5,  0,-10,
    -10,  0,  5,  0,  0,  0,  0,-10,
    -20,-10,-10, -5, -5,-10,-10,-20,
}

local kingMiddleTable = {
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -30,-40,-40,-50,-50,-40,-40,-30,
    -20,-30,-30,-40,-40,-30,-30,-20,
    -10,-20,-20,-20,-20,-20,-20,-10,
     20, 20,  0,  0,  0,  0, 20, 20,
     20, 30, 10,  0,  0, 10, 30, 20,
}

local kingEndTable = {
    -50,-40,-30,-20,-20,-30,-40,-50,
    -30,-20,-10,  0,  0,-10,-20,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 30, 40, 40, 30,-10,-30,
    -30,-10, 20, 30, 30, 20,-10,-30,
    -30,-30,  0,  0,  0,  0,-30,-30,
    -50,-30,-30,-30,-30,-30,-30,-50,
}

local pstTables = {
    [PAWN] = pawnTable,
    [KNIGHT] = knightTable,
    [BISHOP] = bishopTable,
    [ROOK] = rookTable,
    [QUEEN] = queenTable,
}

local function mirror(sq: number): number
    local file = ((sq - 1) % 8) + 1
    local rank = math.floor((sq - 1) / 8) + 1
    local mirrorRank = 9 - rank
    return (mirrorRank - 1) * 8 + file
end

local function evaluate(board: boardMod.Board): number
    local score = 0
    local whiteMaterial = 0
    local blackMaterial = 0

    for sq = 1, 64 do
        local piece = board.squares[sq]
        if piece ~= EMPTY then
            local color = bit32.band(piece, 24)
            local ptype = bit32.band(piece, 7)
            local value = pieceValues[ptype] or 0

            if color == WHITE then
                whiteMaterial += value
            else
                blackMaterial += value
            end
        end
    end

    local isEndgame = (whiteMaterial + blackMaterial - 40000) < 2600

    for sq = 1, 64 do
        local piece = board.squares[sq]
        if piece ~= EMPTY then
            local color = bit32.band(piece, 24)
            local ptype = bit32.band(piece, 7)
            local value = pieceValues[ptype] or 0
            local pst = 0

            if ptype == KING then
                local tbl = if isEndgame then kingEndTable else kingMiddleTable
                if color == WHITE then
                    pst = tbl[mirror(sq)]
                else
                    pst = tbl[sq]
                end
            else
                local tbl = pstTables[ptype]
                if tbl then
                    if color == WHITE then
                        pst = tbl[mirror(sq)]
                    else
                        pst = tbl[sq]
                    end
                end
            end

            if color == WHITE then
                score += value + pst
            else
                score -= value + pst
            end
        end
    end

    return if board.whiteToMove then score else -score
end

return {
    evaluate = evaluate,
    pieceValues = pieceValues,
}
