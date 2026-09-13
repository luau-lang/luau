-- Alpha-beta search with iterative deepening, quiescence, and transposition table

local boardMod = require("./board")
local movegen = require("./movegen")
local eval = require("./eval")

local INFINITY = 999999
local MATE_SCORE = 100000

local TT_EXACT = 0
local TT_ALPHA = 1
local TT_BETA = 2

type TTEntry = {
    hash: number,
    depth: number,
    score: number,
    flag: number,
    bestFrom: number,
    bestTo: number,
}

local ttSize = 65536
local ttMask = ttSize - 1
local tt: {TTEntry?} = table.create(ttSize, nil)

local nodesSearched = 0

local function ttProbe(hash: number, depth: number, alpha: number, beta: number): (number?, number?, number?)
    local idx = bit32.band(hash, ttMask) + 1
    local entry = tt[idx]
    if entry and entry.hash == hash then
        if entry.depth >= depth then
            if entry.flag == TT_EXACT then
                return entry.score, entry.bestFrom, entry.bestTo
            elseif entry.flag == TT_ALPHA and entry.score <= alpha then
                return alpha, entry.bestFrom, entry.bestTo
            elseif entry.flag == TT_BETA and entry.score >= beta then
                return beta, entry.bestFrom, entry.bestTo
            end
        end
        return nil, entry.bestFrom, entry.bestTo
    end
    return nil, nil, nil
end

local function ttStore(hash: number, depth: number, score: number, flag: number, bestFrom: number, bestTo: number)
    local idx = bit32.band(hash, ttMask) + 1
    tt[idx] = {
        hash = hash,
        depth = depth,
        score = score,
        flag = flag,
        bestFrom = bestFrom,
        bestTo = bestTo,
    }
end

local function mvvLva(move: movegen.Move): number
    if move.capture == boardMod.EMPTY then return 0 end
    local captureVal = eval.pieceValues[bit32.band(move.capture, 7)] or 0
    return captureVal * 10
end

local function orderMoves(moves: {movegen.Move}, ttFrom: number?, ttTo: number?)
    local scores: {number} = table.create(#moves, 0)
    for i, move in moves do
        local s = mvvLva(move)
        if ttFrom and ttTo and move.from == ttFrom and move.to == ttTo then
            s += 1000000
        end
        scores[i] = s
    end

    for i = 1, #moves - 1 do
        local bestIdx = i
        local bestScore = scores[i]
        for j = i + 1, #moves do
            if scores[j] > bestScore then
                bestIdx = j
                bestScore = scores[j]
            end
        end
        if bestIdx ~= i then
            moves[i], moves[bestIdx] = moves[bestIdx], moves[i]
            scores[i], scores[bestIdx] = scores[bestIdx], scores[i]
        end
    end
end

local function quiescence(board: boardMod.Board, alpha: number, beta: number, depth: number): number
    nodesSearched += 1
    local standPat = eval.evaluate(board)
    if standPat >= beta then return beta end
    if depth <= -6 then return standPat end
    if standPat > alpha then alpha = standPat end

    local moves = movegen.generateLegalMoves(board)
    for _, move in moves do
        if move.capture == boardMod.EMPTY then continue end

        local newBoard = movegen.makeMove(board, move)
        local score = -quiescence(newBoard, -beta, -alpha, depth - 1)

        if score >= beta then return beta end
        if score > alpha then alpha = score end
    end

    return alpha
end

local function alphaBeta(board: boardMod.Board, depth: number, alpha: number, beta: number, ply: number): number
    if depth <= 0 then
        return quiescence(board, alpha, beta, 0)
    end

    nodesSearched += 1

    local ttScore, ttFrom, ttTo = ttProbe(board.zobrist, depth, alpha, beta)
    if ttScore then return ttScore end

    local moves = movegen.generateLegalMoves(board)
    if #moves == 0 then
        if movegen.isInCheck(board) then
            return -(MATE_SCORE - ply)
        end
        return 0
    end

    orderMoves(moves, ttFrom, ttTo)

    local bestFrom = moves[1].from
    local bestTo = moves[1].to
    local ttFlag = TT_ALPHA
    local bestScore = -INFINITY

    for _, move in moves do
        local newBoard = movegen.makeMove(board, move)
        local score = -alphaBeta(newBoard, depth - 1, -beta, -alpha, ply + 1)

        if score > bestScore then
            bestScore = score
            bestFrom = move.from
            bestTo = move.to
        end

        if score >= beta then
            ttStore(board.zobrist, depth, beta, TT_BETA, move.from, move.to)
            return beta
        end
        if score > alpha then
            alpha = score
            ttFlag = TT_EXACT
        end
    end

    ttStore(board.zobrist, depth, alpha, ttFlag, bestFrom, bestTo)
    return alpha
end

local function search(board: boardMod.Board, maxDepth: number): (movegen.Move?, number, number)
    nodesSearched = 0
    local bestMove: movegen.Move? = nil
    local bestScore = -INFINITY

    for depth = 1, maxDepth do
        local moves = movegen.generateLegalMoves(board)
        if #moves == 0 then break end

        local _, ttFrom, ttTo = ttProbe(board.zobrist, 0, -INFINITY, INFINITY)
        orderMoves(moves, ttFrom, ttTo)

        local alpha = -INFINITY
        local beta = INFINITY
        local currentBest: movegen.Move? = nil
        local currentScore = -INFINITY

        for _, move in moves do
            local newBoard = movegen.makeMove(board, move)
            local score = -alphaBeta(newBoard, depth - 1, -beta, -alpha, 1)

            if score > currentScore then
                currentScore = score
                currentBest = move
            end
            if score > alpha then
                alpha = score
            end
        end

        if currentBest then
            bestMove = currentBest
            bestScore = currentScore
        end
    end

    return bestMove, bestScore, nodesSearched
end

local function clearTT()
    for i = 1, ttSize do
        tt[i] = nil
    end
end

return {
    search = search,
    clearTT = clearTT,
}
