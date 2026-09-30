-- LZ77 match finding with hash-chained sliding window

local WINDOW_SIZE = 32768
local MAX_MATCH = 258
local MIN_MATCH = 3
local HASH_BITS = 15
local HASH_SIZE = bit32.lshift(1, HASH_BITS)
local HASH_MASK = HASH_SIZE - 1

export type Match = {
    length: number,
    distance: number,
}

export type LZ77Token = {
    literal: number?,
    match: Match?,
}

local function hashFunc(data: {number}, pos: number): number
    if pos + 2 > #data then return 0 end
    local h = bit32.bxor(data[pos], bit32.lshift(data[pos + 1], 5), bit32.lshift(data[pos + 2], 10))
    return bit32.band(h, HASH_MASK)
end

local function findMatch(data: {number}, pos: number, head: {number}, prev: {number}, maxChain: number): Match?
    local bestLen = MIN_MATCH - 1
    local bestDist = 0
    local dataLen = #data
    local maxLen = math.min(MAX_MATCH, dataLen - pos + 1)

    if maxLen < MIN_MATCH then return nil end

    local hash = hashFunc(data, pos)
    local chainPos = head[hash + 1]
    local chainCount = 0
    local windowStart = math.max(1, pos - WINDOW_SIZE)

    while chainPos >= windowStart and chainCount < maxChain do
        chainCount += 1
        if data[chainPos + bestLen] == data[pos + bestLen] then
            local len = 0
            while len < maxLen and data[chainPos + len] == data[pos + len] do
                len += 1
            end
            if len > bestLen then
                bestLen = len
                bestDist = pos - chainPos
                if len == maxLen then break end
            end
        end
        local prevIdx = ((chainPos - 1) % WINDOW_SIZE) + 1
        chainPos = prev[prevIdx]
        if chainPos < windowStart then break end
    end

    if bestLen >= MIN_MATCH then
        return {length = bestLen, distance = bestDist}
    end
    return nil
end

local function compress(data: {number}): {LZ77Token}
    local tokens: {LZ77Token} = {}
    local head = table.create(HASH_SIZE, 0)
    local prev = table.create(WINDOW_SIZE, 0)
    local pos = 1
    local dataLen = #data

    while pos <= dataLen do
        local match: Match? = nil
        if pos + MIN_MATCH - 1 <= dataLen then
            match = findMatch(data, pos, head, prev, 64)
        end

        if match then
            table.insert(tokens, {literal = nil, match = match})
            for i = 0, match.length - 1 do
                if pos + i + 2 <= dataLen then
                    local hash = hashFunc(data, pos + i)
                    local idx = ((pos + i - 1) % WINDOW_SIZE) + 1
                    prev[idx] = head[hash + 1]
                    head[hash + 1] = pos + i
                end
            end
            pos += match.length
        else
            table.insert(tokens, {literal = data[pos], match = nil})
            if pos + 2 <= dataLen then
                local hash = hashFunc(data, pos)
                local idx = ((pos - 1) % WINDOW_SIZE) + 1
                prev[idx] = head[hash + 1]
                head[hash + 1] = pos
            end
            pos += 1
        end
    end

    return tokens
end

return {
    compress = compress,
    MIN_MATCH = MIN_MATCH,
    MAX_MATCH = MAX_MATCH,
    WINDOW_SIZE = WINDOW_SIZE,
}
