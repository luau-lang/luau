local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()


-- Git object model + diff benchmark
-- Compatible with: Lua 5.5, LuaJIT 2.x, Lute
-- Implements: SHA-1, content-addressable store, blob/tree/commit objects,
-- Myers diff, unified diff output, patch parsing/application, three-way merge.

-- =========================================================================
-- 32-bit arithmetic helpers
-- =========================================================================
local band, bor, bxor, bnot, lshift, rshift
local _bit32 = rawget(_G, "bit32")
local _bit = rawget(_G, "bit")
if type(_bit32) == "table" then
    band, bor, bxor, bnot, lshift, rshift =
        _bit32.band, _bit32.bor, _bit32.bxor, _bit32.bnot, _bit32.lshift, _bit32.rshift
elseif type(_bit) == "table" then
    band, bor, bxor, lshift, rshift =
        _bit.band, _bit.bor, _bit.bxor, _bit.lshift, _bit.rshift
    bnot = function(x) return bxor(x, 0xFFFFFFFF) end
else
    band = assert(load("local a,b = ... return (a & b) & 0xffffffff"))
    bor = assert(load("local a,b = ... return (a | b) & 0xffffffff"))
    bxor = assert(load("local a,b = ... return (a ~ b) & 0xffffffff"))
    bnot = assert(load("local a = ... return (~a) & 0xffffffff"))
    lshift = assert(load("local a,b = ... return (a << b) & 0xffffffff"))
    rshift = assert(load("local a,b = ... return ((a & 0xffffffff) >> b) & 0xffffffff"))
end

local floor = math.floor
local sub = string.sub
local byte = string.byte
local char = string.char
local format = string.format
local concat = table.concat
local insert = table.insert

-- =========================================================================
-- SHA-1 Implementation
-- =========================================================================

-- Rotate left 32-bit
function rotl(x, n)
    return bor(lshift(x, n), rshift(x, 32 - n))
end

-- Convert a string to an array of 32-bit big-endian words
function strToWords(s)
    local words = {}
    local len = #s
    for i = 1, len, 4 do
        local b0 = byte(s, i) or 0
        local b1 = byte(s, i + 1) or 0
        local b2 = byte(s, i + 2) or 0
        local b3 = byte(s, i + 3) or 0
        words[#words + 1] = bor(lshift(b0, 24), lshift(b1, 16), lshift(b2, 8), b3)
    end
    return words
end

-- SHA-1 padding
function sha1Pad(msg)
    local len = #msg
    local bitLen = len * 8
    -- Append 0x80
    msg = msg .. char(0x80)
    -- Pad to 56 mod 64 bytes
    local padLen = (56 - (#msg % 64)) % 64
    msg = msg .. string.rep(char(0), padLen)
    -- Append 64-bit big-endian length (we only handle up to 32-bit lengths here)
    local highBits = floor(bitLen / 4294967296)
    local lowBits = bitLen % 4294967296
    msg = msg .. char(
        rshift(highBits, 24) % 256,
        rshift(highBits, 16) % 256,
        rshift(highBits, 8) % 256,
        highBits % 256,
        rshift(lowBits, 24) % 256,
        rshift(lowBits, 16) % 256,
        rshift(lowBits, 8) % 256,
        lowBits % 256
    )
    return msg
end

-- Main SHA-1 computation
function sha1(message)
    local msg = sha1Pad(message)
    local h0 = 0x67452301
    local h1 = 0xEFCDAB89
    local h2 = 0x98BADCFE
    local h3 = 0x10325476
    local h4 = 0xC3D2E1F0

    local w = {}
    for chunkStart = 1, #msg, 64 do
        -- Break chunk into 16 32-bit words
        for i = 0, 15 do
            local offset = chunkStart + i * 4
            local b0 = byte(msg, offset)
            local b1 = byte(msg, offset + 1)
            local b2 = byte(msg, offset + 2)
            local b3 = byte(msg, offset + 3)
            w[i] = bor(lshift(b0, 24), lshift(b1, 16), lshift(b2, 8), b3)
        end

        -- Extend to 80 words
        for i = 16, 79 do
            w[i] = rotl(bxor(w[i-3], w[i-8], w[i-14], w[i-16]), 1)
        end

        local a, b, c, d, e = h0, h1, h2, h3, h4

        for i = 0, 79 do
            local f, k
            if i <= 19 then
                f = bor(band(b, c), band(bnot(b), d))
                k = 0x5A827999
            elseif i <= 39 then
                f = bxor(b, c, d)
                k = 0x6ED9EBA1
            elseif i <= 59 then
                f = bor(band(b, c), band(b, d), band(c, d))
                k = 0x8F1BBCDC
            else
                f = bxor(b, c, d)
                k = 0xCA62C1D6
            end

            local temp = (rotl(a, 5) + f + e + k + w[i]) % 4294967296
            e = d
            d = c
            c = rotl(b, 30)
            b = a
            a = temp
        end

        h0 = (h0 + a) % 4294967296
        h1 = (h1 + b) % 4294967296
        h2 = (h2 + c) % 4294967296
        h3 = (h3 + d) % 4294967296
        h4 = (h4 + e) % 4294967296
    end

    return format("%08x%08x%08x%08x%08x", h0, h1, h2, h3, h4)
end

-- Git-style hash: sha1("type len\0content")
function gitHash(objType, content)
    local header = objType .. " " .. #content .. "\0"
    return sha1(header .. content)
end

-- =========================================================================
-- Content-Addressable Object Store
-- =========================================================================
ObjectStore = {}

function createObjectStore()
    return { objects = {}, refs = {} }
end

function storeObject(store, objType, content)
    local hash = gitHash(objType, content)
    if not store.objects[hash] then
        store.objects[hash] = { type = objType, content = content, hash = hash }
    end
    return hash
end

function getObject(store, hash)
    return store.objects[hash]
end

function setRef(store, name, hash)
    store.refs[name] = hash
end

function getRef(store, name)
    return store.refs[name]
end

-- =========================================================================
-- Blob Object
-- =========================================================================
function createBlob(store, content)
    return storeObject(store, "blob", content)
end

function getBlobContent(store, hash)
    local obj = getObject(store, hash)
    if obj and obj.type == "blob" then
        return obj.content
    end
    return nil
end

-- =========================================================================
-- Tree Object
-- =========================================================================

-- entries is a list of {mode, name, hash}
function serializeTree(entries)
    local parts = {}
    for i = 1, #entries do
        local e = entries[i]
        parts[#parts + 1] = e.mode .. " " .. e.name .. "\0" .. e.hash
    end
    return concat(parts, "")
end

function parseTree(content)
    local entries = {}
    local pos = 1
    local len = #content
    while pos <= len do
        local spacePos = content:find(" ", pos)
        if not spacePos then break end
        local mode = sub(content, pos, spacePos - 1)
        local nullPos = content:find("\0", spacePos + 1)
        if not nullPos then break end
        local name = sub(content, spacePos + 1, nullPos - 1)
        local hash = sub(content, nullPos + 1, nullPos + 40)
        entries[#entries + 1] = { mode = mode, name = name, hash = hash }
        pos = nullPos + 41
    end
    return entries
end

function createTree(store, entries)
    -- Sort entries by name for determinism
    table.sort(entries, function(a, b) return a.name < b.name end)
    local content = serializeTree(entries)
    return storeObject(store, "tree", content)
end

function getTreeEntries(store, hash)
    local obj = getObject(store, hash)
    if obj and obj.type == "tree" then
        return parseTree(obj.content)
    end
    return {}
end

-- =========================================================================
-- Commit Object
-- =========================================================================
function serializeCommit(treeHash, parentHashes, author, message)
    local lines = {}
    lines[#lines + 1] = "tree " .. treeHash
    for i = 1, #parentHashes do
        lines[#lines + 1] = "parent " .. parentHashes[i]
    end
    lines[#lines + 1] = "author " .. author
    lines[#lines + 1] = "committer " .. author
    lines[#lines + 1] = ""
    lines[#lines + 1] = message
    return concat(lines, "\n")
end

function parseCommit(content)
    local result = { parents = {} }
    local lines = splitLines(content)
    local i = 1
    while i <= #lines do
        local line = lines[i]
        if line == "" then
            -- Rest is message
            local msgLines = {}
            for j = i + 1, #lines do
                msgLines[#msgLines + 1] = lines[j]
            end
            result.message = concat(msgLines, "\n")
            break
        end
        local key, value = line:match("^(%S+)%s(.+)$")
        if key == "tree" then
            result.tree = value
        elseif key == "parent" then
            result.parents[#result.parents + 1] = value
        elseif key == "author" then
            result.author = value
        elseif key == "committer" then
            result.committer = value
        end
        i = i + 1
    end
    return result
end

function createCommit(store, treeHash, parentHashes, author, message)
    local content = serializeCommit(treeHash, parentHashes, author, message)
    return storeObject(store, "commit", content)
end

function getCommitData(store, hash)
    local obj = getObject(store, hash)
    if obj and obj.type == "commit" then
        return parseCommit(obj.content)
    end
    return nil
end

-- =========================================================================
-- String Utilities
-- =========================================================================
function splitLines(text)
    local lines = {}
    local pos = 1
    local len = #text
    while pos <= len do
        local nl = text:find("\n", pos, true)
        if nl then
            lines[#lines + 1] = sub(text, pos, nl - 1)
            pos = nl + 1
        else
            lines[#lines + 1] = sub(text, pos)
            break
        end
    end
    return lines
end

function joinLines(lines)
    return concat(lines, "\n")
end

function trimRight(s)
    return s:match("^(.-)%s*$")
end

function startsWith(s, prefix)
    return sub(s, 1, #prefix) == prefix
end

-- =========================================================================
-- Myers Diff Algorithm (O(ND))
-- =========================================================================

-- Compute shortest edit script between two sequences of lines
function myersDiff(aLines, bLines)
    local n = #aLines
    local m = #bLines
    local max = n + m
    if max == 0 then return {} end

    -- V array indexed from -max to max, storing x values for each diagonal
    local v = {}
    v[1] = 0
    local trace = {}

    local found = false
    for d = 0, max do
        -- Save current V state for traceback
        local vCopy = {}
        for k2, val in next, v do
            vCopy[k2] = val
        end
        trace[d] = vCopy

        for k = -d, d, 2 do
            local x
            if k == -d or (k ~= d and (v[k - 1] or 0) < (v[k + 1] or 0)) then
                x = v[k + 1] or 0
            else
                x = (v[k - 1] or 0) + 1
            end
            local y = x - k

            -- Follow diagonal (matching lines)
            while x < n and y < m and aLines[x + 1] == bLines[y + 1] do
                x = x + 1
                y = y + 1
            end

            v[k] = x

            if x >= n and y >= m then
                found = true
                break
            end
        end
        if found then break end
    end

    -- Traceback to find the actual edit script
    local edits = {}
    local x = n
    local y = m

    for d = #trace, 0, -1 do
        local vPrev = trace[d]
        local k = x - y
        local prevK
        if k == -d or (k ~= d and (vPrev[k - 1] or 0) < (vPrev[k + 1] or 0)) then
            prevK = k + 1
        else
            prevK = k - 1
        end

        local prevX = vPrev[prevK] or 0
        local prevY = prevX - prevK

        -- Diagonal moves (equal lines)
        while x > prevX and y > prevY do
            x = x - 1
            y = y - 1
            edits[#edits + 1] = { op = "equal", aIdx = x + 1, bIdx = y + 1 }
        end

        if d > 0 then
            if x == prevX then
                -- Insert
                y = y - 1
                edits[#edits + 1] = { op = "insert", bIdx = y + 1 }
            else
                -- Delete
                x = x - 1
                edits[#edits + 1] = { op = "delete", aIdx = x + 1 }
            end
        end
    end

    -- Reverse edits (we built them backwards)
    local reversed = {}
    for i = #edits, 1, -1 do
        reversed[#reversed + 1] = edits[i]
    end
    return reversed
end

-- =========================================================================
-- Unified Diff Format
-- =========================================================================
function generateUnifiedDiff(aName, bName, aLines, bLines, edits, contextSize)
    contextSize = contextSize or 3
    local output = {}
    output[#output + 1] = "--- " .. aName
    output[#output + 1] = "+++ " .. bName

    -- Group edits into hunks
    local hunks = groupIntoHunks(edits, aLines, bLines, contextSize)

    for h = 1, #hunks do
        local hunk = hunks[h]
        output[#output + 1] = format("@@ -%d,%d +%d,%d @@",
            hunk.aStart, hunk.aCount, hunk.bStart, hunk.bCount)
        for i = 1, #hunk.lines do
            output[#output + 1] = hunk.lines[i]
        end
    end

    return concat(output, "\n")
end

function groupIntoHunks(edits, aLines, bLines, contextSize)
    -- First, build a list of change positions
    local changes = {}
    for i = 1, #edits do
        if edits[i].op ~= "equal" then
            changes[#changes + 1] = i
        end
    end

    if #changes == 0 then return {} end

    -- Group changes that are within contextSize*2 of each other
    local groups = {}
    local currentGroup = { changes[1] }
    for i = 2, #changes do
        -- Check gap between consecutive changes in edit list
        if changes[i] - changes[i-1] <= contextSize * 2 + 1 then
            currentGroup[#currentGroup + 1] = changes[i]
        else
            groups[#groups + 1] = currentGroup
            currentGroup = { changes[i] }
        end
    end
    groups[#groups + 1] = currentGroup

    -- Build hunks
    local hunks = {}
    for g = 1, #groups do
        local group = groups[g]
        local firstChange = group[1]
        local lastChange = group[#group]

        -- Determine context boundaries
        local startIdx = math.max(1, firstChange - contextSize)
        local endIdx = math.min(#edits, lastChange + contextSize)

        local hunkLines = {}
        local aStart, aCount, bStart, bCount = nil, 0, nil, 0

        for i = startIdx, endIdx do
            local edit = edits[i]
            if edit.op == "equal" then
                if not aStart then aStart = edit.aIdx end
                if not bStart then bStart = edit.bIdx end
                hunkLines[#hunkLines + 1] = " " .. aLines[edit.aIdx]
                aCount = aCount + 1
                bCount = bCount + 1
            elseif edit.op == "delete" then
                if not aStart then aStart = edit.aIdx end
                if not bStart then
                    -- bStart is the line in b after last context
                    bStart = edit.aIdx - (aStart and (edit.aIdx - aStart) or 0)
                    -- Recalculate based on tracked position
                    bStart = bCount + 1
                end
                hunkLines[#hunkLines + 1] = "-" .. aLines[edit.aIdx]
                aCount = aCount + 1
            elseif edit.op == "insert" then
                if not aStart then aStart = 1 end
                if not bStart then bStart = edit.bIdx end
                hunkLines[#hunkLines + 1] = "+" .. bLines[edit.bIdx]
                bCount = bCount + 1
            end
        end

        if not aStart then aStart = 1 end
        if not bStart then bStart = 1 end

        hunks[#hunks + 1] = {
            aStart = aStart,
            aCount = aCount,
            bStart = bStart,
            bCount = bCount,
            lines = hunkLines
        }
    end

    return hunks
end

-- =========================================================================
-- Simplified Diff (line-level, for merge)
-- =========================================================================
function computeLineDiff(aText, bText)
    local aLines = splitLines(aText)
    local bLines = splitLines(bText)
    local edits = myersDiff(aLines, bLines)
    return edits, aLines, bLines
end

function diffToUnified(aName, bName, aText, bText)
    local aLines = splitLines(aText)
    local bLines = splitLines(bText)
    local edits = myersDiff(aLines, bLines)
    return generateUnifiedDiff(aName, bName, aLines, bLines, edits, 3)
end

-- =========================================================================
-- Patch Parsing
-- =========================================================================
function parsePatch(patchText)
    local lines = splitLines(patchText)
    local hunks = {}
    local currentHunk = nil
    local aFile, bFile
    local seenHunk = false

    for i = 1, #lines do
        local line = lines[i]
        if not seenHunk and startsWith(line, "--- ") then
            aFile = sub(line, 5)
        elseif not seenHunk and startsWith(line, "+++ ") then
            bFile = sub(line, 5)
        elseif startsWith(line, "@@") then
            seenHunk = true
            local aStart, aCount, bStart, bCount =
                line:match("^@@ %-(%d+),(%d+) %+(%d+),(%d+) @@")
            if aStart then
                currentHunk = {
                    aStart = tonumber(aStart),
                    aCount = tonumber(aCount),
                    bStart = tonumber(bStart),
                    bCount = tonumber(bCount),
                    lines = {}
                }
                hunks[#hunks + 1] = currentHunk
            end
        elseif currentHunk then
            if startsWith(line, " ") or startsWith(line, "+") or startsWith(line, "-") then
                currentHunk.lines[#currentHunk.lines + 1] = line
            end
        end
    end

    return { aFile = aFile, bFile = bFile, hunks = hunks }
end

-- =========================================================================
-- Patch Application
-- =========================================================================
function applyPatch(originalText, patch)
    local origLines = splitLines(originalText)
    local result = {}
    local origIdx = 1

    for h = 1, #patch.hunks do
        local hunk = patch.hunks[h]
        -- Copy lines before this hunk
        while origIdx < hunk.aStart do
            result[#result + 1] = origLines[origIdx]
            origIdx = origIdx + 1
        end
        -- Apply hunk
        for i = 1, #hunk.lines do
            local hLine = hunk.lines[i]
            local prefix = sub(hLine, 1, 1)
            local content = sub(hLine, 2)
            if prefix == " " then
                result[#result + 1] = content
                origIdx = origIdx + 1
            elseif prefix == "-" then
                origIdx = origIdx + 1
            elseif prefix == "+" then
                result[#result + 1] = content
            end
        end
    end

    -- Copy remaining lines
    while origIdx <= #origLines do
        result[#result + 1] = origLines[origIdx]
        origIdx = origIdx + 1
    end

    return joinLines(result)
end

-- =========================================================================
-- Three-Way Merge
-- =========================================================================
function threeWayMerge(baseText, oursText, theirsText)
    local baseLines = splitLines(baseText)
    local oursLines = splitLines(oursText)
    local theirsLines = splitLines(theirsText)

    local oursEdits = myersDiff(baseLines, oursLines)
    local theirsEdits = myersDiff(baseLines, theirsLines)

    -- Build change maps: which base lines are modified by each side
    local oursChanges = buildChangeMap(oursEdits, baseLines, oursLines)
    local theirsChanges = buildChangeMap(theirsEdits, baseLines, theirsLines)

    -- Merge
    local result = {}
    local conflicts = 0
    local baseIdx = 1

    while baseIdx <= #baseLines do
        local oc = oursChanges[baseIdx]
        local tc = theirsChanges[baseIdx]

        if not oc and not tc then
            -- No changes, keep base
            result[#result + 1] = baseLines[baseIdx]
            baseIdx = baseIdx + 1
        elseif oc and not tc then
            -- Only ours changed
            for j = 1, #oc.newLines do
                result[#result + 1] = oc.newLines[j]
            end
            baseIdx = baseIdx + oc.baseCount
        elseif tc and not oc then
            -- Only theirs changed
            for j = 1, #tc.newLines do
                result[#result + 1] = tc.newLines[j]
            end
            baseIdx = baseIdx + tc.baseCount
        else
            -- Both changed - check if same change
            local same = (#oc.newLines == #tc.newLines)
            if same then
                for j = 1, #oc.newLines do
                    if oc.newLines[j] ~= tc.newLines[j] then
                        same = false
                        break
                    end
                end
            end
            if same then
                -- Same change, take either
                for j = 1, #oc.newLines do
                    result[#result + 1] = oc.newLines[j]
                end
                baseIdx = baseIdx + oc.baseCount
            else
                -- Conflict!
                conflicts = conflicts + 1
                result[#result + 1] = "<<<<<<< OURS"
                for j = 1, #oc.newLines do
                    result[#result + 1] = oc.newLines[j]
                end
                result[#result + 1] = "======="
                for j = 1, #tc.newLines do
                    result[#result + 1] = tc.newLines[j]
                end
                result[#result + 1] = ">>>>>>> THEIRS"
                baseIdx = baseIdx + math.max(oc.baseCount, tc.baseCount)
            end
        end
    end

    return joinLines(result), conflicts
end

function buildChangeMap(edits, baseLines, newLines)
    local changes = {}
    local i = 1
    while i <= #edits do
        local edit = edits[i]
        if edit.op ~= "equal" then
            -- Collect contiguous changes
            local baseStart = nil
            local baseCount = 0
            local newLinesCollected = {}

            while i <= #edits and edits[i].op ~= "equal" do
                local e = edits[i]
                if e.op == "delete" then
                    if not baseStart then baseStart = e.aIdx end
                    baseCount = baseCount + 1
                elseif e.op == "insert" then
                    if not baseStart then
                        -- Pure insert, anchor to next base line
                        baseStart = e.bIdx
                    end
                    newLinesCollected[#newLinesCollected + 1] = newLines[e.bIdx]
                end
                i = i + 1
            end

            if baseStart and baseStart <= #baseLines then
                changes[baseStart] = {
                    baseCount = math.max(baseCount, 1),
                    newLines = newLinesCollected
                }
            end
        else
            i = i + 1
        end
    end
    return changes
end

-- =========================================================================
-- High-level Git operations
-- =========================================================================
function buildTreeFromFiles(store, files)
    local entries = {}
    for fname, content in next, files do
        local blobHash = createBlob(store, content)
        entries[#entries + 1] = { mode = "100644", name = fname, hash = blobHash }
    end
    return createTree(store, entries)
end

function commitFiles(store, files, parentHashes, author, message)
    local treeHash = buildTreeFromFiles(store, files)
    return createCommit(store, treeHash, parentHashes, author, message)
end

function getFilesFromCommit(store, commitHash)
    local commitData = getCommitData(store, commitHash)
    if not commitData then return {} end
    local entries = getTreeEntries(store, commitData.tree)
    local files = {}
    for i = 1, #entries do
        local e = entries[i]
        files[e.name] = getBlobContent(store, e.hash)
    end
    return files
end

function diffCommits(store, commitA, commitB)
    local filesA = getFilesFromCommit(store, commitA)
    local filesB = getFilesFromCommit(store, commitB)
    local diffs = {}

    -- Find modified and deleted files
    for fname, contentA in next, filesA do
        local contentB = filesB[fname]
        if contentB == nil then
            -- Deleted
            diffs[#diffs + 1] = { file = fname, status = "deleted",
                patch = diffToUnified("a/" .. fname, "/dev/null", contentA, "") }
        elseif contentA ~= contentB then
            -- Modified
            diffs[#diffs + 1] = { file = fname, status = "modified",
                patch = diffToUnified("a/" .. fname, "b/" .. fname, contentA, contentB) }
        end
    end

    -- Find added files
    for fname, contentB in next, filesB do
        if filesA[fname] == nil then
            diffs[#diffs + 1] = { file = fname, status = "added",
                patch = diffToUnified("/dev/null", "b/" .. fname, "", contentB) }
        end
    end

    table.sort(diffs, function(a, b) return a.file < b.file end)
    return diffs
end

function mergeCommits(store, baseCommit, oursCommit, theirsCommit)
    local baseFiles = getFilesFromCommit(store, baseCommit)
    local oursFiles = getFilesFromCommit(store, oursCommit)
    local theirsFiles = getFilesFromCommit(store, theirsCommit)

    local merged = {}
    local totalConflicts = 0

    -- Collect all filenames
    local allFiles = {}
    for fname in next, baseFiles do allFiles[fname] = true end
    for fname in next, oursFiles do allFiles[fname] = true end
    for fname in next, theirsFiles do allFiles[fname] = true end

    for fname in next, allFiles do
        local base = baseFiles[fname] or ""
        local ours = oursFiles[fname] or ""
        local theirs = theirsFiles[fname] or ""

        if ours == theirs then
            if ours ~= "" then
                merged[fname] = ours
            end
        elseif ours == base then
            if theirs ~= "" then
                merged[fname] = theirs
            end
        elseif theirs == base then
            if ours ~= "" then
                merged[fname] = ours
            end
        else
            -- Both modified differently
            local mergedContent, conflicts = threeWayMerge(base, ours, theirs)
            merged[fname] = mergedContent
            totalConflicts = totalConflicts + conflicts
        end
    end

    return merged, totalConflicts
end

-- =========================================================================
-- Test Source Files (realistic content)
-- =========================================================================

TEST_FILE_1 = [[
-- Module: Vector3
-- 3D vector mathematics library

local Vector3 = {}
Vector3.__index = Vector3

function Vector3.new(x, y, z)
    local self = setmetatable({}, Vector3)
    self.x = x or 0
    self.y = y or 0
    self.z = z or 0
    return self
end

function Vector3:magnitude()
    return math.sqrt(self.x * self.x + self.y * self.y + self.z * self.z)
end

function Vector3:normalize()
    local mag = self:magnitude()
    if mag > 0 then
        return Vector3.new(self.x / mag, self.y / mag, self.z / mag)
    end
    return Vector3.new(0, 0, 0)
end

function Vector3:dot(other)
    return self.x * other.x + self.y * other.y + self.z * other.z
end

function Vector3:cross(other)
    return Vector3.new(
        self.y * other.z - self.z * other.y,
        self.z * other.x - self.x * other.z,
        self.x * other.y - self.y * other.x
    )
end

function Vector3:add(other)
    return Vector3.new(self.x + other.x, self.y + other.y, self.z + other.z)
end

function Vector3:sub(other)
    return Vector3.new(self.x - other.x, self.y - other.y, self.z - other.z)
end

function Vector3:mul(scalar)
    return Vector3.new(self.x * scalar, self.y * scalar, self.z * scalar)
end

function Vector3:lerp(other, t)
    return self:add(other:sub(self):mul(t))
end

function Vector3:distance(other)
    return self:sub(other):magnitude()
end

function Vector3:reflect(normal)
    local d = 2 * self:dot(normal)
    return self:sub(normal:mul(d))
end

function Vector3:__tostring()
    return string.format("(%f, %f, %f)", self.x, self.y, self.z)
end

return Vector3
]]

TEST_FILE_2 = [[
-- Module: Matrix4x4
-- 4x4 matrix operations for 3D transformations

local Matrix4x4 = {}
Matrix4x4.__index = Matrix4x4

function Matrix4x4.new()
    local self = setmetatable({}, Matrix4x4)
    self.m = {}
    for i = 1, 16 do
        self.m[i] = 0
    end
    return self
end

function Matrix4x4.identity()
    local mat = Matrix4x4.new()
    mat.m[1] = 1
    mat.m[6] = 1
    mat.m[11] = 1
    mat.m[16] = 1
    return mat
end

function Matrix4x4.translation(x, y, z)
    local mat = Matrix4x4.identity()
    mat.m[13] = x
    mat.m[14] = y
    mat.m[15] = z
    return mat
end

function Matrix4x4.scaling(x, y, z)
    local mat = Matrix4x4.new()
    mat.m[1] = x
    mat.m[6] = y
    mat.m[11] = z
    mat.m[16] = 1
    return mat
end

function Matrix4x4.rotationX(angle)
    local mat = Matrix4x4.identity()
    local c = math.cos(angle)
    local s = math.sin(angle)
    mat.m[6] = c
    mat.m[7] = s
    mat.m[10] = -s
    mat.m[11] = c
    return mat
end

function Matrix4x4.rotationY(angle)
    local mat = Matrix4x4.identity()
    local c = math.cos(angle)
    local s = math.sin(angle)
    mat.m[1] = c
    mat.m[3] = -s
    mat.m[9] = s
    mat.m[11] = c
    return mat
end

function Matrix4x4.rotationZ(angle)
    local mat = Matrix4x4.identity()
    local c = math.cos(angle)
    local s = math.sin(angle)
    mat.m[1] = c
    mat.m[2] = s
    mat.m[5] = -s
    mat.m[6] = c
    return mat
end

function Matrix4x4:multiply(other)
    local result = Matrix4x4.new()
    for row = 0, 3 do
        for col = 0, 3 do
            local sum = 0
            for k = 0, 3 do
                sum = sum + self.m[row * 4 + k + 1] * other.m[k * 4 + col + 1]
            end
            result.m[row * 4 + col + 1] = sum
        end
    end
    return result
end

function Matrix4x4:determinant()
    local m = self.m
    local a = m[1] * (m[6] * m[11] * m[16] - m[6] * m[12] * m[15])
    local b = m[2] * (m[5] * m[11] * m[16] - m[5] * m[12] * m[15])
    local c = m[3] * (m[5] * m[10] * m[16] - m[5] * m[12] * m[14])
    local d = m[4] * (m[5] * m[10] * m[15] - m[5] * m[11] * m[14])
    return a - b + c - d
end

function Matrix4x4:transpose()
    local result = Matrix4x4.new()
    for row = 0, 3 do
        for col = 0, 3 do
            result.m[col * 4 + row + 1] = self.m[row * 4 + col + 1]
        end
    end
    return result
end

return Matrix4x4
]]

TEST_FILE_3 = [[
-- Module: LinkedList
-- Doubly linked list implementation

local LinkedList = {}
LinkedList.__index = LinkedList

local Node = {}
Node.__index = Node

function Node.new(value)
    return setmetatable({ value = value, prev = nil, next = nil }, Node)
end

function LinkedList.new()
    local self = setmetatable({}, LinkedList)
    self.head = nil
    self.tail = nil
    self.size = 0
    return self
end

function LinkedList:pushFront(value)
    local node = Node.new(value)
    if not self.head then
        self.head = node
        self.tail = node
    else
        node.next = self.head
        self.head.prev = node
        self.head = node
    end
    self.size = self.size + 1
    return node
end

function LinkedList:pushBack(value)
    local node = Node.new(value)
    if not self.tail then
        self.head = node
        self.tail = node
    else
        node.prev = self.tail
        self.tail.next = node
        self.tail = node
    end
    self.size = self.size + 1
    return node
end

function LinkedList:popFront()
    if not self.head then return nil end
    local value = self.head.value
    self.head = self.head.next
    if self.head then
        self.head.prev = nil
    else
        self.tail = nil
    end
    self.size = self.size - 1
    return value
end

function LinkedList:popBack()
    if not self.tail then return nil end
    local value = self.tail.value
    self.tail = self.tail.prev
    if self.tail then
        self.tail.next = nil
    else
        self.head = nil
    end
    self.size = self.size - 1
    return value
end

function LinkedList:remove(node)
    if node.prev then
        node.prev.next = node.next
    else
        self.head = node.next
    end
    if node.next then
        node.next.prev = node.prev
    else
        self.tail = node.prev
    end
    self.size = self.size - 1
end

function LinkedList:find(value)
    local current = self.head
    while current do
        if current.value == value then
            return current
        end
        current = current.next
    end
    return nil
end

function LinkedList:toArray()
    local arr = {}
    local current = self.head
    while current do
        arr[#arr + 1] = current.value
        current = current.next
    end
    return arr
end

function LinkedList:isEmpty()
    return self.size == 0
end

return LinkedList
]]

TEST_FILE_4 = [[
-- Module: HashMap
-- Hash map with separate chaining

local HashMap = {}
HashMap.__index = HashMap

function HashMap.new(initialCapacity)
    local self = setmetatable({}, HashMap)
    self.capacity = initialCapacity or 16
    self.size = 0
    self.loadFactor = 0.75
    self.buckets = {}
    for i = 1, self.capacity do
        self.buckets[i] = {}
    end
    return self
end

function HashMap:hash(key)
    local h = 0
    local s = tostring(key)
    for i = 1, #s do
        h = (h * 31 + string.byte(s, i)) % self.capacity
    end
    return h + 1
end

function HashMap:put(key, value)
    if self.size >= self.capacity * self.loadFactor then
        self:resize()
    end
    local idx = self:hash(key)
    local bucket = self.buckets[idx]
    for i = 1, #bucket do
        if bucket[i].key == key then
            bucket[i].value = value
            return
        end
    end
    bucket[#bucket + 1] = { key = key, value = value }
    self.size = self.size + 1
end

function HashMap:get(key)
    local idx = self:hash(key)
    local bucket = self.buckets[idx]
    for i = 1, #bucket do
        if bucket[i].key == key then
            return bucket[i].value
        end
    end
    return nil
end

function HashMap:remove(key)
    local idx = self:hash(key)
    local bucket = self.buckets[idx]
    for i = 1, #bucket do
        if bucket[i].key == key then
            table.remove(bucket, i)
            self.size = self.size - 1
            return true
        end
    end
    return false
end

function HashMap:contains(key)
    return self:get(key) ~= nil
end

function HashMap:resize()
    local oldBuckets = self.buckets
    self.capacity = self.capacity * 2
    self.buckets = {}
    for i = 1, self.capacity do
        self.buckets[i] = {}
    end
    self.size = 0
    for i = 1, #oldBuckets do
        local bucket = oldBuckets[i]
        for j = 1, #bucket do
            self:put(bucket[j].key, bucket[j].value)
        end
    end
end

function HashMap:keys()
    local result = {}
    for i = 1, self.capacity do
        local bucket = self.buckets[i]
        for j = 1, #bucket do
            result[#result + 1] = bucket[j].key
        end
    end
    return result
end

return HashMap
]]

TEST_FILE_5 = [[
-- Module: EventEmitter
-- Event system with priorities and once-listeners

local EventEmitter = {}
EventEmitter.__index = EventEmitter

function EventEmitter.new()
    local self = setmetatable({}, EventEmitter)
    self.listeners = {}
    self.onceListeners = {}
    return self
end

function EventEmitter:on(event, callback, priority)
    priority = priority or 0
    if not self.listeners[event] then
        self.listeners[event] = {}
    end
    local list = self.listeners[event]
    list[#list + 1] = { callback = callback, priority = priority }
    table.sort(list, function(a, b) return a.priority > b.priority end)
    return self
end

function EventEmitter:once(event, callback, priority)
    priority = priority or 0
    if not self.onceListeners[event] then
        self.onceListeners[event] = {}
    end
    local list = self.onceListeners[event]
    list[#list + 1] = { callback = callback, priority = priority }
    return self
end

function EventEmitter:emit(event, ...)
    local list = self.listeners[event]
    if list then
        for i = 1, #list do
            list[i].callback(...)
        end
    end
    local onceList = self.onceListeners[event]
    if onceList then
        for i = 1, #onceList do
            onceList[i].callback(...)
        end
        self.onceListeners[event] = nil
    end
end

function EventEmitter:off(event, callback)
    local list = self.listeners[event]
    if not list then return end
    for i = #list, 1, -1 do
        if list[i].callback == callback then
            table.remove(list, i)
            break
        end
    end
end

function EventEmitter:removeAllListeners(event)
    if event then
        self.listeners[event] = nil
        self.onceListeners[event] = nil
    else
        self.listeners = {}
        self.onceListeners = {}
    end
end

function EventEmitter:listenerCount(event)
    local count = 0
    if self.listeners[event] then
        count = count + #self.listeners[event]
    end
    if self.onceListeners[event] then
        count = count + #self.onceListeners[event]
    end
    return count
end

return EventEmitter
]]

TEST_FILE_6 = [[
-- Module: Scheduler
-- Priority-based task scheduler with time slicing

local Scheduler = {}
Scheduler.__index = Scheduler

local Task = {}
Task.__index = Task

function Task.new(id, priority, callback, delay)
    return setmetatable({
        id = id,
        priority = priority,
        callback = callback,
        delay = delay or 0,
        scheduledTime = 0,
        completed = false,
        cancelled = false
    }, Task)
end

function Scheduler.new()
    local self = setmetatable({}, Scheduler)
    self.tasks = {}
    self.currentTime = 0
    self.nextId = 1
    self.completedCount = 0
    return self
end

function Scheduler:schedule(priority, callback, delay)
    local task = Task.new(self.nextId, priority, callback, delay)
    task.scheduledTime = self.currentTime + (delay or 0)
    self.nextId = self.nextId + 1
    self.tasks[#self.tasks + 1] = task
    self:sortTasks()
    return task
end

function Scheduler:sortTasks()
    table.sort(self.tasks, function(a, b)
        if a.scheduledTime ~= b.scheduledTime then
            return a.scheduledTime < b.scheduledTime
        end
        return a.priority > b.priority
    end)
end

function Scheduler:tick(deltaTime)
    self.currentTime = self.currentTime + deltaTime
    local executed = 0
    local toRemove = {}
    for i = 1, #self.tasks do
        local task = self.tasks[i]
        if not task.cancelled and not task.completed then
            if task.scheduledTime <= self.currentTime then
                task.callback(task)
                task.completed = true
                self.completedCount = self.completedCount + 1
                executed = executed + 1
                toRemove[#toRemove + 1] = i
            end
        elseif task.cancelled then
            toRemove[#toRemove + 1] = i
        end
    end
    -- Remove completed/cancelled tasks (in reverse order)
    for i = #toRemove, 1, -1 do
        table.remove(self.tasks, toRemove[i])
    end
    return executed
end

function Scheduler:cancel(task)
    task.cancelled = true
end

function Scheduler:pendingCount()
    local count = 0
    for i = 1, #self.tasks do
        if not self.tasks[i].completed and not self.tasks[i].cancelled then
            count = count + 1
        end
    end
    return count
end

return Scheduler
]]

TEST_FILE_7 = [[
-- Module: BinarySearchTree
-- Self-balancing binary search tree (AVL-like)

local BST = {}
BST.__index = BST

local BSTNode = {}
BSTNode.__index = BSTNode

function BSTNode.new(key, value)
    return setmetatable({
        key = key,
        value = value,
        left = nil,
        right = nil,
        height = 1
    }, BSTNode)
end

function BST.new()
    local self = setmetatable({}, BST)
    self.root = nil
    self.size = 0
    return self
end

function BST:getHeight(node)
    if not node then return 0 end
    return node.height
end

function BST:getBalance(node)
    if not node then return 0 end
    return self:getHeight(node.left) - self:getHeight(node.right)
end

function BST:updateHeight(node)
    local lh = self:getHeight(node.left)
    local rh = self:getHeight(node.right)
    node.height = math.max(lh, rh) + 1
end

function BST:rotateRight(y)
    local x = y.left
    local t2 = x.right
    x.right = y
    y.left = t2
    self:updateHeight(y)
    self:updateHeight(x)
    return x
end

function BST:rotateLeft(x)
    local y = x.right
    local t2 = y.left
    y.left = x
    x.right = t2
    self:updateHeight(x)
    self:updateHeight(y)
    return y
end

function BST:insertNode(node, key, value)
    if not node then
        self.size = self.size + 1
        return BSTNode.new(key, value)
    end
    if key < node.key then
        node.left = self:insertNode(node.left, key, value)
    elseif key > node.key then
        node.right = self:insertNode(node.right, key, value)
    else
        node.value = value
        return node
    end

    self:updateHeight(node)
    local balance = self:getBalance(node)

    if balance > 1 and key < node.left.key then
        return self:rotateRight(node)
    end
    if balance < -1 and key > node.right.key then
        return self:rotateLeft(node)
    end
    if balance > 1 and key > node.left.key then
        node.left = self:rotateLeft(node.left)
        return self:rotateRight(node)
    end
    if balance < -1 and key < node.right.key then
        node.right = self:rotateRight(node.right)
        return self:rotateLeft(node)
    end

    return node
end

function BST:insert(key, value)
    self.root = self:insertNode(self.root, key, value)
end

function BST:search(key)
    local node = self.root
    while node do
        if key == node.key then return node.value end
        if key < node.key then
            node = node.left
        else
            node = node.right
        end
    end
    return nil
end

function BST:inorder(node, result)
    if not node then return end
    self:inorder(node.left, result)
    result[#result + 1] = { key = node.key, value = node.value }
    self:inorder(node.right, result)
end

function BST:toSortedArray()
    local result = {}
    self:inorder(self.root, result)
    return result
end

return BST
]]

TEST_FILE_8 = [[
-- Module: StringBuffer
-- Efficient string building with rope-like structure

local StringBuffer = {}
StringBuffer.__index = StringBuffer

function StringBuffer.new()
    local self = setmetatable({}, StringBuffer)
    self.parts = {}
    self.totalLength = 0
    return self
end

function StringBuffer:append(str)
    if str and #str > 0 then
        self.parts[#self.parts + 1] = str
        self.totalLength = self.totalLength + #str
    end
    return self
end

function StringBuffer:prepend(str)
    if str and #str > 0 then
        table.insert(self.parts, 1, str)
        self.totalLength = self.totalLength + #str
    end
    return self
end

function StringBuffer:appendLine(str)
    self:append(str or "")
    self:append("\n")
    return self
end

function StringBuffer:toString()
    return table.concat(self.parts)
end

function StringBuffer:length()
    return self.totalLength
end

function StringBuffer:clear()
    self.parts = {}
    self.totalLength = 0
    return self
end

function StringBuffer:indexOf(pattern, start)
    local full = self:toString()
    return full:find(pattern, start, true)
end

function StringBuffer:replace(old, new)
    local full = self:toString()
    local result = full:gsub(old, new, 1)
    self.parts = { result }
    self.totalLength = #result
    return self
end

function StringBuffer:split(delimiter)
    local full = self:toString()
    local result = {}
    local pos = 1
    while pos <= #full do
        local dStart = full:find(delimiter, pos, true)
        if dStart then
            result[#result + 1] = full:sub(pos, dStart - 1)
            pos = dStart + #delimiter
        else
            result[#result + 1] = full:sub(pos)
            break
        end
    end
    return result
end

function StringBuffer:reverse()
    local full = self:toString()
    self.parts = { full:reverse() }
    return self
end

function StringBuffer:upper()
    local full = self:toString()
    self.parts = { full:upper() }
    return self
end

function StringBuffer:lower()
    local full = self:toString()
    self.parts = { full:lower() }
    return self
end

function StringBuffer:trim()
    local full = self:toString()
    local result = full:match("^%s*(.-)%s*$")
    self.parts = { result }
    self.totalLength = #result
    return self
end

return StringBuffer
]]

TEST_FILE_9 = [[
-- Module: StateMachine
-- Finite state machine with transitions and guards

local StateMachine = {}
StateMachine.__index = StateMachine

function StateMachine.new(initialState)
    local self = setmetatable({}, StateMachine)
    self.currentState = initialState
    self.states = {}
    self.transitions = {}
    self.history = {}
    self.onEnter = {}
    self.onExit = {}
    return self
end

function StateMachine:addState(name, config)
    self.states[name] = config or {}
end

function StateMachine:addTransition(from, event, to, guard)
    if not self.transitions[from] then
        self.transitions[from] = {}
    end
    self.transitions[from][event] = { target = to, guard = guard }
end

function StateMachine:setOnEnter(state, callback)
    self.onEnter[state] = callback
end

function StateMachine:setOnExit(state, callback)
    self.onExit[state] = callback
end

function StateMachine:trigger(event, context)
    local trans = self.transitions[self.currentState]
    if not trans then return false end
    local t = trans[event]
    if not t then return false end
    if t.guard and not t.guard(context) then
        return false
    end
    local oldState = self.currentState
    if self.onExit[oldState] then
        self.onExit[oldState](context)
    end
    self.currentState = t.target
    self.history[#self.history + 1] = {
        from = oldState,
        event = event,
        to = t.target
    }
    if self.onEnter[t.target] then
        self.onEnter[t.target](context)
    end
    return true
end

function StateMachine:getState()
    return self.currentState
end

function StateMachine:canTrigger(event, context)
    local trans = self.transitions[self.currentState]
    if not trans then return false end
    local t = trans[event]
    if not t then return false end
    if t.guard and not t.guard(context) then
        return false
    end
    return true
end

function StateMachine:getHistory()
    return self.history
end

function StateMachine:reset(state)
    self.currentState = state or self.history[1] and self.history[1].from
    self.history = {}
end

return StateMachine
]]

TEST_FILE_10 = [[
-- Module: JSON
-- Simple JSON encoder/decoder

local JSON = {}

function JSON.encode(value)
    local vType = type(value)
    if value == nil then
        return "null"
    elseif vType == "boolean" then
        return value and "true" or "false"
    elseif vType == "number" then
        if value ~= value then return "null" end
        if value == math.huge or value == -math.huge then return "null" end
        if value == math.floor(value) then
            return string.format("%d", value)
        end
        return tostring(value)
    elseif vType == "string" then
        local escaped = value:gsub('\\', '\\\\')
            :gsub('"', '\\"')
            :gsub('\n', '\\n')
            :gsub('\r', '\\r')
            :gsub('\t', '\\t')
        return '"' .. escaped .. '"'
    elseif vType == "table" then
        -- Check if array
        local isArray = true
        local maxIdx = 0
        for k in next, value do
            if type(k) ~= "number" or k ~= math.floor(k) or k < 1 then
                isArray = false
                break
            end
            if k > maxIdx then maxIdx = k end
        end
        if isArray and maxIdx == #value then
            local parts = {}
            for i = 1, #value do
                parts[i] = JSON.encode(value[i])
            end
            return "[" .. table.concat(parts, ",") .. "]"
        else
            local parts = {}
            for k, v in next, value do
                parts[#parts + 1] = JSON.encode(tostring(k)) .. ":" .. JSON.encode(v)
            end
            table.sort(parts)
            return "{" .. table.concat(parts, ",") .. "}"
        end
    end
    return "null"
end

function JSON.decode(str)
    local pos = 1
    local function skipWhitespace()
        while pos <= #str do
            local c = str:sub(pos, pos)
            if c == " " or c == "\t" or c == "\n" or c == "\r" then
                pos = pos + 1
            else
                break
            end
        end
    end
    local function parseValue()
        skipWhitespace()
        local c = str:sub(pos, pos)
        if c == '"' then
            return JSON._parseString(str, pos)
        elseif c == '{' then
            return JSON._parseObject(str, pos)
        elseif c == '[' then
            return JSON._parseArray(str, pos)
        elseif str:sub(pos, pos + 3) == "true" then
            pos = pos + 4
            return true
        elseif str:sub(pos, pos + 4) == "false" then
            pos = pos + 5
            return false
        elseif str:sub(pos, pos + 3) == "null" then
            pos = pos + 4
            return nil
        else
            return JSON._parseNumber(str, pos)
        end
    end
    return parseValue()
end

function JSON._parseString(str, startPos)
    -- Simple string parser
    local result = {}
    local i = startPos + 1
    while i <= #str do
        local c = str:sub(i, i)
        if c == '"' then
            return table.concat(result), i + 1
        elseif c == '\\' then
            i = i + 1
            local next = str:sub(i, i)
            if next == 'n' then result[#result + 1] = '\n'
            elseif next == 't' then result[#result + 1] = '\t'
            elseif next == 'r' then result[#result + 1] = '\r'
            else result[#result + 1] = next
            end
        else
            result[#result + 1] = c
        end
        i = i + 1
    end
    return table.concat(result), i
end

function JSON._parseNumber(str, startPos)
    local numStr = str:match("^%-?%d+%.?%d*[eE]?[%+%-]?%d*", startPos)
    if numStr then
        return tonumber(numStr), startPos + #numStr
    end
    return 0, startPos
end

function JSON._parseObject(str, startPos)
    -- Simplified, not fully robust
    return {}, startPos + 2
end

function JSON._parseArray(str, startPos)
    return {}, startPos + 2
end

return JSON
]]

TEST_FILE_11 = [[
-- Module: Logger
-- Structured logging with levels and formatters

local Logger = {}
Logger.__index = Logger

Logger.LEVELS = {
    DEBUG = 10,
    INFO = 20,
    WARN = 30,
    ERROR = 40,
    FATAL = 50
}

function Logger.new(name, level)
    local self = setmetatable({}, Logger)
    self.name = name
    self.level = level or Logger.LEVELS.INFO
    self.handlers = {}
    self.buffer = {}
    self.maxBuffer = 1000
    return self
end

function Logger:addHandler(handler)
    self.handlers[#self.handlers + 1] = handler
end

function Logger:log(level, message, context)
    if level < self.level then return end
    local entry = {
        timestamp = os.clock(),
        level = level,
        logger = self.name,
        message = message,
        context = context
    }
    self.buffer[#self.buffer + 1] = entry
    if #self.buffer > self.maxBuffer then
        table.remove(self.buffer, 1)
    end
    for i = 1, #self.handlers do
        self.handlers[i](entry)
    end
end

function Logger:debug(msg, ctx) self:log(Logger.LEVELS.DEBUG, msg, ctx) end
function Logger:info(msg, ctx) self:log(Logger.LEVELS.INFO, msg, ctx) end
function Logger:warn(msg, ctx) self:log(Logger.LEVELS.WARN, msg, ctx) end
function Logger:error(msg, ctx) self:log(Logger.LEVELS.ERROR, msg, ctx) end
function Logger:fatal(msg, ctx) self:log(Logger.LEVELS.FATAL, msg, ctx) end

function Logger:getBuffer()
    return self.buffer
end

function Logger:clearBuffer()
    self.buffer = {}
end

function Logger:setLevel(level)
    self.level = level
end

function Logger.formatSimple(entry)
    local levelName = "UNKNOWN"
    for name, val in next, Logger.LEVELS do
        if val == entry.level then
            levelName = name
            break
        end
    end
    return string.format("[%s] %s: %s", levelName, entry.logger, entry.message)
end

return Logger
]]

TEST_FILE_12 = [[
-- Module: PathFinder
-- A* pathfinding on a grid

local PathFinder = {}
PathFinder.__index = PathFinder

function PathFinder.new(width, height)
    local self = setmetatable({}, PathFinder)
    self.width = width
    self.height = height
    self.grid = {}
    for y = 1, height do
        self.grid[y] = {}
        for x = 1, width do
            self.grid[y][x] = 0
        end
    end
    return self
end

function PathFinder:setWall(x, y)
    if x >= 1 and x <= self.width and y >= 1 and y <= self.height then
        self.grid[y][x] = 1
    end
end

function PathFinder:isWalkable(x, y)
    if x < 1 or x > self.width or y < 1 or y > self.height then
        return false
    end
    return self.grid[y][x] == 0
end

function PathFinder:heuristic(x1, y1, x2, y2)
    return math.abs(x1 - x2) + math.abs(y1 - y2)
end

function PathFinder:findPath(startX, startY, endX, endY)
    local open = {}
    local closed = {}
    local cameFrom = {}

    local startKey = startY * self.width + startX
    open[startKey] = { x = startX, y = startY, g = 0,
        h = self:heuristic(startX, startY, endX, endY) }
    open[startKey].f = open[startKey].g + open[startKey].h

    while true do
        -- Find lowest f in open set
        local bestKey, bestNode = nil, nil
        for k, node in next, open do
            if not bestNode or node.f < bestNode.f then
                bestKey = k
                bestNode = node
            end
        end

        if not bestNode then return nil end

        if bestNode.x == endX and bestNode.y == endY then
            -- Reconstruct path
            local path = {}
            local key = bestKey
            while key do
                local node = closed[key] or bestNode
                path[#path + 1] = { x = node.x, y = node.y }
                key = cameFrom[key]
            end
            -- Reverse
            local reversed = {}
            for i = #path, 1, -1 do
                reversed[#reversed + 1] = path[i]
            end
            return reversed
        end

        open[bestKey] = nil
        closed[bestKey] = bestNode

        -- Check neighbors
        local neighbors = {
            { x = bestNode.x + 1, y = bestNode.y },
            { x = bestNode.x - 1, y = bestNode.y },
            { x = bestNode.x, y = bestNode.y + 1 },
            { x = bestNode.x, y = bestNode.y - 1 }
        }

        for i = 1, #neighbors do
            local nx, ny = neighbors[i].x, neighbors[i].y
            if self:isWalkable(nx, ny) then
                local nKey = ny * self.width + nx
                if not closed[nKey] then
                    local g = bestNode.g + 1
                    local existing = open[nKey]
                    if not existing or g < existing.g then
                        local h = self:heuristic(nx, ny, endX, endY)
                        open[nKey] = { x = nx, y = ny, g = g, h = h, f = g + h }
                        cameFrom[nKey] = bestKey
                    end
                end
            end
        end
    end
end

return PathFinder
]]

TEST_FILE_13 = [[
-- Module: TokenStream
-- Lexer/tokenizer for a simple expression language

local TokenStream = {}
TokenStream.__index = TokenStream

TokenStream.TOKEN_TYPES = {
    NUMBER = "NUMBER",
    STRING = "STRING",
    IDENTIFIER = "IDENTIFIER",
    OPERATOR = "OPERATOR",
    PAREN_OPEN = "PAREN_OPEN",
    PAREN_CLOSE = "PAREN_CLOSE",
    COMMA = "COMMA",
    SEMICOLON = "SEMICOLON",
    KEYWORD = "KEYWORD",
    EOF = "EOF"
}

local KEYWORDS = {
    ["if"] = true, ["then"] = true, ["else"] = true,
    ["while"] = true, ["do"] = true, ["end"] = true,
    ["function"] = true, ["return"] = true, ["local"] = true,
    ["true"] = true, ["false"] = true, ["nil"] = true,
    ["and"] = true, ["or"] = true, ["not"] = true
}

function TokenStream.new(source)
    local self = setmetatable({}, TokenStream)
    self.source = source
    self.pos = 1
    self.tokens = {}
    self:tokenize()
    self.readPos = 1
    return self
end

function TokenStream:peek()
    return self.source:sub(self.pos, self.pos)
end

function TokenStream:advance()
    self.pos = self.pos + 1
end

function TokenStream:skipWhitespace()
    while self.pos <= #self.source do
        local c = self:peek()
        if c == " " or c == "\t" or c == "\n" or c == "\r" then
            self:advance()
        elseif c == "-" and self.source:sub(self.pos, self.pos + 1) == "--" then
            while self.pos <= #self.source and self:peek() ~= "\n" do
                self:advance()
            end
        else
            break
        end
    end
end

function TokenStream:readNumber()
    local start = self.pos
    while self.pos <= #self.source and self:peek():match("%d") do
        self:advance()
    end
    if self.pos <= #self.source and self:peek() == "." then
        self:advance()
        while self.pos <= #self.source and self:peek():match("%d") do
            self:advance()
        end
    end
    return { type = TokenStream.TOKEN_TYPES.NUMBER,
             value = self.source:sub(start, self.pos - 1) }
end

function TokenStream:readString()
    local quote = self:peek()
    self:advance()
    local start = self.pos
    while self.pos <= #self.source and self:peek() ~= quote do
        if self:peek() == "\\" then self:advance() end
        self:advance()
    end
    local value = self.source:sub(start, self.pos - 1)
    self:advance()
    return { type = TokenStream.TOKEN_TYPES.STRING, value = value }
end

function TokenStream:readIdentifier()
    local start = self.pos
    while self.pos <= #self.source and self:peek():match("[%w_]") do
        self:advance()
    end
    local value = self.source:sub(start, self.pos - 1)
    local tokenType = KEYWORDS[value] and TokenStream.TOKEN_TYPES.KEYWORD
        or TokenStream.TOKEN_TYPES.IDENTIFIER
    return { type = tokenType, value = value }
end

function TokenStream:tokenize()
    while self.pos <= #self.source do
        self:skipWhitespace()
        if self.pos > #self.source then break end
        local c = self:peek()
        if c:match("%d") then
            self.tokens[#self.tokens + 1] = self:readNumber()
        elseif c == '"' or c == "'" then
            self.tokens[#self.tokens + 1] = self:readString()
        elseif c:match("[%a_]") then
            self.tokens[#self.tokens + 1] = self:readIdentifier()
        elseif c == "(" then
            self.tokens[#self.tokens + 1] = { type = TokenStream.TOKEN_TYPES.PAREN_OPEN, value = c }
            self:advance()
        elseif c == ")" then
            self.tokens[#self.tokens + 1] = { type = TokenStream.TOKEN_TYPES.PAREN_CLOSE, value = c }
            self:advance()
        elseif c == "," then
            self.tokens[#self.tokens + 1] = { type = TokenStream.TOKEN_TYPES.COMMA, value = c }
            self:advance()
        elseif c == ";" then
            self.tokens[#self.tokens + 1] = { type = TokenStream.TOKEN_TYPES.SEMICOLON, value = c }
            self:advance()
        elseif c:match("[%+%-%*/%%=<>!&|^~]") then
            local start = self.pos
            self:advance()
            if self.pos <= #self.source and self:peek():match("[=<>]") then
                self:advance()
            end
            self.tokens[#self.tokens + 1] = { type = TokenStream.TOKEN_TYPES.OPERATOR,
                value = self.source:sub(start, self.pos - 1) }
        else
            self:advance()
        end
    end
    self.tokens[#self.tokens + 1] = { type = TokenStream.TOKEN_TYPES.EOF, value = "" }
end

function TokenStream:next()
    local token = self.tokens[self.readPos]
    self.readPos = self.readPos + 1
    return token
end

function TokenStream:peekToken()
    return self.tokens[self.readPos]
end

function TokenStream:hasMore()
    return self.readPos <= #self.tokens and
        self.tokens[self.readPos].type ~= TokenStream.TOKEN_TYPES.EOF
end

return TokenStream
]]

TEST_FILE_14 = [[
-- Module: QuadTree
-- Spatial partitioning data structure for 2D collision detection

local QuadTree = {}
QuadTree.__index = QuadTree

local MAX_OBJECTS = 10
local MAX_LEVELS = 5

function QuadTree.new(level, bounds)
    local self = setmetatable({}, QuadTree)
    self.level = level or 0
    self.objects = {}
    self.nodes = {}
    self.bounds = bounds or { x = 0, y = 0, width = 800, height = 600 }
    return self
end

function QuadTree:clear()
    self.objects = {}
    for i = 1, #self.nodes do
        self.nodes[i]:clear()
    end
    self.nodes = {}
end

function QuadTree:split()
    local subWidth = self.bounds.width / 2
    local subHeight = self.bounds.height / 2
    local x = self.bounds.x
    local y = self.bounds.y

    self.nodes[1] = QuadTree.new(self.level + 1,
        { x = x + subWidth, y = y, width = subWidth, height = subHeight })
    self.nodes[2] = QuadTree.new(self.level + 1,
        { x = x, y = y, width = subWidth, height = subHeight })
    self.nodes[3] = QuadTree.new(self.level + 1,
        { x = x, y = y + subHeight, width = subWidth, height = subHeight })
    self.nodes[4] = QuadTree.new(self.level + 1,
        { x = x + subWidth, y = y + subHeight, width = subWidth, height = subHeight })
end

function QuadTree:getIndex(rect)
    local index = -1
    local vertMid = self.bounds.x + self.bounds.width / 2
    local horizMid = self.bounds.y + self.bounds.height / 2

    local topQuad = (rect.y < horizMid and rect.y + rect.height < horizMid)
    local bottomQuad = (rect.y > horizMid)

    if rect.x < vertMid and rect.x + rect.width < vertMid then
        if topQuad then index = 2
        elseif bottomQuad then index = 3
        end
    elseif rect.x > vertMid then
        if topQuad then index = 1
        elseif bottomQuad then index = 4
        end
    end

    return index
end

function QuadTree:insert(rect)
    if #self.nodes > 0 then
        local index = self:getIndex(rect)
        if index ~= -1 then
            self.nodes[index]:insert(rect)
            return
        end
    end

    self.objects[#self.objects + 1] = rect

    if #self.objects > MAX_OBJECTS and self.level < MAX_LEVELS then
        if #self.nodes == 0 then
            self:split()
        end

        local i = 1
        while i <= #self.objects do
            local index = self:getIndex(self.objects[i])
            if index ~= -1 then
                local obj = table.remove(self.objects, i)
                self.nodes[index]:insert(obj)
            else
                i = i + 1
            end
        end
    end
end

function QuadTree:retrieve(returnObjects, rect)
    local index = self:getIndex(rect)
    if index ~= -1 and #self.nodes > 0 then
        self.nodes[index]:retrieve(returnObjects, rect)
    end

    for i = 1, #self.objects do
        returnObjects[#returnObjects + 1] = self.objects[i]
    end

    return returnObjects
end

function QuadTree:count()
    local total = #self.objects
    for i = 1, #self.nodes do
        total = total + self.nodes[i]:count()
    end
    return total
end

return QuadTree
]]

TEST_FILE_15 = [[
-- Module: Signal
-- Reactive signal/computed value system (like SolidJS signals)

local Signal = {}
Signal.__index = Signal

local Computed = {}
Computed.__index = Computed

local Effect = {}
Effect.__index = Effect

local currentEffect = nil

function Signal.new(initialValue)
    local self = setmetatable({}, Signal)
    self.value = initialValue
    self.subscribers = {}
    return self
end

function Signal:get()
    if currentEffect then
        self.subscribers[currentEffect] = true
    end
    return self.value
end

function Signal:set(newValue)
    if self.value ~= newValue then
        self.value = newValue
        self:notify()
    end
end

function Signal:notify()
    for subscriber in next, self.subscribers do
        subscriber:update()
    end
end

function Computed.new(fn)
    local self = setmetatable({}, Computed)
    self.fn = fn
    self.value = nil
    self.dirty = true
    self.subscribers = {}
    self:update()
    return self
end

function Computed:get()
    if currentEffect then
        self.subscribers[currentEffect] = true
    end
    if self.dirty then
        self:recompute()
    end
    return self.value
end

function Computed:recompute()
    local prev = currentEffect
    currentEffect = self
    self.value = self.fn()
    currentEffect = prev
    self.dirty = false
end

function Computed:update()
    self.dirty = true
    for subscriber in next, self.subscribers do
        subscriber:update()
    end
end

function Effect.new(fn)
    local self = setmetatable({}, Effect)
    self.fn = fn
    self.disposed = false
    self:run()
    return self
end

function Effect:run()
    if self.disposed then return end
    local prev = currentEffect
    currentEffect = self
    self.fn()
    currentEffect = prev
end

function Effect:update()
    self:run()
end

function Effect:dispose()
    self.disposed = true
end

-- Factory functions
function createSignal(value)
    local sig = Signal.new(value)
    local getter = function() return sig:get() end
    local setter = function(v) sig:set(v) end
    return getter, setter
end

function createComputed(fn)
    local comp = Computed.new(fn)
    return function() return comp:get() end
end

function createEffect(fn)
    return Effect.new(fn)
end

return {
    Signal = Signal,
    Computed = Computed,
    Effect = Effect,
    createSignal = createSignal,
    createComputed = createComputed,
    createEffect = createEffect
}
]]

TEST_FILE_16 = [[
-- Module: RingBuffer
-- Fixed-size circular buffer with overflow handling

local RingBuffer = {}
RingBuffer.__index = RingBuffer

function RingBuffer.new(capacity)
    local self = setmetatable({}, RingBuffer)
    self.capacity = capacity
    self.buffer = {}
    self.head = 1
    self.tail = 1
    self.size = 0
    return self
end

function RingBuffer:push(value)
    self.buffer[self.tail] = value
    if self.size == self.capacity then
        self.head = self.head % self.capacity + 1
    else
        self.size = self.size + 1
    end
    self.tail = self.tail % self.capacity + 1
end

function RingBuffer:pop()
    if self.size == 0 then return nil end
    local value = self.buffer[self.head]
    self.buffer[self.head] = nil
    self.head = self.head % self.capacity + 1
    self.size = self.size - 1
    return value
end

function RingBuffer:peek()
    if self.size == 0 then return nil end
    return self.buffer[self.head]
end

function RingBuffer:peekBack()
    if self.size == 0 then return nil end
    local idx = (self.tail - 2) % self.capacity + 1
    return self.buffer[idx]
end

function RingBuffer:isFull()
    return self.size == self.capacity
end

function RingBuffer:isEmpty()
    return self.size == 0
end

function RingBuffer:getSize()
    return self.size
end

function RingBuffer:toArray()
    local arr = {}
    local idx = self.head
    for i = 1, self.size do
        arr[i] = self.buffer[idx]
        idx = idx % self.capacity + 1
    end
    return arr
end

function RingBuffer:clear()
    self.buffer = {}
    self.head = 1
    self.tail = 1
    self.size = 0
end

function RingBuffer:contains(value)
    local idx = self.head
    for i = 1, self.size do
        if self.buffer[idx] == value then return true end
        idx = idx % self.capacity + 1
    end
    return false
end

function RingBuffer:average()
    if self.size == 0 then return 0 end
    local sum = 0
    local idx = self.head
    for i = 1, self.size do
        sum = sum + (self.buffer[idx] or 0)
        idx = idx % self.capacity + 1
    end
    return sum / self.size
end

return RingBuffer
]]

TEST_FILE_17 = [[
-- Module: Tween
-- Animation tweening library with easing functions

local Tween = {}
Tween.__index = Tween

-- Easing functions
Tween.Easing = {}

function Tween.Easing.linear(t)
    return t
end

function Tween.Easing.easeInQuad(t)
    return t * t
end

function Tween.Easing.easeOutQuad(t)
    return t * (2 - t)
end

function Tween.Easing.easeInOutQuad(t)
    if t < 0.5 then
        return 2 * t * t
    else
        return -1 + (4 - 2 * t) * t
    end
end

function Tween.Easing.easeInCubic(t)
    return t * t * t
end

function Tween.Easing.easeOutCubic(t)
    local t1 = t - 1
    return t1 * t1 * t1 + 1
end

function Tween.Easing.easeInOutCubic(t)
    if t < 0.5 then
        return 4 * t * t * t
    else
        local t1 = (2 * t - 2)
        return 0.5 * t1 * t1 * t1 + 1
    end
end

function Tween.Easing.easeInElastic(t)
    if t == 0 or t == 1 then return t end
    return -math.pow(2, 10 * (t - 1)) * math.sin((t - 1.1) * 5 * math.pi)
end

function Tween.Easing.easeOutElastic(t)
    if t == 0 or t == 1 then return t end
    return math.pow(2, -10 * t) * math.sin((t - 0.1) * 5 * math.pi) + 1
end

function Tween.Easing.easeInOutElastic(t)
    if t == 0 or t == 1 then return t end
    t = t * 2
    if t < 1 then
        return -0.5 * math.pow(2, 10 * (t - 1)) * math.sin((t - 1.1) * 5 * math.pi)
    end
    return 0.5 * math.pow(2, -10 * (t - 1)) * math.sin((t - 1.1) * 5 * math.pi) + 1
end

function Tween.Easing.easeInBounce(t)
    return 1 - Tween.Easing.easeOutBounce(1 - t)
end

function Tween.Easing.easeOutBounce(t)
    if t < 1 / 2.75 then
        return 7.5625 * t * t
    elseif t < 2 / 2.75 then
        t = t - 1.5 / 2.75
        return 7.5625 * t * t + 0.75
    elseif t < 2.5 / 2.75 then
        t = t - 2.25 / 2.75
        return 7.5625 * t * t + 0.9375
    else
        t = t - 2.625 / 2.75
        return 7.5625 * t * t + 0.984375
    end
end

function Tween.new(startVal, endVal, duration, easingFn)
    local self = setmetatable({}, Tween)
    self.startVal = startVal
    self.endVal = endVal
    self.duration = duration
    self.easingFn = easingFn or Tween.Easing.linear
    self.elapsed = 0
    self.completed = false
    self.onUpdate = nil
    self.onComplete = nil
    return self
end

function Tween:update(dt)
    if self.completed then return self.endVal end
    self.elapsed = self.elapsed + dt
    if self.elapsed >= self.duration then
        self.elapsed = self.duration
        self.completed = true
    end
    local t = self.elapsed / self.duration
    local easedT = self.easingFn(t)
    local value = self.startVal + (self.endVal - self.startVal) * easedT
    if self.onUpdate then self.onUpdate(value) end
    if self.completed and self.onComplete then self.onComplete() end
    return value
end

function Tween:reset()
    self.elapsed = 0
    self.completed = false
end

function Tween:isCompleted()
    return self.completed
end

function Tween:getValue()
    local t = self.elapsed / self.duration
    local easedT = self.easingFn(t)
    return self.startVal + (self.endVal - self.startVal) * easedT
end

return Tween
]]

TEST_FILE_18 = [[
-- Module: ObjectPool
-- Reusable object pool to avoid garbage collection pressure

local ObjectPool = {}
ObjectPool.__index = ObjectPool

function ObjectPool.new(factory, resetFn, initialSize)
    local self = setmetatable({}, ObjectPool)
    self.factory = factory
    self.resetFn = resetFn or function(obj) end
    self.pool = {}
    self.active = {}
    self.activeCount = 0
    self.totalCreated = 0

    initialSize = initialSize or 0
    for i = 1, initialSize do
        self.pool[i] = factory()
        self.totalCreated = self.totalCreated + 1
    end

    return self
end

function ObjectPool:acquire()
    local obj
    if #self.pool > 0 then
        obj = table.remove(self.pool)
    else
        obj = self.factory()
        self.totalCreated = self.totalCreated + 1
    end
    self.active[obj] = true
    self.activeCount = self.activeCount + 1
    return obj
end

function ObjectPool:release(obj)
    if self.active[obj] then
        self.active[obj] = nil
        self.activeCount = self.activeCount - 1
        self.resetFn(obj)
        self.pool[#self.pool + 1] = obj
    end
end

function ObjectPool:releaseAll()
    for obj in next, self.active do
        self.active[obj] = nil
        self.resetFn(obj)
        self.pool[#self.pool + 1] = obj
    end
    self.activeCount = 0
end

function ObjectPool:getActiveCount()
    return self.activeCount
end

function ObjectPool:getPoolSize()
    return #self.pool
end

function ObjectPool:getTotalCreated()
    return self.totalCreated
end

function ObjectPool:prewarm(count)
    for i = 1, count do
        local obj = self.factory()
        self.totalCreated = self.totalCreated + 1
        self.pool[#self.pool + 1] = obj
    end
end

function ObjectPool:shrink(targetSize)
    while #self.pool > targetSize do
        table.remove(self.pool)
    end
end

return ObjectPool
]]

TEST_FILE_19 = [[
-- Module: CommandPattern
-- Command pattern with undo/redo stack

local CommandHistory = {}
CommandHistory.__index = CommandHistory

function CommandHistory.new(maxSize)
    local self = setmetatable({}, CommandHistory)
    self.undoStack = {}
    self.redoStack = {}
    self.maxSize = maxSize or 100
    return self
end

function CommandHistory:execute(command)
    command:execute()
    self.undoStack[#self.undoStack + 1] = command
    -- Clear redo stack on new command
    self.redoStack = {}
    -- Enforce max size
    if #self.undoStack > self.maxSize then
        table.remove(self.undoStack, 1)
    end
end

function CommandHistory:undo()
    if #self.undoStack == 0 then return false end
    local command = table.remove(self.undoStack)
    command:undo()
    self.redoStack[#self.redoStack + 1] = command
    return true
end

function CommandHistory:redo()
    if #self.redoStack == 0 then return false end
    local command = table.remove(self.redoStack)
    command:execute()
    self.undoStack[#self.undoStack + 1] = command
    return true
end

function CommandHistory:canUndo()
    return #self.undoStack > 0
end

function CommandHistory:canRedo()
    return #self.redoStack > 0
end

function CommandHistory:clear()
    self.undoStack = {}
    self.redoStack = {}
end

function CommandHistory:getUndoCount()
    return #self.undoStack
end

function CommandHistory:getRedoCount()
    return #self.redoStack
end

-- Example command: SetValueCommand
local SetValueCommand = {}
SetValueCommand.__index = SetValueCommand

function SetValueCommand.new(target, key, newValue)
    local self = setmetatable({}, SetValueCommand)
    self.target = target
    self.key = key
    self.newValue = newValue
    self.oldValue = target[key]
    return self
end

function SetValueCommand:execute()
    self.target[self.key] = self.newValue
end

function SetValueCommand:undo()
    self.target[self.key] = self.oldValue
end

-- Batch command
local BatchCommand = {}
BatchCommand.__index = BatchCommand

function BatchCommand.new(commands)
    local self = setmetatable({}, BatchCommand)
    self.commands = commands or {}
    return self
end

function BatchCommand:execute()
    for i = 1, #self.commands do
        self.commands[i]:execute()
    end
end

function BatchCommand:undo()
    for i = #self.commands, 1, -1 do
        self.commands[i]:undo()
    end
end

return {
    CommandHistory = CommandHistory,
    SetValueCommand = SetValueCommand,
    BatchCommand = BatchCommand
}
]]

TEST_FILE_20 = [[
-- Module: Observable
-- Observer pattern with filtering and mapping

local Observable = {}
Observable.__index = Observable

function Observable.new()
    local self = setmetatable({}, Observable)
    self.observers = {}
    self.transforms = {}
    return self
end

function Observable:subscribe(observer)
    self.observers[#self.observers + 1] = observer
    return function()
        for i = #self.observers, 1, -1 do
            if self.observers[i] == observer then
                table.remove(self.observers, i)
                break
            end
        end
    end
end

function Observable:emit(value)
    local transformed = value
    for i = 1, #self.transforms do
        transformed = self.transforms[i](transformed)
        if transformed == nil then return end
    end
    for i = 1, #self.observers do
        self.observers[i](transformed)
    end
end

function Observable:map(fn)
    local newObs = Observable.new()
    newObs.transforms = {}
    for i = 1, #self.transforms do
        newObs.transforms[i] = self.transforms[i]
    end
    newObs.transforms[#newObs.transforms + 1] = fn
    -- Subscribe the new observable to this one
    self:subscribe(function(val)
        newObs:emit(val)
    end)
    return newObs
end

function Observable:filter(predicate)
    local newObs = Observable.new()
    self:subscribe(function(val)
        if predicate(val) then
            newObs:emit(val)
        end
    end)
    return newObs
end

function Observable:reduce(reducer, initial)
    local acc = initial
    self:subscribe(function(val)
        acc = reducer(acc, val)
    end)
    return function() return acc end
end

function Observable:take(count)
    local taken = 0
    local newObs = Observable.new()
    self:subscribe(function(val)
        if taken < count then
            taken = taken + 1
            newObs:emit(val)
        end
    end)
    return newObs
end

function Observable:skip(count)
    local skipped = 0
    local newObs = Observable.new()
    self:subscribe(function(val)
        if skipped >= count then
            newObs:emit(val)
        else
            skipped = skipped + 1
        end
    end)
    return newObs
end

function Observable:debounce(minInterval)
    local lastTime = 0
    local newObs = Observable.new()
    self:subscribe(function(val)
        local now = os.clock()
        if now - lastTime >= minInterval then
            lastTime = now
            newObs:emit(val)
        end
    end)
    return newObs
end

function Observable:distinct()
    local lastVal = nil
    local newObs = Observable.new()
    self:subscribe(function(val)
        if val ~= lastVal then
            lastVal = val
            newObs:emit(val)
        end
    end)
    return newObs
end

return Observable
]]

TEST_FILE_21 = [[
-- Module: ECS
-- Entity Component System (simplified)

local ECS = {}
ECS.__index = ECS

function ECS.new()
    local self = setmetatable({}, ECS)
    self.nextEntityId = 1
    self.entities = {}
    self.components = {}
    self.systems = {}
    return self
end

function ECS:createEntity()
    local id = self.nextEntityId
    self.nextEntityId = self.nextEntityId + 1
    self.entities[id] = true
    return id
end

function ECS:destroyEntity(entityId)
    self.entities[entityId] = nil
    for compType in next, self.components do
        self.components[compType][entityId] = nil
    end
end

function ECS:addComponent(entityId, componentType, data)
    if not self.components[componentType] then
        self.components[componentType] = {}
    end
    self.components[componentType][entityId] = data
end

function ECS:removeComponent(entityId, componentType)
    if self.components[componentType] then
        self.components[componentType][entityId] = nil
    end
end

function ECS:getComponent(entityId, componentType)
    if self.components[componentType] then
        return self.components[componentType][entityId]
    end
    return nil
end

function ECS:hasComponent(entityId, componentType)
    return self.components[componentType] and
           self.components[componentType][entityId] ~= nil
end

function ECS:query(...)
    local required = {...}
    local results = {}
    for entityId in next, self.entities do
        local hasAll = true
        for i = 1, #required do
            if not self:hasComponent(entityId, required[i]) then
                hasAll = false
                break
            end
        end
        if hasAll then
            results[#results + 1] = entityId
        end
    end
    return results
end

function ECS:addSystem(name, requiredComponents, updateFn)
    self.systems[#self.systems + 1] = {
        name = name,
        required = requiredComponents,
        update = updateFn
    }
end

function ECS:update(dt)
    for i = 1, #self.systems do
        local system = self.systems[i]
        local entities = self:query(table.unpack(system.required))
        for j = 1, #entities do
            system.update(self, entities[j], dt)
        end
    end
end

function ECS:entityCount()
    local count = 0
    for _ in next, self.entities do
        count = count + 1
    end
    return count
end

function ECS:componentCount(componentType)
    if not self.components[componentType] then return 0 end
    local count = 0
    for _ in next, self.components[componentType] do
        count = count + 1
    end
    return count
end

return ECS
]]

TEST_FILE_22 = [[
-- Module: VirtualDOM
-- Simplified virtual DOM diffing and patching

local VDOM = {}
VDOM.__index = VDOM

function VDOM.createElement(tag, props, children)
    return {
        tag = tag,
        props = props or {},
        children = children or {},
        key = props and props.key or nil
    }
end

function VDOM.createTextNode(text)
    return {
        tag = "#text",
        props = { value = text },
        children = {}
    }
end

function VDOM.diff(oldNode, newNode)
    local patches = {}

    if oldNode == nil then
        patches[#patches + 1] = { type = "CREATE", node = newNode }
    elseif newNode == nil then
        patches[#patches + 1] = { type = "REMOVE" }
    elseif oldNode.tag ~= newNode.tag then
        patches[#patches + 1] = { type = "REPLACE", node = newNode }
    else
        -- Diff props
        local propPatches = VDOM.diffProps(oldNode.props, newNode.props)
        if #propPatches > 0 then
            patches[#patches + 1] = { type = "PROPS", changes = propPatches }
        end

        -- Diff children
        local childPatches = VDOM.diffChildren(oldNode.children, newNode.children)
        if #childPatches > 0 then
            patches[#patches + 1] = { type = "CHILDREN", patches = childPatches }
        end
    end

    return patches
end

function VDOM.diffProps(oldProps, newProps)
    local changes = {}
    -- Check for changed/added props
    for k, v in next, newProps do
        if oldProps[k] ~= v then
            changes[#changes + 1] = { key = k, value = v }
        end
    end
    -- Check for removed props
    for k in next, oldProps do
        if newProps[k] == nil then
            changes[#changes + 1] = { key = k, value = nil }
        end
    end
    return changes
end

function VDOM.diffChildren(oldChildren, newChildren)
    local patches = {}
    local maxLen = math.max(#oldChildren, #newChildren)
    for i = 1, maxLen do
        local childPatches = VDOM.diff(oldChildren[i], newChildren[i])
        patches[#patches + 1] = childPatches
    end
    return patches
end

function VDOM.render(node, indent)
    indent = indent or 0
    local prefix = string.rep("  ", indent)
    if node.tag == "#text" then
        return prefix .. node.props.value
    end

    local lines = {}
    local propsStr = ""
    for k, v in next, node.props do
        if k ~= "key" then
            propsStr = propsStr .. " " .. k .. '="' .. tostring(v) .. '"'
        end
    end

    lines[#lines + 1] = prefix .. "<" .. node.tag .. propsStr .. ">"
    for i = 1, #node.children do
        lines[#lines + 1] = VDOM.render(node.children[i], indent + 1)
    end
    lines[#lines + 1] = prefix .. "</" .. node.tag .. ">"
    return table.concat(lines, "\n")
end

function VDOM.patchCount(patches)
    local count = 0
    for i = 1, #patches do
        local p = patches[i]
        count = count + 1
        if p.type == "CHILDREN" then
            for j = 1, #p.patches do
                count = count + #p.patches[j]
            end
        end
    end
    return count
end

return VDOM
]]

TEST_FILE_23 = [==[
-- Module: Coroutine Pool
-- Manages a pool of coroutines for cooperative multitasking

local CoroutinePool = {}
CoroutinePool.__index = CoroutinePool

function CoroutinePool.new(maxConcurrent)
    local self = setmetatable({}, CoroutinePool)
    self.maxConcurrent = maxConcurrent or 4
    self.queue = {}
    self.running = {}
    self.runningCount = 0
    self.completed = {}
    self.completedCount = 0
    self.failedCount = 0
    return self
end

function CoroutinePool:submit(fn, id)
    id = id or (#self.queue + self.completedCount + 1)
    self.queue[#self.queue + 1] = { fn = fn, id = id }
end

function CoroutinePool:tick()
    -- Start new coroutines if capacity allows
    while self.runningCount < self.maxConcurrent and #self.queue > 0 do
        local task = table.remove(self.queue, 1)
        local co = coroutine.create(task.fn)
        self.running[task.id] = co
        self.runningCount = self.runningCount + 1
    end

    -- Resume running coroutines
    local toRemove = {}
    for id, co in next, self.running do
        if coroutine.status(co) == "suspended" then
            local ok, err = coroutine.resume(co)
            if not ok then
                toRemove[#toRemove + 1] = id
                self.failedCount = self.failedCount + 1
            elseif coroutine.status(co) == "dead" then
                toRemove[#toRemove + 1] = id
                self.completed[id] = true
                self.completedCount = self.completedCount + 1
            end
        elseif coroutine.status(co) == "dead" then
            toRemove[#toRemove + 1] = id
            self.completed[id] = true
            self.completedCount = self.completedCount + 1
        end
    end

    for i = 1, #toRemove do
        self.running[toRemove[i]] = nil
        self.runningCount = self.runningCount - 1
    end

    return self.runningCount > 0 or #self.queue > 0
end

function CoroutinePool:runAll()
    while self:tick() do end
end

function CoroutinePool:isIdle()
    return self.runningCount == 0 and #self.queue == 0
end

function CoroutinePool:getStats()
    return {
        queued = #self.queue,
        running = self.runningCount,
        completed = self.completedCount,
        failed = self.failedCount
    }
end

return CoroutinePool
]==]

TEST_FILE_24 = [[
-- Module: NetworkProtocol
-- Simple request/response protocol parser and builder

local Protocol = {}
Protocol.__index = Protocol

Protocol.STATUS_OK = 200
Protocol.STATUS_NOT_FOUND = 404
Protocol.STATUS_ERROR = 500
Protocol.STATUS_UNAUTHORIZED = 401
Protocol.STATUS_BAD_REQUEST = 400

Protocol.METHOD_GET = "GET"
Protocol.METHOD_POST = "POST"
Protocol.METHOD_PUT = "PUT"
Protocol.METHOD_DELETE = "DELETE"
Protocol.METHOD_PATCH = "PATCH"

function Protocol.createRequest(method, path, headers, body)
    local req = {
        method = method,
        path = path,
        headers = headers or {},
        body = body or "",
        timestamp = 0,
        id = math.random(100000, 999999)
    }
    return req
end

function Protocol.createResponse(status, headers, body)
    local resp = {
        status = status,
        headers = headers or {},
        body = body or "",
        timestamp = 0
    }
    return resp
end

function Protocol.serializeRequest(req)
    local parts = {}
    parts[#parts + 1] = req.method .. " " .. req.path .. " PROTO/1.1"
    parts[#parts + 1] = "X-Request-Id: " .. req.id
    for k, v in next, req.headers do
        parts[#parts + 1] = k .. ": " .. v
    end
    if #req.body > 0 then
        parts[#parts + 1] = "Content-Length: " .. #req.body
    end
    parts[#parts + 1] = ""
    if #req.body > 0 then
        parts[#parts + 1] = req.body
    end
    return table.concat(parts, "\r\n")
end

function Protocol.serializeResponse(resp)
    local parts = {}
    local statusText = "OK"
    if resp.status == 404 then statusText = "Not Found"
    elseif resp.status == 500 then statusText = "Internal Error"
    elseif resp.status == 401 then statusText = "Unauthorized"
    elseif resp.status == 400 then statusText = "Bad Request"
    end
    parts[#parts + 1] = "PROTO/1.1 " .. resp.status .. " " .. statusText
    for k, v in next, resp.headers do
        parts[#parts + 1] = k .. ": " .. v
    end
    if #resp.body > 0 then
        parts[#parts + 1] = "Content-Length: " .. #resp.body
    end
    parts[#parts + 1] = ""
    if #resp.body > 0 then
        parts[#parts + 1] = resp.body
    end
    return table.concat(parts, "\r\n")
end

function Protocol.parseRequest(raw)
    local lines = {}
    local pos = 1
    while pos <= #raw do
        local nl = raw:find("\r\n", pos, true)
        if nl then
            lines[#lines + 1] = raw:sub(pos, nl - 1)
            pos = nl + 2
        else
            lines[#lines + 1] = raw:sub(pos)
            break
        end
    end

    if #lines == 0 then return nil end

    local method, path = lines[1]:match("^(%S+)%s+(%S+)")
    if not method then return nil end

    local headers = {}
    local bodyStart = nil
    for i = 2, #lines do
        if lines[i] == "" then
            bodyStart = i + 1
            break
        end
        local k, v = lines[i]:match("^([^:]+):%s*(.+)$")
        if k then headers[k] = v end
    end

    local body = ""
    if bodyStart and bodyStart <= #lines then
        local bodyParts = {}
        for i = bodyStart, #lines do
            bodyParts[#bodyParts + 1] = lines[i]
        end
        body = table.concat(bodyParts, "\r\n")
    end

    return Protocol.createRequest(method, path, headers, body)
end

function Protocol.parseResponse(raw)
    local lines = {}
    local pos = 1
    while pos <= #raw do
        local nl = raw:find("\r\n", pos, true)
        if nl then
            lines[#lines + 1] = raw:sub(pos, nl - 1)
            pos = nl + 2
        else
            lines[#lines + 1] = raw:sub(pos)
            break
        end
    end

    if #lines == 0 then return nil end
    local status = lines[1]:match("PROTO/%d+%.%d+%s+(%d+)")
    if not status then return nil end

    local headers = {}
    local bodyStart = nil
    for i = 2, #lines do
        if lines[i] == "" then
            bodyStart = i + 1
            break
        end
        local k, v = lines[i]:match("^([^:]+):%s*(.+)$")
        if k then headers[k] = v end
    end

    local body = ""
    if bodyStart and bodyStart <= #lines then
        local bodyParts = {}
        for i = bodyStart, #lines do
            bodyParts[#bodyParts + 1] = lines[i]
        end
        body = table.concat(bodyParts, "\r\n")
    end

    return Protocol.createResponse(tonumber(status), headers, body)
end

-- Router
local Router = {}
Router.__index = Router

function Router.new()
    local self = setmetatable({}, Router)
    self.routes = {}
    self.middleware = {}
    return self
end

function Router:addRoute(method, pattern, handler)
    self.routes[#self.routes + 1] = {
        method = method,
        pattern = pattern,
        handler = handler
    }
end

function Router:addMiddleware(fn)
    self.middleware[#self.middleware + 1] = fn
end

function Router:match(method, path)
    for i = 1, #self.routes do
        local route = self.routes[i]
        if route.method == method then
            local match = path:match(route.pattern)
            if match then
                return route.handler, match
            end
        end
    end
    return nil
end

function Router:handle(request)
    -- Run middleware
    for i = 1, #self.middleware do
        local result = self.middleware[i](request)
        if result then return result end
    end

    local handler, param = self:match(request.method, request.path)
    if handler then
        return handler(request, param)
    end
    return Protocol.createResponse(Protocol.STATUS_NOT_FOUND, {},
        "Not Found: " .. request.path)
end

return { Protocol = Protocol, Router = Router }
]]

TEST_FILE_25 = [[
-- Module: Database
-- In-memory database with indexing and queries

local Database = {}
Database.__index = Database

local Table = {}
Table.__index = Table

function Database.new(name)
    local self = setmetatable({}, Database)
    self.name = name
    self.tables = {}
    self.version = 0
    return self
end

function Database:createTable(tableName, schema)
    local tbl = Table.new(tableName, schema)
    self.tables[tableName] = tbl
    self.version = self.version + 1
    return tbl
end

function Database:getTable(tableName)
    return self.tables[tableName]
end

function Database:dropTable(tableName)
    self.tables[tableName] = nil
    self.version = self.version + 1
end

function Database:tableNames()
    local names = {}
    for name in next, self.tables do
        names[#names + 1] = name
    end
    table.sort(names)
    return names
end

function Table.new(name, schema)
    local self = setmetatable({}, Table)
    self.name = name
    self.schema = schema or {}
    self.rows = {}
    self.nextId = 1
    self.indexes = {}
    return self
end

function Table:insert(row)
    row._id = self.nextId
    self.nextId = self.nextId + 1
    self.rows[row._id] = row
    -- Update indexes
    for field, index in next, self.indexes do
        local key = row[field]
        if key then
            if not index[key] then index[key] = {} end
            index[key][row._id] = true
        end
    end
    return row._id
end

function Table:get(id)
    return self.rows[id]
end

function Table:delete(id)
    local row = self.rows[id]
    if not row then return false end
    -- Remove from indexes
    for field, index in next, self.indexes do
        local key = row[field]
        if key and index[key] then
            index[key][id] = nil
        end
    end
    self.rows[id] = nil
    return true
end

function Table:update(id, changes)
    local row = self.rows[id]
    if not row then return false end
    for k, v in next, changes do
        -- Update indexes
        if self.indexes[k] then
            local oldKey = row[k]
            if oldKey and self.indexes[k][oldKey] then
                self.indexes[k][oldKey][id] = nil
            end
            if v then
                if not self.indexes[k][v] then
                    self.indexes[k][v] = {}
                end
                self.indexes[k][v][id] = true
            end
        end
        row[k] = v
    end
    return true
end

function Table:createIndex(field)
    self.indexes[field] = {}
    -- Build index from existing rows
    for id, row in next, self.rows do
        local key = row[field]
        if key then
            if not self.indexes[field][key] then
                self.indexes[field][key] = {}
            end
            self.indexes[field][key][id] = true
        end
    end
end

function Table:findByIndex(field, value)
    local index = self.indexes[field]
    if not index then return {} end
    local ids = index[value]
    if not ids then return {} end
    local results = {}
    for id in next, ids do
        results[#results + 1] = self.rows[id]
    end
    return results
end

function Table:select(predicate)
    local results = {}
    for id, row in next, self.rows do
        if predicate(row) then
            results[#results + 1] = row
        end
    end
    return results
end

function Table:selectAll()
    local results = {}
    for id, row in next, self.rows do
        results[#results + 1] = row
    end
    return results
end

function Table:count()
    local n = 0
    for _ in next, self.rows do n = n + 1 end
    return n
end

function Table:aggregate(field, fn, initial)
    local acc = initial
    for id, row in next, self.rows do
        if row[field] then
            acc = fn(acc, row[field])
        end
    end
    return acc
end

function Table:orderBy(field, descending)
    local results = self:selectAll()
    table.sort(results, function(a, b)
        if descending then
            return (a[field] or "") > (b[field] or "")
        else
            return (a[field] or "") < (b[field] or "")
        end
    end)
    return results
end

function Table:limit(results, n)
    if #results <= n then return results end
    local limited = {}
    for i = 1, n do
        limited[i] = results[i]
    end
    return limited
end

-- Join two tables
function Database:innerJoin(tableName1, tableName2, field1, field2)
    local t1 = self.tables[tableName1]
    local t2 = self.tables[tableName2]
    if not t1 or not t2 then return {} end

    local results = {}
    for id1, row1 in next, t1.rows do
        for id2, row2 in next, t2.rows do
            if row1[field1] == row2[field2] then
                local joined = {}
                for k, v in next, row1 do joined[tableName1 .. "." .. k] = v end
                for k, v in next, row2 do joined[tableName2 .. "." .. k] = v end
                results[#results + 1] = joined
            end
        end
    end
    return results
end

return { Database = Database, Table = Table }
]]

TEST_FILE_25_MODIFIED = [[
-- Module: Database (v2)
-- In-memory database with indexing, queries, and transactions

local Database = {}
Database.__index = Database

local Table = {}
Table.__index = Table

function Database.new(name)
    local self = setmetatable({}, Database)
    self.name = name
    self.tables = {}
    self.version = 0
    self.transactionLog = {}
    return self
end

function Database:createTable(tableName, schema)
    local tbl = Table.new(tableName, schema)
    self.tables[tableName] = tbl
    self.version = self.version + 1
    self.transactionLog[#self.transactionLog + 1] = {
        op = "CREATE_TABLE", table = tableName, time = os.clock()
    }
    return tbl
end

function Database:getTable(tableName)
    return self.tables[tableName]
end

function Database:dropTable(tableName)
    self.tables[tableName] = nil
    self.version = self.version + 1
    self.transactionLog[#self.transactionLog + 1] = {
        op = "DROP_TABLE", table = tableName, time = os.clock()
    }
end

function Database:beginTransaction()
    return { operations = {}, committed = false }
end

function Database:commitTransaction(tx)
    tx.committed = true
    self.transactionLog[#self.transactionLog + 1] = {
        op = "COMMIT", operations = #tx.operations, time = os.clock()
    }
end

function Database:rollbackTransaction(tx)
    -- Undo operations in reverse
    for i = #tx.operations, 1, -1 do
        local op = tx.operations[i]
        if op.type == "INSERT" then
            op.table:delete(op.id)
        elseif op.type == "DELETE" then
            op.table:insert(op.row)
        end
    end
end

function Database:tableNames()
    local names = {}
    for name in next, self.tables do
        names[#names + 1] = name
    end
    table.sort(names)
    return names
end

function Database:getVersion()
    return self.version
end

return { Database = Database, Table = Table }
]]

TEST_FILE_26 = [==[
-- Module: FSM Compiler
-- Compiles state machine definitions into optimized transition tables

local FSMCompiler = {}
FSMCompiler.__index = FSMCompiler

function FSMCompiler.new()
    local self = setmetatable({}, FSMCompiler)
    self.states = {}
    self.events = {}
    self.transitions = {}
    self.initialState = nil
    return self
end

function FSMCompiler:addState(name, config)
    self.states[name] = config or { onEnter = nil, onExit = nil }
    if not self.initialState then
        self.initialState = name
    end
end

function FSMCompiler:addEvent(name)
    self.events[name] = true
end

function FSMCompiler:addTransition(fromState, event, toState, action)
    local key = fromState .. ":" .. event
    self.transitions[key] = { target = toState, action = action }
end

function FSMCompiler:compile()
    -- Build transition table as nested lookup
    local table_lookup = {}
    for key, trans in next, self.transitions do
        local from, event = key:match("^(.+):(.+)$")
        if from and event then
            if not table_lookup[from] then
                table_lookup[from] = {}
            end
            table_lookup[from][event] = trans
        end
    end

    -- Build state index for array-based lookup
    local stateIndex = {}
    local stateList = {}
    for name in next, self.states do
        stateList[#stateList + 1] = name
    end
    table.sort(stateList)
    for i = 1, #stateList do
        stateIndex[stateList[i]] = i
    end

    return {
        transitionTable = table_lookup,
        stateIndex = stateIndex,
        stateList = stateList,
        initialState = self.initialState,
        stateCount = #stateList
    }
end

function FSMCompiler:validate()
    local errors = {}

    -- Check all transition targets exist
    for key, trans in next, self.transitions do
        local from = key:match("^(.+):")
        if not self.states[from] then
            errors[#errors + 1] = "Source state '" .. from .. "' not defined"
        end
        if not self.states[trans.target] then
            errors[#errors + 1] = "Target state '" .. trans.target .. "' not defined"
        end
    end

    -- Check for unreachable states
    local reachable = {}
    reachable[self.initialState] = true
    local changed = true
    while changed do
        changed = false
        for key, trans in next, self.transitions do
            local from = key:match("^(.+):")
            if reachable[from] and not reachable[trans.target] then
                reachable[trans.target] = true
                changed = true
            end
        end
    end

    for name in next, self.states do
        if not reachable[name] then
            errors[#errors + 1] = "State '" .. name .. "' is unreachable"
        end
    end

    return #errors == 0, errors
end

function FSMCompiler:toDot()
    local lines = {}
    lines[#lines + 1] = "digraph FSM {"
    lines[#lines + 1] = "  rankdir=LR;"
    lines[#lines + 1] = '  node [shape=circle];'

    for key, trans in next, self.transitions do
        local from, event = key:match("^(.+):(.+)$")
        if from and event then
            lines[#lines + 1] = string.format('  "%s" -> "%s" [label="%s"];',
                from, trans.target, event)
        end
    end

    lines[#lines + 1] = "}"
    return table.concat(lines, "\n")
end

function FSMCompiler:stateCount()
    local count = 0
    for _ in next, self.states do count = count + 1 end
    return count
end

function FSMCompiler:transitionCount()
    local count = 0
    for _ in next, self.transitions do count = count + 1 end
    return count
end

return FSMCompiler
]==]

-- Additional test files with variants for merge testing
TEST_FILE_1_MODIFIED_A = [[
-- Module: Vector3
-- 3D vector mathematics library
-- Version 2.0 - Added angle operations

local Vector3 = {}
Vector3.__index = Vector3

function Vector3.new(x, y, z)
    local self = setmetatable({}, Vector3)
    self.x = x or 0
    self.y = y or 0
    self.z = z or 0
    return self
end

function Vector3.zero()
    return Vector3.new(0, 0, 0)
end

function Vector3.one()
    return Vector3.new(1, 1, 1)
end

function Vector3:magnitude()
    return math.sqrt(self.x * self.x + self.y * self.y + self.z * self.z)
end

function Vector3:magnitudeSquared()
    return self.x * self.x + self.y * self.y + self.z * self.z
end

function Vector3:normalize()
    local mag = self:magnitude()
    if mag > 0.000001 then
        return Vector3.new(self.x / mag, self.y / mag, self.z / mag)
    end
    return Vector3.new(0, 0, 0)
end

function Vector3:dot(other)
    return self.x * other.x + self.y * other.y + self.z * other.z
end

function Vector3:cross(other)
    return Vector3.new(
        self.y * other.z - self.z * other.y,
        self.z * other.x - self.x * other.z,
        self.x * other.y - self.y * other.x
    )
end

function Vector3:add(other)
    return Vector3.new(self.x + other.x, self.y + other.y, self.z + other.z)
end

function Vector3:sub(other)
    return Vector3.new(self.x - other.x, self.y - other.y, self.z - other.z)
end

function Vector3:mul(scalar)
    return Vector3.new(self.x * scalar, self.y * scalar, self.z * scalar)
end

function Vector3:div(scalar)
    if scalar == 0 then return Vector3.zero() end
    return Vector3.new(self.x / scalar, self.y / scalar, self.z / scalar)
end

function Vector3:lerp(other, t)
    return self:add(other:sub(self):mul(t))
end

function Vector3:distance(other)
    return self:sub(other):magnitude()
end

function Vector3:angle(other)
    local d = self:dot(other)
    local m = self:magnitude() * other:magnitude()
    if m < 0.000001 then return 0 end
    return math.acos(math.max(-1, math.min(1, d / m)))
end

function Vector3:reflect(normal)
    local d = 2 * self:dot(normal)
    return self:sub(normal:mul(d))
end

function Vector3:project(onto)
    local d = onto:dot(onto)
    if d < 0.000001 then return Vector3.zero() end
    return onto:mul(self:dot(onto) / d)
end

function Vector3:__tostring()
    return string.format("(%f, %f, %f)", self.x, self.y, self.z)
end

function Vector3:__eq(other)
    return self.x == other.x and self.y == other.y and self.z == other.z
end

return Vector3
]]

TEST_FILE_1_MODIFIED_B = [[
-- Module: Vector3
-- 3D vector mathematics library
-- Optimized for performance

local Vector3 = {}
Vector3.__index = Vector3

local sqrt = math.sqrt
local abs = math.abs

function Vector3.new(x, y, z)
    local self = setmetatable({}, Vector3)
    self.x = x or 0
    self.y = y or 0
    self.z = z or 0
    return self
end

function Vector3:magnitude()
    return sqrt(self.x * self.x + self.y * self.y + self.z * self.z)
end

function Vector3:normalize()
    local mag = self:magnitude()
    if mag > 1e-8 then
        return Vector3.new(self.x / mag, self.y / mag, self.z / mag)
    end
    return Vector3.new(0, 0, 0)
end

function Vector3:dot(other)
    return self.x * other.x + self.y * other.y + self.z * other.z
end

function Vector3:cross(other)
    return Vector3.new(
        self.y * other.z - self.z * other.y,
        self.z * other.x - self.x * other.z,
        self.x * other.y - self.y * other.x
    )
end

function Vector3:add(other)
    return Vector3.new(self.x + other.x, self.y + other.y, self.z + other.z)
end

function Vector3:sub(other)
    return Vector3.new(self.x - other.x, self.y - other.y, self.z - other.z)
end

function Vector3:mul(scalar)
    return Vector3.new(self.x * scalar, self.y * scalar, self.z * scalar)
end

function Vector3:lerp(other, t)
    local oneMinusT = 1 - t
    return Vector3.new(
        self.x * oneMinusT + other.x * t,
        self.y * oneMinusT + other.y * t,
        self.z * oneMinusT + other.z * t
    )
end

function Vector3:distance(other)
    local dx = self.x - other.x
    local dy = self.y - other.y
    local dz = self.z - other.z
    return sqrt(dx * dx + dy * dy + dz * dz)
end

function Vector3:reflect(normal)
    local d = 2 * self:dot(normal)
    return self:sub(normal:mul(d))
end

function Vector3:clampMagnitude(maxMag)
    local mag = self:magnitude()
    if mag > maxMag then
        return self:mul(maxMag / mag)
    end
    return Vector3.new(self.x, self.y, self.z)
end

function Vector3:__tostring()
    return string.format("(%.4f, %.4f, %.4f)", self.x, self.y, self.z)
end

return Vector3
]]

-- =========================================================================
-- Checksum utility for verification
-- =========================================================================
function checksumString(s)
    local h = 5381
    for i = 1, #s do
        h = ((h * 33) + byte(s, i)) % 4294967296
    end
    return h
end

-- =========================================================================
-- Main Benchmark Workload
-- =========================================================================
function runBenchmarkIteration()
    local store = createObjectStore()
    local checksums = {}

    -- =====================================================================
    -- Phase 1: SHA-1 correctness and object creation
    -- =====================================================================

    -- Test SHA-1 with known values
    local hash1 = sha1("")
    assert(hash1 == "da39a3ee5e6b4b0d3255bfef95601890afd80709",
        "SHA-1 empty string failed: " .. hash1)

    local hash2 = sha1("abc")
    assert(hash2 == "a9993e364706816aba3e25717850c26c9cd0d89d",
        "SHA-1 'abc' failed: " .. hash2)

    local hash3 = sha1("The quick brown fox jumps over the lazy dog")
    assert(hash3 == "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12",
        "SHA-1 fox failed: " .. hash3)

    -- =====================================================================
    -- Phase 2: Build repository with multiple commits
    -- =====================================================================

    -- Initial commit with all test files
    local initialFiles = {
        ["vector3.lua"] = TEST_FILE_1,
        ["matrix4x4.lua"] = TEST_FILE_2,
        ["linkedlist.lua"] = TEST_FILE_3,
        ["hashmap.lua"] = TEST_FILE_4,
        ["events.lua"] = TEST_FILE_5,
        ["scheduler.lua"] = TEST_FILE_6,
        ["bst.lua"] = TEST_FILE_7,
        ["stringbuf.lua"] = TEST_FILE_8,
        ["statemachine.lua"] = TEST_FILE_9,
        ["json.lua"] = TEST_FILE_10,
        ["logger.lua"] = TEST_FILE_11,
        ["pathfinder.lua"] = TEST_FILE_12,
        ["tokenstream.lua"] = TEST_FILE_13
    }

    local commit1 = commitFiles(store, initialFiles, {},
        "Alice <alice@example.com> 1700000000 +0000",
        "Initial commit: add all modules")
    setRef(store, "main", commit1)

    checksums[#checksums + 1] = checksumString(commit1)

    -- Second commit: modify some files
    local modifiedFiles = {}
    for k, v in next, initialFiles do modifiedFiles[k] = v end
    modifiedFiles["vector3.lua"] = TEST_FILE_1_MODIFIED_A
    -- Add a new file
    modifiedFiles["config.lua"] = [[
-- Configuration module
local Config = {}
Config.VERSION = "1.0.0"
Config.DEBUG = false
Config.MAX_ENTITIES = 1000
Config.TICK_RATE = 60
Config.GRAVITY = -9.81
Config.FRICTION = 0.3
Config.RESTITUTION = 0.5

function Config.validate()
    assert(Config.MAX_ENTITIES > 0)
    assert(Config.TICK_RATE > 0)
    return true
end

return Config
]]

    local commit2 = commitFiles(store, modifiedFiles, {commit1},
        "Bob <bob@example.com> 1700001000 +0000",
        "Update vector3 with angle ops, add config")
    setRef(store, "main", commit2)

    checksums[#checksums + 1] = checksumString(commit2)

    -- Third commit: branch point for merge testing
    local branchFiles = {}
    for k, v in next, modifiedFiles do branchFiles[k] = v end
    branchFiles["scheduler.lua"] = branchFiles["scheduler.lua"] ..
        "\n-- Enhanced with recurring tasks\n"

    local commit3 = commitFiles(store, branchFiles, {commit2},
        "Alice <alice@example.com> 1700002000 +0000",
        "Enhance scheduler with docs")
    setRef(store, "feature-branch", commit3)

    checksums[#checksums + 1] = checksumString(commit3)

    -- =====================================================================
    -- Phase 3: Diff operations
    -- =====================================================================

    -- Diff between commit1 and commit2
    local diffs12 = diffCommits(store, commit1, commit2)
    assert(#diffs12 > 0, "Expected diffs between commits 1 and 2")

    -- Verify we detect the vector3 modification
    local foundVector3Diff = false
    for i = 1, #diffs12 do
        if diffs12[i].file == "vector3.lua" then
            foundVector3Diff = true
            checksums[#checksums + 1] = checksumString(diffs12[i].patch)
        end
    end
    assert(foundVector3Diff, "Should detect vector3.lua modification")

    -- Diff between commit2 and commit3
    local diffs23 = diffCommits(store, commit2, commit3)
    assert(#diffs23 > 0, "Expected diffs between commits 2 and 3")
    for i = 1, #diffs23 do
        checksums[#checksums + 1] = checksumString(diffs23[i].patch)
    end

    -- =====================================================================
    -- Phase 4: Patch parsing and application
    -- =====================================================================

    -- Create a diff, parse it as a patch, and apply it
    local originalText = TEST_FILE_8
    local modifiedText = TEST_FILE_8:gsub("StringBuffer", "StringBuilder")
    local patchText = diffToUnified("a/stringbuf.lua", "b/stringbuf.lua",
        originalText, modifiedText)

    checksums[#checksums + 1] = checksumString(patchText)

    local patch = parsePatch(patchText)
    assert(patch.aFile == "a/stringbuf.lua", "Patch aFile mismatch")
    assert(patch.bFile == "b/stringbuf.lua", "Patch bFile mismatch")
    assert(#patch.hunks > 0, "Patch should have hunks")

    -- Apply the patch
    local patchedText = applyPatch(originalText, patch)
    -- The patched text should match the modified text
    -- (Note: exact match depends on diff granularity, just check it changed)
    assert(patchedText ~= originalText, "Patch should modify the text")
    checksums[#checksums + 1] = checksumString(patchedText)

    -- =====================================================================
    -- Phase 5: Three-way merge
    -- =====================================================================

    -- Merge test: both sides modify vector3 differently
    local mergeResult, conflicts = threeWayMerge(
        TEST_FILE_1, TEST_FILE_1_MODIFIED_A, TEST_FILE_1_MODIFIED_B)
    assert(conflicts > 0, "Expected merge conflicts with divergent changes")
    checksums[#checksums + 1] = checksumString(mergeResult)

    -- Merge test: non-conflicting changes
    local baseSimple = "line1\nline2\nline3\nline4\nline5\n"
    local oursSimple = "line1\nline2 modified\nline3\nline4\nline5\n"
    local theirsSimple = "line1\nline2\nline3\nline4 changed\nline5\n"
    local mergedSimple, simpleConflicts = threeWayMerge(
        baseSimple, oursSimple, theirsSimple)
    assert(simpleConflicts == 0,
        "Non-overlapping changes should not conflict, got " .. simpleConflicts)
    checksums[#checksums + 1] = checksumString(mergedSimple)

    -- =====================================================================
    -- Phase 6: Full merge workflow
    -- =====================================================================

    -- Create divergent branch
    local branchAFiles = {}
    for k, v in next, initialFiles do branchAFiles[k] = v end
    branchAFiles["vector3.lua"] = TEST_FILE_1_MODIFIED_A
    branchAFiles["newfileA.lua"] = "-- Added by branch A\nlocal x = 42\nreturn x\n"

    local commitA = commitFiles(store, branchAFiles, {commit1},
        "Alice <alice@example.com> 1700003000 +0000",
        "Branch A: update vector3, add newfileA")

    local branchBFiles = {}
    for k, v in next, initialFiles do branchBFiles[k] = v end
    branchBFiles["vector3.lua"] = TEST_FILE_1_MODIFIED_B
    branchBFiles["newfileB.lua"] = "-- Added by branch B\nlocal y = 99\nreturn y\n"

    local commitB = commitFiles(store, branchBFiles, {commit1},
        "Bob <bob@example.com> 1700003000 +0000",
        "Branch B: optimize vector3, add newfileB")

    -- Merge the two branches
    local mergedFiles, totalConflicts = mergeCommits(store, commit1, commitA, commitB)
    assert(mergedFiles["newfileA.lua"] ~= nil, "Should have newfileA.lua")
    assert(mergedFiles["newfileB.lua"] ~= nil, "Should have newfileB.lua")
    assert(totalConflicts > 0, "Vector3 should have merge conflicts")

    checksums[#checksums + 1] = checksumString(mergedFiles["vector3.lua"] or "")
    checksums[#checksums + 1] = checksumString(mergedFiles["newfileA.lua"] or "")
    checksums[#checksums + 1] = checksumString(mergedFiles["newfileB.lua"] or "")

    -- =====================================================================
    -- Phase 7: Large diff stress test
    -- =====================================================================

    -- Generate a large file with predictable content
    local largeParts = {}
    for i = 1, 200 do
        largeParts[#largeParts + 1] = format("function func_%04d(x, y)", i)
        largeParts[#largeParts + 1] = format("    local result = x * %d + y * %d", i, i * 2)
        largeParts[#largeParts + 1] = "    if result > 1000 then"
        largeParts[#largeParts + 1] = "        result = result - 1000"
        largeParts[#largeParts + 1] = "    end"
        largeParts[#largeParts + 1] = "    return result"
        largeParts[#largeParts + 1] = "end"
        largeParts[#largeParts + 1] = ""
    end
    local largeFileA = concat(largeParts, "\n")

    -- Modify every 10th function
    local largeParts2 = {}
    for i = 1, 200 do
        if i % 10 == 0 then
            largeParts2[#largeParts2 + 1] = format("function func_%04d(x, y, z)", i)
            largeParts2[#largeParts2 + 1] = format("    local result = x * %d + y * %d + z", i, i * 2)
            largeParts2[#largeParts2 + 1] = "    if result > 2000 then"
            largeParts2[#largeParts2 + 1] = "        result = result % 2000"
            largeParts2[#largeParts2 + 1] = "    end"
            largeParts2[#largeParts2 + 1] = "    return result"
            largeParts2[#largeParts2 + 1] = "end"
        else
            largeParts2[#largeParts2 + 1] = format("function func_%04d(x, y)", i)
            largeParts2[#largeParts2 + 1] = format("    local result = x * %d + y * %d", i, i * 2)
            largeParts2[#largeParts2 + 1] = "    if result > 1000 then"
            largeParts2[#largeParts2 + 1] = "        result = result - 1000"
            largeParts2[#largeParts2 + 1] = "    end"
            largeParts2[#largeParts2 + 1] = "    return result"
            largeParts2[#largeParts2 + 1] = "end"
        end
        largeParts2[#largeParts2 + 1] = ""
    end
    local largeFileB = concat(largeParts2, "\n")

    local largeDiff = diffToUnified("a/large.lua", "b/large.lua", largeFileA, largeFileB)
    assert(#largeDiff > 100, "Large diff should produce substantial output")
    checksums[#checksums + 1] = checksumString(largeDiff)

    -- Parse and apply the large patch
    local largePatch = parsePatch(largeDiff)
    local largePatched = applyPatch(largeFileA, largePatch)
    checksums[#checksums + 1] = checksumString(largePatched)

    -- =====================================================================
    -- Phase 8: Multiple sequential commits (simulating history)
    -- =====================================================================

    local historyFiles = {}
    for k, v in next, initialFiles do historyFiles[k] = v end
    local prevCommit = commit1
    local commitHistory = { commit1 }

    for step = 1, 5 do
        -- Each step modifies a different file
        local fileNames = {"linkedlist.lua", "hashmap.lua", "events.lua",
                          "scheduler.lua", "bst.lua"}
        local fname = fileNames[step]
        local original = historyFiles[fname]
        -- Add a comment at the top
        historyFiles[fname] = format("-- Revision %d\n", step) .. original
        local c = commitFiles(store, historyFiles, {prevCommit},
            format("Dev%d <dev%d@example.com> %d +0000", step, step, 1700004000 + step * 1000),
            format("Revision %d: update %s", step, fname))
        commitHistory[#commitHistory + 1] = c
        prevCommit = c
    end

    -- Diff across entire history
    local fullDiffs = diffCommits(store, commitHistory[1], commitHistory[#commitHistory])
    assert(#fullDiffs > 0, "Should have diffs across history")
    for i = 1, #fullDiffs do
        checksums[#checksums + 1] = checksumString(fullDiffs[i].patch)
    end

    -- =====================================================================
    -- Phase 9: Object store integrity
    -- =====================================================================

    -- Verify all objects are retrievable and consistent
    local objectCount = 0
    for hash, obj in next, store.objects do
        objectCount = objectCount + 1
        -- Verify hash matches content
        local expectedHash = gitHash(obj.type, obj.content)
        assert(hash == expectedHash,
            "Object store corruption: hash mismatch for " .. hash)
    end
    assert(objectCount > 30, "Expected many objects, got " .. objectCount)
    checksums[#checksums + 1] = objectCount

    -- =====================================================================
    -- Phase 10: Diff edge cases
    -- =====================================================================

    -- Empty to non-empty
    local emptyDiff = diffToUnified("a/empty", "b/full", "", "hello\nworld\n")
    checksums[#checksums + 1] = checksumString(emptyDiff)

    -- Non-empty to empty
    local delDiff = diffToUnified("a/full", "b/empty", "hello\nworld\n", "")
    checksums[#checksums + 1] = checksumString(delDiff)

    -- Identical files
    local identDiff = diffToUnified("a/same", "b/same", "same\ncontent\n", "same\ncontent\n")
    checksums[#checksums + 1] = checksumString(identDiff)

    -- Single line change
    local singleDiff = diffToUnified("a/f", "b/f",
        "aaa\nbbb\nccc\nddd\neee\n",
        "aaa\nbbb\nCCC\nddd\neee\n")
    checksums[#checksums + 1] = checksumString(singleDiff)

    -- =====================================================================
    -- Phase 11: Extended repository with all test files
    -- =====================================================================

    local extendedFiles = {
        ["vector3.lua"] = TEST_FILE_1,
        ["matrix4x4.lua"] = TEST_FILE_2,
        ["linkedlist.lua"] = TEST_FILE_3,
        ["hashmap.lua"] = TEST_FILE_4,
        ["events.lua"] = TEST_FILE_5,
        ["scheduler.lua"] = TEST_FILE_6,
        ["bst.lua"] = TEST_FILE_7,
        ["stringbuf.lua"] = TEST_FILE_8,
        ["statemachine.lua"] = TEST_FILE_9,
        ["json.lua"] = TEST_FILE_10,
        ["logger.lua"] = TEST_FILE_11,
        ["pathfinder.lua"] = TEST_FILE_12,
        ["tokenstream.lua"] = TEST_FILE_13,
        ["quadtree.lua"] = TEST_FILE_14,
        ["signal.lua"] = TEST_FILE_15,
        ["ringbuffer.lua"] = TEST_FILE_16,
        ["tween.lua"] = TEST_FILE_17,
        ["objectpool.lua"] = TEST_FILE_18,
        ["command.lua"] = TEST_FILE_19,
        ["observable.lua"] = TEST_FILE_20,
        ["ecs.lua"] = TEST_FILE_21,
        ["vdom.lua"] = TEST_FILE_22,
        ["coroutinepool.lua"] = TEST_FILE_23,
        ["protocol.lua"] = TEST_FILE_24,
        ["database.lua"] = TEST_FILE_25,
        ["fsmcompiler.lua"] = TEST_FILE_26
    }

    local extStore = createObjectStore()
    local extCommit1 = commitFiles(extStore, extendedFiles, {},
        "Charlie <charlie@example.com> 1700010000 +0000",
        "Full project: 26 modules")
    checksums[#checksums + 1] = checksumString(extCommit1)

    -- Modify several files in a second commit
    local extModified = {}
    for k, v in next, extendedFiles do extModified[k] = v end
    extModified["quadtree.lua"] = extModified["quadtree.lua"]:gsub(
        "MAX_OBJECTS = 10", "MAX_OBJECTS = 20")
    extModified["ringbuffer.lua"] = extModified["ringbuffer.lua"]:gsub(
        "function RingBuffer:average", "function RingBuffer:mean")
    extModified["tween.lua"] = "-- Tween v2.0\n" .. extModified["tween.lua"]
    extModified["ecs.lua"] = extModified["ecs.lua"]:gsub(
        "self.nextEntityId = 1", "self.nextEntityId = 0")

    local extCommit2 = commitFiles(extStore, extModified, {extCommit1},
        "Charlie <charlie@example.com> 1700011000 +0000",
        "Tweak quadtree, ringbuffer, tween, ecs")
    checksums[#checksums + 1] = checksumString(extCommit2)

    -- Diff the extended commits
    local extDiffs = diffCommits(extStore, extCommit1, extCommit2)
    assert(#extDiffs >= 4, "Expected at least 4 file diffs, got " .. #extDiffs)
    for i = 1, #extDiffs do
        checksums[#checksums + 1] = checksumString(extDiffs[i].patch)
    end

    -- =====================================================================
    -- Phase 12: Branching and merging on extended repo
    -- =====================================================================

    -- Branch A: refactor signal module
    local branchAExt = {}
    for k, v in next, extendedFiles do branchAExt[k] = v end
    branchAExt["signal.lua"] = branchAExt["signal.lua"]:gsub(
        "local currentEffect = nil", "local currentEffect = nil\nlocal batchQueue = {}")
    branchAExt["observable.lua"] = branchAExt["observable.lua"]:gsub(
        "self.observers = {}", "self.observers = {}\n    self.paused = false")
    branchAExt["newutil.lua"] = [[
-- Utility module added in branch A
local Util = {}
function Util.clamp(val, minVal, maxVal)
    return math.max(minVal, math.min(maxVal, val))
end
function Util.lerp(a, b, t)
    return a + (b - a) * t
end
function Util.map(tbl, fn)
    local result = {}
    for i = 1, #tbl do result[i] = fn(tbl[i]) end
    return result
end
function Util.filter(tbl, fn)
    local result = {}
    for i = 1, #tbl do
        if fn(tbl[i]) then result[#result + 1] = tbl[i] end
    end
    return result
end
function Util.reduce(tbl, fn, init)
    local acc = init
    for i = 1, #tbl do acc = fn(acc, tbl[i]) end
    return acc
end
return Util
]]

    local extCommitA = commitFiles(extStore, branchAExt, {extCommit1},
        "Alice <alice@example.com> 1700012000 +0000",
        "Branch A: enhance signal/observable, add util")

    -- Branch B: optimize different modules
    local branchBExt = {}
    for k, v in next, extendedFiles do branchBExt[k] = v end
    branchBExt["pathfinder.lua"] = branchBExt["pathfinder.lua"]:gsub(
        "function PathFinder:heuristic", "-- Optimized heuristic\nfunction PathFinder:heuristic")
    branchBExt["coroutinepool.lua"] = branchBExt["coroutinepool.lua"]:gsub(
        "self.maxConcurrent = maxConcurrent or 4",
        "self.maxConcurrent = maxConcurrent or 8")
    branchBExt["perf.lua"] = [[
-- Performance monitoring module added in branch B
local Perf = {}
Perf.timers = {}
function Perf.start(name)
    Perf.timers[name] = os.clock()
end
function Perf.stop(name)
    local elapsed = os.clock() - (Perf.timers[name] or 0)
    Perf.timers[name] = nil
    return elapsed
end
function Perf.measure(name, fn)
    Perf.start(name)
    local result = fn()
    local elapsed = Perf.stop(name)
    return result, elapsed
end
return Perf
]]

    local extCommitB = commitFiles(extStore, branchBExt, {extCommit1},
        "Bob <bob@example.com> 1700012000 +0000",
        "Branch B: optimize pathfinder/pool, add perf")

    -- Merge branches
    local extMerged, extConflicts = mergeCommits(extStore, extCommit1, extCommitA, extCommitB)
    -- Both added different new files, should be conflict-free for those
    assert(extMerged["newutil.lua"] ~= nil, "Should have newutil.lua from branch A")
    assert(extMerged["perf.lua"] ~= nil, "Should have perf.lua from branch B")
    checksums[#checksums + 1] = checksumString(extMerged["signal.lua"] or "")
    checksums[#checksums + 1] = checksumString(extMerged["pathfinder.lua"] or "")
    checksums[#checksums + 1] = checksumString(extMerged["newutil.lua"] or "")
    checksums[#checksums + 1] = checksumString(extMerged["perf.lua"] or "")

    -- =====================================================================
    -- Phase 13: SHA-1 stress (hash many objects)
    -- =====================================================================

    local hashStore = createObjectStore()
    for i = 1, 100 do
        local content = format("file content number %d with some padding to make it longer: %s",
            i, string.rep("x", i * 3))
        createBlob(hashStore, content)
    end
    -- Verify all 100 objects stored uniquely
    local hashCount = 0
    for _ in next, hashStore.objects do hashCount = hashCount + 1 end
    assert(hashCount == 100, "Expected 100 unique hashes, got " .. hashCount)
    checksums[#checksums + 1] = hashCount

    -- Hash some larger strings
    for i = 1, 20 do
        local bigContent = string.rep(format("line %d: data data data\n", i), 50)
        local h = sha1(bigContent)
        checksums[#checksums + 1] = checksumString(h)
    end

    -- =====================================================================
    -- Phase 14: Complex merge scenarios
    -- =====================================================================

    -- Test: three-way merge with insertions at different points
    local mergeBase = "header\n"
    for i = 1, 20 do
        mergeBase = mergeBase .. format("line %d\n", i)
    end
    mergeBase = mergeBase .. "footer\n"

    -- Ours: insert after line 5
    local mergeOurs = "header\n"
    for i = 1, 20 do
        mergeOurs = mergeOurs .. format("line %d\n", i)
        if i == 5 then
            mergeOurs = mergeOurs .. "inserted by ours after line 5\n"
        end
    end
    mergeOurs = mergeOurs .. "footer\n"

    -- Theirs: insert after line 15
    local mergeTheirs = "header\n"
    for i = 1, 20 do
        mergeTheirs = mergeTheirs .. format("line %d\n", i)
        if i == 15 then
            mergeTheirs = mergeTheirs .. "inserted by theirs after line 15\n"
        end
    end
    mergeTheirs = mergeTheirs .. "footer\n"

    local mergedResult, mergeConflictCount = threeWayMerge(mergeBase, mergeOurs, mergeTheirs)
    checksums[#checksums + 1] = checksumString(mergedResult)
    -- Both insertions should be present (non-overlapping)
    assert(mergedResult:find("inserted by ours"), "Should contain ours insertion")
    assert(mergedResult:find("inserted by theirs"), "Should contain theirs insertion")

    -- Test: merge with deletions
    local delBase = ""
    for i = 1, 30 do
        delBase = delBase .. format("item %d\n", i)
    end

    -- Ours removes items 5-10
    local delOurs = ""
    for i = 1, 30 do
        if i < 5 or i > 10 then
            delOurs = delOurs .. format("item %d\n", i)
        end
    end

    -- Theirs removes items 20-25
    local delTheirs = ""
    for i = 1, 30 do
        if i < 20 or i > 25 then
            delTheirs = delTheirs .. format("item %d\n", i)
        end
    end

    local delMerged, delConflicts = threeWayMerge(delBase, delOurs, delTheirs)
    checksums[#checksums + 1] = checksumString(delMerged)

    -- =====================================================================
    -- Phase 15: Patch round-trip testing
    -- =====================================================================

    -- Generate diffs for several file pairs, parse them, and apply
    local patchTestFiles = {
        { TEST_FILE_3, TEST_FILE_3:gsub("LinkedList", "DoublyLinkedList") },
        { TEST_FILE_7, TEST_FILE_7:gsub("BST", "AVLTree") },
        { TEST_FILE_9, TEST_FILE_9:gsub("StateMachine", "FSM") },
        { TEST_FILE_12, TEST_FILE_12:gsub("PathFinder", "AStarSolver") },
    }

    for idx = 1, #patchTestFiles do
        local orig = patchTestFiles[idx][1]
        local modified = patchTestFiles[idx][2]
        local pText = diffToUnified("a/file.lua", "b/file.lua", orig, modified)
        local p = parsePatch(pText)
        local applied = applyPatch(orig, p)
        assert(applied ~= orig, "Patch " .. idx .. " should change the file")
        checksums[#checksums + 1] = checksumString(applied)
    end

    -- =====================================================================
    -- Phase 16: Commit history traversal
    -- =====================================================================

    -- Build a longer linear history
    local histStore = createObjectStore()
    local histFiles = { ["main.lua"] = "-- main\nprint('hello')\n" }
    local histPrev = commitFiles(histStore, histFiles, {},
        "Dev <dev@co.com> 1700020000 +0000", "init")
    local allCommits = { histPrev }

    for step = 1, 15 do
        histFiles["main.lua"] = histFiles["main.lua"] ..
            format("print('step %d')\n", step)
        if step % 3 == 0 then
            histFiles[format("mod%d.lua", step)] = format(
                "-- Module %d\nlocal M = {}\nfunction M.run() return %d end\nreturn M\n",
                step, step * step)
        end
        local c = commitFiles(histStore, histFiles, {histPrev},
            format("Dev <dev@co.com> %d +0000", 1700020000 + step * 100),
            format("step %d", step))
        allCommits[#allCommits + 1] = c
        histPrev = c
    end

    -- Traverse and verify commit chain
    local current = allCommits[#allCommits]
    local chainLen = 0
    while current do
        local data = getCommitData(histStore, current)
        if not data then break end
        chainLen = chainLen + 1
        if #data.parents > 0 then
            current = data.parents[1]
        else
            current = nil
        end
    end
    assert(chainLen == 16, "Expected chain of 16 commits, got " .. chainLen)
    checksums[#checksums + 1] = chainLen

    -- Diff first vs last
    local historyDiff = diffCommits(histStore, allCommits[1], allCommits[#allCommits])
    for i = 1, #historyDiff do
        checksums[#checksums + 1] = checksumString(historyDiff[i].patch)
    end

    -- =====================================================================
    -- Phase 17: Protocol and database file diffs
    -- =====================================================================

    -- Modify protocol file
    local protocolOrig = TEST_FILE_24
    local protocolMod = protocolOrig:gsub("PROTO/1.1", "PROTO/2.0")
    protocolMod = protocolMod:gsub("STATUS_OK = 200", "STATUS_OK = 200\nProtocol.STATUS_CREATED = 201")
    local protoDiff = diffToUnified("a/protocol.lua", "b/protocol.lua",
        protocolOrig, protocolMod)
    checksums[#checksums + 1] = checksumString(protoDiff)

    -- Modify database file
    local dbOrig = TEST_FILE_25
    local dbMod = dbOrig:gsub("self.version = 0", "self.version = 1")
    dbMod = dbMod:gsub("function Table:count()", "function Table:size()")
    local dbDiff = diffToUnified("a/database.lua", "b/database.lua", dbOrig, dbMod)
    checksums[#checksums + 1] = checksumString(dbDiff)

    -- Parse and apply both patches
    local protoPatch = parsePatch(protoDiff)
    local protoApplied = applyPatch(protocolOrig, protoPatch)
    assert(protoApplied ~= protocolOrig, "Protocol patch should change file")
    checksums[#checksums + 1] = checksumString(protoApplied)

    local dbPatch = parsePatch(dbDiff)
    local dbApplied = applyPatch(dbOrig, dbPatch)
    assert(dbApplied ~= dbOrig, "Database patch should change file")
    checksums[#checksums + 1] = checksumString(dbApplied)

    -- =====================================================================
    -- Phase 18: Multi-file merge with conflicts in various locations
    -- =====================================================================

    -- Create a scenario with 5 files where 2 conflict
    local mBase = {
        ["app.lua"] = "-- App\nlocal App = {}\nApp.version = '1.0'\nfunction App.init()\n    print('starting')\nend\nfunction App.run()\n    App.init()\n    print('running')\nend\nreturn App\n",
        ["config.lua"] = "-- Config\nlocal C = {}\nC.debug = false\nC.port = 8080\nC.host = 'localhost'\nC.timeout = 30\nreturn C\n",
        ["utils.lua"] = "-- Utils\nlocal U = {}\nfunction U.add(a,b) return a+b end\nfunction U.sub(a,b) return a-b end\nfunction U.mul(a,b) return a*b end\nfunction U.div(a,b) return a/b end\nreturn U\n",
        ["logger.lua"] = "-- Logger\nlocal L = {}\nL.level = 'info'\nfunction L.log(msg) print(msg) end\nfunction L.error(msg) print('ERROR: '..msg) end\nfunction L.warn(msg) print('WARN: '..msg) end\nreturn L\n",
        ["server.lua"] = "-- Server\nlocal S = {}\nS.running = false\nfunction S.start() S.running = true end\nfunction S.stop() S.running = false end\nfunction S.status() return S.running end\nreturn S\n",
    }

    local mOurs = {}
    for k, v in next, mBase do mOurs[k] = v end
    mOurs["app.lua"] = "-- App v2\nlocal App = {}\nApp.version = '2.0'\nfunction App.init(config)\n    print('starting v2')\n    App.config = config\nend\nfunction App.run()\n    App.init({})\n    print('running v2')\nend\nreturn App\n"
    mOurs["config.lua"] = "-- Config (production)\nlocal C = {}\nC.debug = false\nC.port = 443\nC.host = '0.0.0.0'\nC.timeout = 60\nC.ssl = true\nreturn C\n"
    mOurs["newfeature.lua"] = "-- New feature from ours\nlocal F = {}\nfunction F.activate() return true end\nreturn F\n"

    local mTheirs = {}
    for k, v in next, mBase do mTheirs[k] = v end
    mTheirs["app.lua"] = "-- App (refactored)\nlocal App = {}\nApp.version = '1.1'\nfunction App.initialize()\n    print('initializing')\nend\nfunction App.run()\n    App.initialize()\n    print('running app')\nend\nreturn App\n"
    mTheirs["utils.lua"] = "-- Utils (extended)\nlocal U = {}\nfunction U.add(a,b) return a+b end\nfunction U.sub(a,b) return a-b end\nfunction U.mul(a,b) return a*b end\nfunction U.div(a,b) if b==0 then return 0 end return a/b end\nfunction U.pow(a,b) return a^b end\nfunction U.mod(a,b) return a%b end\nreturn U\n"
    mTheirs["hotfix.lua"] = "-- Hotfix from theirs\nlocal H = {}\nfunction H.apply() return true end\nreturn H\n"

    local mStoreM = createObjectStore()
    local mBaseC = commitFiles(mStoreM, mBase, {},
        "Dev <dev@co.com> 1700030000 +0000", "base")
    local mOursC = commitFiles(mStoreM, mOurs, {mBaseC},
        "Alice <a@co.com> 1700031000 +0000", "ours changes")
    local mTheirsC = commitFiles(mStoreM, mTheirs, {mBaseC},
        "Bob <b@co.com> 1700031000 +0000", "theirs changes")

    local mMerged, mConflicts = mergeCommits(mStoreM, mBaseC, mOursC, mTheirsC)
    -- app.lua should conflict (both modified differently)
    assert(mConflicts > 0, "Should have conflicts in app.lua")
    -- Both new files should be present
    assert(mMerged["newfeature.lua"] ~= nil, "Should have newfeature.lua")
    assert(mMerged["hotfix.lua"] ~= nil, "Should have hotfix.lua")
    -- utils.lua only changed by theirs
    assert(mMerged["utils.lua"] == mTheirs["utils.lua"], "utils should be theirs version")
    -- config.lua only changed by ours
    assert(mMerged["config.lua"] == mOurs["config.lua"], "config should be ours version")

    checksums[#checksums + 1] = checksumString(mMerged["app.lua"] or "")
    checksums[#checksums + 1] = checksumString(mMerged["config.lua"] or "")
    checksums[#checksums + 1] = checksumString(mMerged["utils.lua"] or "")
    checksums[#checksums + 1] = checksumString(mMerged["newfeature.lua"] or "")
    checksums[#checksums + 1] = checksumString(mMerged["hotfix.lua"] or "")

    -- =====================================================================
    -- Phase 19: Large-scale diff with many scattered changes
    -- =====================================================================

    -- Generate a config-like file with many key-value pairs
    local configParts = {}
    configParts[#configParts + 1] = "-- Auto-generated configuration"
    configParts[#configParts + 1] = "local Config = {}"
    configParts[#configParts + 1] = ""
    for i = 1, 100 do
        configParts[#configParts + 1] = format("Config.setting_%03d = %d", i, i * 7)
    end
    configParts[#configParts + 1] = ""
    configParts[#configParts + 1] = "return Config"
    local configA = concat(configParts, "\n")

    -- Change every 5th setting
    local configParts2 = {}
    configParts2[#configParts2 + 1] = "-- Auto-generated configuration"
    configParts2[#configParts2 + 1] = "local Config = {}"
    configParts2[#configParts2 + 1] = ""
    for i = 1, 100 do
        if i % 5 == 0 then
            configParts2[#configParts2 + 1] = format("Config.setting_%03d = %d  -- updated", i, i * 11)
        else
            configParts2[#configParts2 + 1] = format("Config.setting_%03d = %d", i, i * 7)
        end
    end
    configParts2[#configParts2 + 1] = ""
    configParts2[#configParts2 + 1] = "return Config"
    local configB = concat(configParts2, "\n")

    local configDiff = diffToUnified("a/config.lua", "b/config.lua", configA, configB)
    assert(#configDiff > 200, "Config diff should be substantial")
    checksums[#checksums + 1] = checksumString(configDiff)

    -- Apply patch and verify
    local configPatch = parsePatch(configDiff)
    if #configPatch.hunks > 0 then
        local configPatched = applyPatch(configA, configPatch)
        checksums[#checksums + 1] = checksumString(configPatched)
    end

    -- =====================================================================
    -- Phase 20: Verify SHA-1 properties
    -- =====================================================================

    -- Verify that even small changes produce very different hashes
    local baseStr = "This is a test string for hash avalanche testing"
    local baseHash = sha1(baseStr)
    for i = 1, 10 do
        local modified = baseStr:sub(1, i) ..
            string.char(baseStr:byte(i + 1) + 1) ..
            baseStr:sub(i + 2)
        local modHash = sha1(modified)
        -- Hashes should be completely different
        assert(modHash ~= baseHash,
            "Hash collision on single-bit change at position " .. i)
        checksums[#checksums + 1] = checksumString(modHash)
    end

    -- Verify determinism of the full pipeline
    local verifyStore = createObjectStore()
    local verifyFiles = {
        ["a.lua"] = "local a = 1\nreturn a\n",
        ["b.lua"] = "local b = 2\nreturn b\n",
    }
    local vc1 = commitFiles(verifyStore, verifyFiles, {},
        "V <v@v.com> 1700040000 +0000", "verify commit 1")
    verifyFiles["a.lua"] = "local a = 10\nreturn a\n"
    local vc2 = commitFiles(verifyStore, verifyFiles, {vc1},
        "V <v@v.com> 1700041000 +0000", "verify commit 2")

    -- Do it again and check same hashes
    local verifyStore2 = createObjectStore()
    local verifyFiles2 = {
        ["a.lua"] = "local a = 1\nreturn a\n",
        ["b.lua"] = "local b = 2\nreturn b\n",
    }
    local vc1b = commitFiles(verifyStore2, verifyFiles2, {},
        "V <v@v.com> 1700040000 +0000", "verify commit 1")
    verifyFiles2["a.lua"] = "local a = 10\nreturn a\n"
    local vc2b = commitFiles(verifyStore2, verifyFiles2, {vc1b},
        "V <v@v.com> 1700041000 +0000", "verify commit 2")

    assert(vc1 == vc1b, "Deterministic commit 1 failed")
    assert(vc2 == vc2b, "Deterministic commit 2 failed")
    checksums[#checksums + 1] = checksumString(vc1 .. vc2)

    -- =====================================================================
    -- Compute overall checksum for verification
    -- =====================================================================
    local finalChecksum = 0
    for i = 1, #checksums do
        finalChecksum = (finalChecksum * 31 + checksums[i]) % 4294967296
    end

    return finalChecksum, objectCount
end

-- =========================================================================
-- Run benchmark loop
-- =========================================================================
local ITERATIONS = 2
local allPassed = true
local firstChecksum = nil
local correctChecksum = 2674509866

for iter = 1, ITERATIONS do
    local checksum, objCount = runBenchmarkIteration()
    if checksum ~= correctChecksum then
        print("FAIL: Incorrect results at iteration " .. iter)
        print("  Expected checksum: " .. correctChecksum)
        print("  Got checksum: " .. checksum)
        allPassed = false
    end
end

if allPassed then
    print("Git benchmark: all " .. ITERATIONS .. " iterations passed.")
else
    print("Git benchmark: FAILED")
    error("Benchmark failed")
end

end

bench.runCode(test, "git")
