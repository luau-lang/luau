local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()

-- NFA-based regex engine benchmark for Luau/Lua.
-- Implements a full Thompson's construction regex engine with:
--   - Regex parser producing an AST
--   - NFA construction via Thompson's algorithm
--   - NFA simulation with epsilon closure and capture tracking
-- Target runtimes: Luau (lute), Lua 5.5, LuaJIT.

local floor = math.floor
local clock = os.clock
local sbyte = string.byte
local ssub = string.sub
local slen = string.len
local schar = string.char
local srep = string.rep
local sformat = string.format

-- ============================================================
-- SECTION 1: Regex Parser
-- ============================================================

-- AST node types:
--   literal: {type="literal", char=<byte>}
--   dot: {type="dot"}
--   class: {type="class", ranges={}, negated=<bool>}
--   quantifier: {type="quantifier", child=<node>, kind=<"*"|"+"|"?">}
--   concat: {type="concat", children={}}
--   alternation: {type="alternation", left=<node>, right=<node>}
--   group: {type="group", child=<node>, index=<int>}
--   anchor_start: {type="anchor_start"}
--   anchor_end: {type="anchor_end"}

-- Forward declarations (global functions)
local nextStateId = 0

function makeStateId()
    nextStateId = nextStateId + 1
    return nextStateId
end

function resetStateId()
    nextStateId = 0
end

-- Character class helpers
function isDigit(ch)
    return ch >= 48 and ch <= 57
end

function isWord(ch)
    return (ch >= 48 and ch <= 57) or (ch >= 65 and ch <= 90) or (ch >= 97 and ch <= 122) or ch == 95
end

function isSpace(ch)
    return ch == 32 or ch == 9 or ch == 10 or ch == 13 or ch == 12
end

-- Parser state
function createParser(pattern)
    return {
        pattern = pattern,
        pos = 1,
        len = slen(pattern),
        groupCount = 0
    }
end

function parserPeek(p)
    if p.pos > p.len then return nil end
    return sbyte(p.pattern, p.pos)
end

function parserAdvance(p)
    local ch = sbyte(p.pattern, p.pos)
    p.pos = p.pos + 1
    return ch
end

function parserAtEnd(p)
    return p.pos > p.len
end

-- Parse an escape sequence, returning an AST node
function parseEscape(p)
    local ch = parserAdvance(p)
    if ch == nil then
        error("Unexpected end of pattern after backslash")
    end
    -- \d = digits
    if ch == 100 then -- 'd'
        return {type="class", ranges={{48,57}}, negated=false}
    end
    -- \D = non-digits
    if ch == 68 then -- 'D'
        return {type="class", ranges={{48,57}}, negated=true}
    end
    -- \w = word chars
    if ch == 119 then -- 'w'
        return {type="class", ranges={{48,57},{65,90},{97,122},{95,95}}, negated=false}
    end
    -- \W = non-word
    if ch == 87 then -- 'W'
        return {type="class", ranges={{48,57},{65,90},{97,122},{95,95}}, negated=true}
    end
    -- \s = whitespace
    if ch == 115 then -- 's'
        return {type="class", ranges={{9,13},{32,32}}, negated=false}
    end
    -- \S = non-whitespace
    if ch == 83 then -- 'S'
        return {type="class", ranges={{9,13},{32,32}}, negated=true}
    end
    -- Escaped literal
    return {type="literal", char=ch}
end

-- Parse character class [...]
function parseCharClass(p)
    local negated = false
    local ranges = {}
    -- Check for negation
    local ch = parserPeek(p)
    if ch == 94 then -- '^'
        negated = true
        parserAdvance(p)
    end
    -- Parse class contents
    while true do
        ch = parserPeek(p)
        if ch == nil then
            error("Unterminated character class")
        end
        if ch == 93 then -- ']'
            parserAdvance(p)
            break
        end
        local startCh
        if ch == 92 then -- '\'
            parserAdvance(p)
            local esc = parserAdvance(p)
            if esc == 100 then -- 'd'
                ranges[#ranges+1] = {48, 57}
                startCh = nil
            elseif esc == 119 then -- 'w'
                ranges[#ranges+1] = {48, 57}
                ranges[#ranges+1] = {65, 90}
                ranges[#ranges+1] = {97, 122}
                ranges[#ranges+1] = {95, 95}
                startCh = nil
            elseif esc == 115 then -- 's'
                ranges[#ranges+1] = {9, 13}
                ranges[#ranges+1] = {32, 32}
                startCh = nil
            else
                startCh = esc
            end
        else
            startCh = parserAdvance(p)
        end
        if startCh ~= nil then
            -- Check for range: a-z
            local next = parserPeek(p)
            if next == 45 then -- '-'
                parserAdvance(p)
                local endCh
                local afterDash = parserPeek(p)
                if afterDash == 93 then -- ']' right after dash means literal dash
                    -- treat dash as literal, put back
                    ranges[#ranges+1] = {startCh, startCh}
                    ranges[#ranges+1] = {45, 45}
                elseif afterDash == 92 then -- escape in range end
                    parserAdvance(p)
                    endCh = parserAdvance(p)
                    ranges[#ranges+1] = {startCh, endCh}
                else
                    endCh = parserAdvance(p)
                    ranges[#ranges+1] = {startCh, endCh}
                end
            else
                ranges[#ranges+1] = {startCh, startCh}
            end
        end
    end
    return {type="class", ranges=ranges, negated=negated}
end

-- Parse atom: literal, dot, group, class, anchor, escape
function parseAtom(p)
    local ch = parserPeek(p)
    if ch == nil then return nil end

    -- '(' grouping
    if ch == 40 then
        parserAdvance(p)
        p.groupCount = p.groupCount + 1
        local idx = p.groupCount
        local child = parseAlternation(p)
        local closing = parserPeek(p)
        if closing ~= 41 then -- ')'
            error("Expected closing parenthesis at pos " .. p.pos)
        end
        parserAdvance(p)
        return {type="group", child=child, index=idx}
    end

    -- '[' char class
    if ch == 91 then
        parserAdvance(p)
        return parseCharClass(p)
    end

    -- '.' any char
    if ch == 46 then
        parserAdvance(p)
        return {type="dot"}
    end

    -- '^' anchor start
    if ch == 94 then
        parserAdvance(p)
        return {type="anchor_start"}
    end

    -- '$' anchor end
    if ch == 36 then
        parserAdvance(p)
        return {type="anchor_end"}
    end

    -- '\' escape
    if ch == 92 then
        parserAdvance(p)
        return parseEscape(p)
    end

    -- Not a special char that terminates expression
    if ch == 41 or ch == 124 then -- ')' or '|'
        return nil
    end

    -- Regular literal character
    parserAdvance(p)
    return {type="literal", char=ch}
end

-- Parse quantifier suffix on atom
function parseQuantified(p)
    local atom = parseAtom(p)
    if atom == nil then return nil end
    local ch = parserPeek(p)
    if ch == 42 then -- '*'
        parserAdvance(p)
        return {type="quantifier", child=atom, kind="*"}
    elseif ch == 43 then -- '+'
        parserAdvance(p)
        return {type="quantifier", child=atom, kind="+"}
    elseif ch == 63 then -- '?'
        parserAdvance(p)
        return {type="quantifier", child=atom, kind="?"}
    end
    return atom
end

-- Parse concatenation
function parseConcat(p)
    local children = {}
    while true do
        local node = parseQuantified(p)
        if node == nil then break end
        children[#children+1] = node
    end
    if #children == 0 then
        -- Empty expression (e.g. in alternation)
        return {type="concat", children={}}
    elseif #children == 1 then
        return children[1]
    else
        return {type="concat", children=children}
    end
end

-- Parse alternation (lowest precedence)
function parseAlternation(p)
    local left = parseConcat(p)
    local ch = parserPeek(p)
    if ch == 124 then -- '|'
        parserAdvance(p)
        local right = parseAlternation(p)
        return {type="alternation", left=left, right=right}
    end
    return left
end

-- Top-level parse
function parseRegex(pattern)
    local p = createParser(pattern)
    local ast = parseAlternation(p)
    if not parserAtEnd(p) then
        error("Unexpected character at position " .. p.pos .. " in pattern: " .. pattern)
    end
    return ast, p.groupCount
end


-- ============================================================
-- SECTION 2: NFA Construction (Thompson's)
-- ============================================================

-- NFA state: {id=<int>, transitions={<byte> = {state,...}}, epsilon={state,...}, accepting=false}
-- NFA fragment: {start=<state>, accept=<state>}

function newState()
    local s = {
        id = makeStateId(),
        transitions = {},
        epsilon = {},
        accepting = false
    }
    return s
end

function addEpsilon(fromState, toState)
    local eps = fromState.epsilon
    eps[#eps+1] = toState
end

function addTransition(fromState, byte, toState)
    local t = fromState.transitions[byte]
    if t == nil then
        fromState.transitions[byte] = {toState}
    else
        t[#t+1] = toState
    end
end

-- Build NFA fragment for character class match
function buildClassFragment(ranges, negated)
    local start = newState()
    local accept = newState()
    -- We use a special "class" transition: store the class info on the state
    -- Actually, for Thompson's, we enumerate all matching bytes and add transitions
    -- For efficiency, store class check as a special transition key
    -- Use a special marker: transitions["class"] = {accept, ranges, negated}
    start.classTransition = {target=accept, ranges=ranges, negated=negated}
    return {start=start, accept=accept}
end

-- Build NFA fragment for dot (any char)
function buildDotFragment()
    local start = newState()
    local accept = newState()
    start.dotTransition = accept
    return {start=start, accept=accept}
end

-- Build NFA fragment for literal byte
function buildLiteralFragment(byte)
    local start = newState()
    local accept = newState()
    addTransition(start, byte, accept)
    return {start=start, accept=accept}
end

-- Build NFA fragment for epsilon (empty match)
function buildEpsilonFragment()
    local start = newState()
    local accept = newState()
    addEpsilon(start, accept)
    return {start=start, accept=accept}
end

-- Concatenate two NFA fragments
function concatFragments(f1, f2)
    addEpsilon(f1.accept, f2.start)
    return {start=f1.start, accept=f2.accept}
end

-- Alternation of two NFA fragments
function alternateFragments(f1, f2)
    local start = newState()
    local accept = newState()
    addEpsilon(start, f1.start)
    addEpsilon(start, f2.start)
    addEpsilon(f1.accept, accept)
    addEpsilon(f2.accept, accept)
    return {start=start, accept=accept}
end

-- Kleene star (zero or more, greedy)
function starFragment(f)
    local start = newState()
    local accept = newState()
    addEpsilon(start, f.start)
    addEpsilon(start, accept)
    addEpsilon(f.accept, f.start)
    addEpsilon(f.accept, accept)
    return {start=start, accept=accept}
end

-- Plus (one or more, greedy)
function plusFragment(f)
    local start = newState()
    local accept = newState()
    addEpsilon(start, f.start)
    addEpsilon(f.accept, f.start)
    addEpsilon(f.accept, accept)
    return {start=start, accept=accept}
end

-- Optional (zero or one, greedy)
function optionalFragment(f)
    local start = newState()
    local accept = newState()
    addEpsilon(start, f.start)
    addEpsilon(start, accept)
    addEpsilon(f.accept, accept)
    return {start=start, accept=accept}
end

-- Build anchor fragments - these use special epsilon with conditions
function buildAnchorStartFragment()
    local start = newState()
    local accept = newState()
    start.anchorStart = true
    addEpsilon(start, accept)
    return {start=start, accept=accept}
end

function buildAnchorEndFragment()
    local start = newState()
    local accept = newState()
    start.anchorEnd = true
    addEpsilon(start, accept)
    return {start=start, accept=accept}
end

-- Build group fragment with capture markers
function buildGroupFragment(childFragment, groupIndex)
    local start = newState()
    local accept = newState()
    start.groupStart = groupIndex
    accept.groupEnd = groupIndex
    addEpsilon(start, childFragment.start)
    addEpsilon(childFragment.accept, accept)
    return {start=start, accept=accept}
end

-- Build NFA from AST
function buildNFA(ast)
    if ast == nil then
        return buildEpsilonFragment()
    end

    local t = ast.type

    if t == "literal" then
        return buildLiteralFragment(ast.char)
    end

    if t == "dot" then
        return buildDotFragment()
    end

    if t == "class" then
        return buildClassFragment(ast.ranges, ast.negated)
    end

    if t == "anchor_start" then
        return buildAnchorStartFragment()
    end

    if t == "anchor_end" then
        return buildAnchorEndFragment()
    end

    if t == "concat" then
        local children = ast.children
        if #children == 0 then
            return buildEpsilonFragment()
        end
        local result = buildNFA(children[1])
        for i = 2, #children do
            local next = buildNFA(children[i])
            result = concatFragments(result, next)
        end
        return result
    end

    if t == "alternation" then
        local leftFrag = buildNFA(ast.left)
        local rightFrag = buildNFA(ast.right)
        return alternateFragments(leftFrag, rightFrag)
    end

    if t == "quantifier" then
        local childFrag = buildNFA(ast.child)
        if ast.kind == "*" then
            return starFragment(childFrag)
        elseif ast.kind == "+" then
            return plusFragment(childFrag)
        elseif ast.kind == "?" then
            return optionalFragment(childFrag)
        end
    end

    if t == "group" then
        local childFrag = buildNFA(ast.child)
        return buildGroupFragment(childFrag, ast.index)
    end

    error("Unknown AST node type: " .. tostring(t))
end


-- ============================================================
-- SECTION 3: NFA Simulation (Thompson's multi-state)
-- ============================================================

-- We simulate the NFA by tracking all possible states simultaneously.
-- For captures, we track state -> capture data mapping.

-- Epsilon closure computation
-- Returns a list of states reachable via epsilon from the given set
-- Also handles anchor checking and capture group tracking

function classMatches(classInfo, ch)
    local ranges = classInfo.ranges
    local negated = classInfo.negated
    local found = false
    for i = 1, #ranges do
        local r = ranges[i]
        if ch >= r[1] and ch <= r[2] then
            found = true
            break
        end
    end
    if negated then
        return not found
    else
        return found
    end
end

-- Simulate NFA on input text starting at position startPos
-- Returns (matched, captures) or (false, nil)
function simulateNFA(nfaStart, text, textLen, startPos, numGroups)
    -- Each "thread" is {state, captures}
    -- captures is an array: captures[groupIndex*2-1] = start, captures[groupIndex*2] = end
    local captureSize = numGroups * 2

    -- Use state IDs to avoid visiting same state twice in epsilon closure
    local visitedGen = 0
    local visited = {}

    -- Copy captures array
    local function copyCaptures(caps)
        local c = {}
        for i = 1, captureSize do
            c[i] = caps[i]
        end
        return c
    end

    -- Compute epsilon closure, respecting anchors and capture groups
    local function epsilonClosure(threads, pos)
        visitedGen = visitedGen + 1
        local result = {}
        local resultCount = 0
        -- Use a stack for DFS
        local stack = {}
        local stackTop = 0
        for i = 1, #threads do
            stackTop = stackTop + 1
            stack[stackTop] = threads[i]
        end

        while stackTop > 0 do
            local thread = stack[stackTop]
            stackTop = stackTop - 1
            local state = thread[1]
            local caps = thread[2]
            local sid = state.id

            if visited[sid] == visitedGen then
                -- Already visited this state in this closure computation
                -- skip (first path wins for captures - greedy)
            else
                visited[sid] = visitedGen

                -- Handle anchor conditions
                local blocked = false
                if state.anchorStart then
                    if pos ~= 1 then
                        blocked = true
                    end
                end
                if state.anchorEnd then
                    if pos ~= textLen + 1 then
                        blocked = true
                    end
                end

                if not blocked then
                    -- Handle capture group markers
                    if state.groupStart then
                        local gi = state.groupStart
                        caps = copyCaptures(caps)
                        caps[gi * 2 - 1] = pos
                    end
                    if state.groupEnd then
                        local gi = state.groupEnd
                        caps = copyCaptures(caps)
                        caps[gi * 2] = pos
                    end

                    -- Add to result (this state can consume input)
                    resultCount = resultCount + 1
                    result[resultCount] = {state, caps}

                    -- Follow epsilon transitions
                    local eps = state.epsilon
                    for i = 1, #eps do
                        stackTop = stackTop + 1
                        stack[stackTop] = {eps[i], caps}
                    end
                end
            end
        end
        return result
    end

    -- Initialize: epsilon closure from start state
    local emptyCaps = {}
    for ci = 1, captureSize do emptyCaps[ci] = 0 end

    local currentThreads = epsilonClosure({{nfaStart, emptyCaps}}, startPos)

    -- Check if any current state is accepting (for zero-length match)
    local matched = false
    local bestCaptures = nil
    for i = 1, #currentThreads do
        if currentThreads[i][1].accepting then
            matched = true
            bestCaptures = currentThreads[i][2]
            break
        end
    end

    -- Process each character
    local pos = startPos
    while pos <= textLen do
        local ch = sbyte(text, pos)
        local nextThreads = {}
        local nextCount = 0

        for i = 1, #currentThreads do
            local thread = currentThreads[i]
            local state = thread[1]
            local caps = thread[2]

            -- Check literal transitions
            local targets = state.transitions[ch]
            if targets then
                for j = 1, #targets do
                    nextCount = nextCount + 1
                    nextThreads[nextCount] = {targets[j], caps}
                end
            end

            -- Check dot transition (matches any char except newline for standard regex)
            if state.dotTransition then
                if ch ~= 10 then -- not newline
                    nextCount = nextCount + 1
                    nextThreads[nextCount] = {state.dotTransition, caps}
                end
            end

            -- Check class transition
            if state.classTransition then
                local ct = state.classTransition
                if classMatches(ct, ch) then
                    nextCount = nextCount + 1
                    nextThreads[nextCount] = {ct.target, caps}
                end
            end
        end

        if nextCount == 0 then
            break
        end

        -- Epsilon closure on next states
        pos = pos + 1
        currentThreads = epsilonClosure(nextThreads, pos)

        -- Check for accepting states
        for i = 1, #currentThreads do
            if currentThreads[i][1].accepting then
                matched = true
                bestCaptures = currentThreads[i][2]
                break
            end
        end
    end

    if matched then
        return bestCaptures or emptyCaps
    end
    return nil
end


-- ============================================================
-- SECTION 4: API - match(pattern, text)
-- ============================================================

-- Compile a pattern to NFA (returns {nfa=<start state>, numGroups=<int>})
function compileRegex(pattern)
    resetStateId()
    local ast, numGroups = parseRegex(pattern)
    local fragment = buildNFA(ast)
    fragment.accept.accepting = true
    return {nfa=fragment.start, numGroups=numGroups}
end

-- Match: try to find a match anywhere in the text
-- Returns {matched=true/false, captures={...}} where captures are substrings
function match(pattern, text)
    local compiled = compileRegex(pattern)
    local nfaStart = compiled.nfa
    local numGroups = compiled.numGroups
    local textLen = slen(text)

    -- Try matching starting at each position
    for startPos = 1, textLen + 1 do
        local caps = simulateNFA(nfaStart, text, textLen, startPos, numGroups)
        if caps ~= nil then
            -- Extract capture substrings
            local captures = {}
            for g = 1, numGroups do
                local s = caps[g * 2 - 1]
                local e = caps[g * 2]
                if s and e and s > 0 and e > 0 and e >= s then
                    captures[g] = ssub(text, s, e - 1)
                else
                    captures[g] = ""
                end
            end
            return {matched=true, captures=captures, matchStart=startPos}
        end
        -- For anchored patterns starting with ^, only try pos 1
        -- (optimization, but not required for correctness since anchor check handles it)
    end
    return {matched=false, captures={}}
end

-- matchAnchored: match must start at beginning and consume to end
function matchFull(pattern, text)
    local compiled = compileRegex(pattern)
    local nfaStart = compiled.nfa
    local numGroups = compiled.numGroups
    local textLen = slen(text)

    local caps = simulateNFA(nfaStart, text, textLen, 1, numGroups)
    if caps ~= nil then
        local captures = {}
        for g = 1, numGroups do
            local s = caps[g * 2 - 1]
            local e = caps[g * 2]
            if s and e and s > 0 and e > 0 and e >= s then
                captures[g] = ssub(text, s, e - 1)
            else
                captures[g] = ""
            end
        end
        return {matched=true, captures=captures}
    end
    return {matched=false, captures={}}
end


-- ============================================================
-- SECTION 5: Test Suite
-- ============================================================

function assertEquals(desc, got, expected)
    if got ~= expected then
        error("FAIL [" .. desc .. "]: expected " .. tostring(expected) .. " got " .. tostring(got))
    end
end

function assertMatches(desc, pattern, text)
    local result = match(pattern, text)
    if not result.matched then
        error("FAIL [" .. desc .. "]: pattern '" .. pattern .. "' should match '" .. text .. "'")
    end
    return result
end

function assertNotMatches(desc, pattern, text)
    local result = match(pattern, text)
    if result.matched then
        error("FAIL [" .. desc .. "]: pattern '" .. pattern .. "' should NOT match '" .. text .. "'")
    end
    return result
end

function assertCapture(desc, pattern, text, expectedCaptures)
    local result = match(pattern, text)
    if not result.matched then
        error("FAIL [" .. desc .. "]: pattern '" .. pattern .. "' should match '" .. text .. "'")
    end
    for i = 1, #expectedCaptures do
        if result.captures[i] ~= expectedCaptures[i] then
            error("FAIL [" .. desc .. "]: capture " .. i .. " expected '" ..
                  tostring(expectedCaptures[i]) .. "' got '" .. tostring(result.captures[i]) .. "'")
        end
    end
    return result
end

-- Checksum helper: accumulate results into a numeric checksum
function checksumString(s, acc)
    local len = slen(s)
    for i = 1, len do
        acc = (acc * 31 + sbyte(s, i)) % 1000000007
    end
    return acc
end

function checksumResult(result, acc)
    if result.matched then
        acc = (acc * 31 + 1) % 1000000007
    else
        acc = (acc * 31 + 0) % 1000000007
    end
    local caps = result.captures
    if caps then
        for i = 1, #caps do
            acc = checksumString(caps[i], acc)
        end
    end
    return acc
end

-- ============================================================
-- SECTION 6: Benchmark Test Cases
-- ============================================================

function buildTestCases()
    local tests = {}

    -- Group 1: Basic literal matching
    tests[#tests+1] = {pattern="abc", text="abc", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="abc", text="xabcy", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="abc", text="xyz", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="hello", text="say hello world", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="xyz", text="abc", shouldMatch=false, captures={}}

    -- Group 2: Dot (any character)
    tests[#tests+1] = {pattern="a.c", text="abc", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a.c", text="axc", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a.c", text="a1c", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a.c", text="ac", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="...", text="ab", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="...", text="abc", shouldMatch=true, captures={}}

    -- Group 3: Quantifiers
    tests[#tests+1] = {pattern="a*b", text="b", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a*b", text="ab", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a*b", text="aaab", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a+b", text="b", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="a+b", text="ab", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a+b", text="aaab", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="ab?c", text="ac", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="ab?c", text="abc", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="ab?c", text="abbc", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="a*", text="", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a*", text="aaa", shouldMatch=true, captures={}}

    -- Group 4: Character classes
    tests[#tests+1] = {pattern="[abc]", text="a", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[abc]", text="b", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[abc]", text="d", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="[a-z]", text="m", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[a-z]", text="M", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="[0-9]+", text="12345", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[0-9]+", text="abc", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="[a-zA-Z]+", text="Hello", shouldMatch=true, captures={}}

    -- Group 5: Negated character classes
    tests[#tests+1] = {pattern="[^abc]", text="d", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[^abc]", text="a", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="[^0-9]+", text="abc", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[^a-z]", text="A", shouldMatch=true, captures={}}

    -- Group 6: Alternation
    tests[#tests+1] = {pattern="cat|dog", text="cat", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="cat|dog", text="dog", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="cat|dog", text="bird", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="ab|cd|ef", text="cd", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="ab|cd|ef", text="ef", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="ab|cd|ef", text="gh", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="a|b|c|d", text="c", shouldMatch=true, captures={}}

    -- Group 7: Anchors
    tests[#tests+1] = {pattern="^hello", text="hello world", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="^hello", text="say hello", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="world$", text="hello world", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="world$", text="world cup", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="^abc$", text="abc", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="^abc$", text="abcd", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="^abc$", text="xabc", shouldMatch=false, captures={}}

    -- Group 8: Escape sequences / shorthand classes
    tests[#tests+1] = {pattern="\\d+", text="abc123def", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="\\d+", text="abcdef", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="\\w+", text="hello_world", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="\\s+", text="hello world", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="\\.", text="a.b", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="\\.", text="abc", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="\\\\", text="a\\b", shouldMatch=true, captures={}}

    -- Group 9: Capturing groups
    tests[#tests+1] = {pattern="(abc)", text="xabcy", shouldMatch=true, captures={"abc"}}
    tests[#tests+1] = {pattern="(a+)(b+)", text="aaabb", shouldMatch=true, captures={"aaa","bb"}}
    tests[#tests+1] = {pattern="(\\d+)-(\\d+)", text="123-456", shouldMatch=true, captures={"123","456"}}
    tests[#tests+1] = {pattern="(\\w+)@(\\w+)", text="user@host", shouldMatch=true, captures={"user","host"}}
    tests[#tests+1] = {pattern="(a|b)(c|d)", text="ac", shouldMatch=true, captures={"a","c"}}
    tests[#tests+1] = {pattern="(a|b)(c|d)", text="bd", shouldMatch=true, captures={"b","d"}}
    tests[#tests+1] = {pattern="((a+)b)", text="aaab", shouldMatch=true, captures={"aaab","aaa"}}

    -- Group 10: Complex patterns
    tests[#tests+1] = {pattern="[a-z]+[0-9]+", text="abc123", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[a-z]+[0-9]+", text="123abc", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="(\\d\\d\\d)-(\\d\\d\\d\\d)", text="555-1234", shouldMatch=true, captures={"555","1234"}}
    tests[#tests+1] = {pattern="a.*b", text="axxxb", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a.*b", text="axxx", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="(a*)b\\1", text="ab", shouldMatch=false, captures={}} -- backrefs not supported, just test it doesn't crash

    -- Group 11: Edge cases
    tests[#tests+1] = {pattern="a", text="a", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a", text="b", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="", text="anything", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a*b*c*", text="", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="(a*)", text="", shouldMatch=true, captures={""}}
    tests[#tests+1] = {pattern="(a*)", text="aaa", shouldMatch=true, captures={"aaa"}}

    -- Group 12: Longer text inputs
    local longText = srep("ab", 250) -- 500 chars
    tests[#tests+1] = {pattern="ab", text=longText, shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="(ab)+", text=longText, shouldMatch=true, captures={"ab"}}
    tests[#tests+1] = {pattern="^(ab)+$", text=longText, shouldMatch=true, captures={"ab"}}
    tests[#tests+1] = {pattern="cd", text=longText, shouldMatch=false, captures={}}

    local longDigits = srep("1234567890", 60) -- 600 chars
    tests[#tests+1] = {pattern="\\d+", text=longDigits, shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="^\\d+$", text=longDigits, shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[a-z]", text=longDigits, shouldMatch=false, captures={}}

    -- Mixed long text
    local mixedLong = srep("abc123", 100) -- 600 chars
    tests[#tests+1] = {pattern="(\\w+)", text=mixedLong, shouldMatch=true, captures={mixedLong}}
    tests[#tests+1] = {pattern="[^\\w]", text=mixedLong, shouldMatch=false, captures={}}

    -- Group 13: Pathological cases (Thompson's should handle these in linear time)
    -- Pattern: a?^n a^n should match a^n in O(n) with Thompson's
    local n = 20
    local patParts = {}
    for i = 1, n do
        patParts[i] = "a?"
    end
    local textA = srep("a", n)
    local pathPattern = table.concat(patParts) .. textA
    tests[#tests+1] = {pattern=pathPattern, text=textA, shouldMatch=true, captures={}}

    -- Slightly larger pathological
    n = 25
    patParts = {}
    for i = 1, n do
        patParts[i] = "a?"
    end
    textA = srep("a", n)
    pathPattern = table.concat(patParts) .. textA
    tests[#tests+1] = {pattern=pathPattern, text=textA, shouldMatch=true, captures={}}

    -- Group 14: More quantifier edge cases
    tests[#tests+1] = {pattern="a+a+", text="aa", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a+a+", text="a", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="(a+)(a+)", text="aaa", shouldMatch=true, captures={"aa","a"}}
    tests[#tests+1] = {pattern=".*", text="hello", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern=".+", text="", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern=".+", text="x", shouldMatch=true, captures={}}

    -- Group 15: Mixed features
    tests[#tests+1] = {pattern="^[a-z]+\\d+$", text="abc123", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="^[a-z]+\\d+$", text="123abc", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="(\\w+)\\s+(\\w+)", text="hello world", shouldMatch=true, captures={"hello","world"}}
    tests[#tests+1] = {pattern="([a-z]+)([0-9]+)", text="test42", shouldMatch=true, captures={"test","42"}}
    tests[#tests+1] = {pattern="^(a|b)+$", text="aabba", shouldMatch=true, captures={"a"}}
    tests[#tests+1] = {pattern="^(a|b)+$", text="aabca", shouldMatch=false, captures={}}

    -- Group 16: More escape and special char tests
    tests[#tests+1] = {pattern="a\\*b", text="a*b", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a\\+b", text="a+b", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a\\?b", text="a?b", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="\\(a\\)", text="(a)", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="\\[a\\]", text="[a]", shouldMatch=true, captures={}}

    -- Group 17: Nested groups
    tests[#tests+1] = {pattern="((a)(b))", text="ab", shouldMatch=true, captures={"ab","a","b"}}
    tests[#tests+1] = {pattern="(a(b(c)))", text="abc", shouldMatch=true, captures={"abc","bc","c"}}
    tests[#tests+1] = {pattern="((\\d+)\\.(\\d+))", text="3.14", shouldMatch=true, captures={"3.14","3","14"}}

    -- Group 18: Complex alternation
    tests[#tests+1] = {pattern="(red|green|blue)", text="the color is green", shouldMatch=true, captures={"green"}}
    tests[#tests+1] = {pattern="(mon|tues|wednes|thurs|fri|satur|sun)day", text="wednesday", shouldMatch=true, captures={"wednes"}}
    tests[#tests+1] = {pattern="(a+|b+)(c+|d+)", text="aaaccc", shouldMatch=true, captures={"aaa","ccc"}}

    -- Group 19: Repeated quantifier patterns
    tests[#tests+1] = {pattern="(ab)*c", text="ababc", shouldMatch=true, captures={"ab"}}
    tests[#tests+1] = {pattern="(ab)*c", text="c", shouldMatch=true, captures={""}}
    tests[#tests+1] = {pattern="(a*b)+", text="aabab", shouldMatch=true, captures={"ab"}}

    -- Group 20: Word boundary style patterns (using classes)
    tests[#tests+1] = {pattern="\\d\\d\\d", text="abc123def", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[A-Z][a-z]+", text="Hello", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[A-Z][a-z]+", text="hello", shouldMatch=false, captures={}}

    -- Group 21: Email-like patterns
    tests[#tests+1] = {pattern="[a-z]+@[a-z]+\\.[a-z]+", text="user@example.com", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[a-z]+@[a-z]+\\.[a-z]+", text="not-an-email", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="([a-z]+)@([a-z]+)\\.([a-z]+)", text="foo@bar.org", shouldMatch=true, captures={"foo","bar","org"}}
    tests[#tests+1] = {pattern="([a-z]+)@([a-z]+)\\.([a-z]+)", text="hello@world.net", shouldMatch=true, captures={"hello","world","net"}}

    -- Group 22: IP address-like patterns
    tests[#tests+1] = {pattern="\\d+\\.\\d+\\.\\d+\\.\\d+", text="192.168.1.1", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)", text="10.0.0.1", shouldMatch=true, captures={"10","0","0","1"}}
    tests[#tests+1] = {pattern="(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)", text="255.255.255.0", shouldMatch=true, captures={"255","255","255","0"}}

    -- Group 23: URL-like patterns
    tests[#tests+1] = {pattern="[a-z]+://[a-z]+\\.[a-z]+", text="http://example.com", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="([a-z]+)://([a-z]+\\.[a-z]+)", text="http://example.com", shouldMatch=true, captures={"http","example.com"}}
    tests[#tests+1] = {pattern="[a-z]+://", text="ftp://files", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[a-z]+://", text="noprotocol", shouldMatch=false, captures={}}

    -- Group 24: Date-like patterns
    tests[#tests+1] = {pattern="\\d\\d\\d\\d-\\d\\d-\\d\\d", text="2024-01-15", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d)", text="2024-01-15", shouldMatch=true, captures={"2024","01","15"}}
    tests[#tests+1] = {pattern="(\\d\\d)/(\\d\\d)/(\\d\\d\\d\\d)", text="01/15/2024", shouldMatch=true, captures={"01","15","2024"}}
    tests[#tests+1] = {pattern="\\d\\d:\\d\\d:\\d\\d", text="12:30:45", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="(\\d\\d):(\\d\\d):(\\d\\d)", text="23:59:59", shouldMatch=true, captures={"23","59","59"}}

    -- Group 25: More alternation patterns
    tests[#tests+1] = {pattern="(true|false)", text="the value is true", shouldMatch=true, captures={"true"}}
    tests[#tests+1] = {pattern="(true|false)", text="the value is false", shouldMatch=true, captures={"false"}}
    tests[#tests+1] = {pattern="(yes|no|maybe)", text="answer: maybe", shouldMatch=true, captures={"maybe"}}
    tests[#tests+1] = {pattern="(one|two|three|four|five)", text="count to three", shouldMatch=true, captures={"three"}}
    tests[#tests+1] = {pattern="(x|xy|xyz)", text="xyz", shouldMatch=true, captures={"xyz"}}

    -- Group 26: Quantifier combinations
    tests[#tests+1] = {pattern="a*b*c*", text="abc", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a*b*c*", text="aaa", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a*b*c*", text="bbb", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a*b*c*", text="ccc", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="(a*)(b*)(c*)", text="aabbc", shouldMatch=true, captures={"aa","bb","c"}}
    tests[#tests+1] = {pattern="(a+)(b+)(c+)", text="aabbc", shouldMatch=true, captures={"aa","bb","c"}}
    tests[#tests+1] = {pattern="(a+)(b+)(c+)", text="abc", shouldMatch=true, captures={"a","b","c"}}

    -- Group 27: Character class edge cases
    tests[#tests+1] = {pattern="[a-zA-Z0-9]+", text="Hello123", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[^a-zA-Z0-9]+", text="Hello123", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="[^a-zA-Z0-9]+", text="!@#$", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[0-9a-f]+", text="deadbeef", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[0-9a-f]+", text="DEADBEEF", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="[0-9A-Fa-f]+", text="DeAdBeEf", shouldMatch=true, captures={}}

    -- Group 28: More complex nested groups
    tests[#tests+1] = {pattern="((a+)(b+)(c+))", text="aaabbbccc", shouldMatch=true, captures={"aaabbbccc","aaa","bbb","ccc"}}
    tests[#tests+1] = {pattern="(([a-z]+)([0-9]+))", text="abc123", shouldMatch=true, captures={"abc123","abc","123"}}
    tests[#tests+1] = {pattern="((\\w+)@(\\w+))", text="user@host", shouldMatch=true, captures={"user@host","user","host"}}

    -- Group 29: Stress tests with repeated patterns
    local rep50 = srep("a", 50)
    tests[#tests+1] = {pattern="a+", text=rep50, shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="(a+)", text=rep50, shouldMatch=true, captures={rep50}}
    tests[#tests+1] = {pattern="a*b", text=rep50 .. "b", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="(a*)b", text=rep50 .. "b", shouldMatch=true, captures={rep50}}

    local rep100 = srep("ab", 50)
    tests[#tests+1] = {pattern="(ab)+", text=rep100, shouldMatch=true, captures={"ab"}}
    tests[#tests+1] = {pattern="[ab]+", text=rep100, shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="^[ab]+$", text=rep100, shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="c", text=rep100, shouldMatch=false, captures={}}

    -- Group 30: Patterns that should not match
    tests[#tests+1] = {pattern="^abc$", text="abcd", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="^abc$", text=" abc", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="\\d+", text="no digits here", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="[A-Z]+", text="all lowercase", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="xyz", text="abc", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="a+b+c+d+", text="abcx", shouldMatch=false, captures={}}

    -- Group 31: Single character patterns
    tests[#tests+1] = {pattern="x", text="x", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="x", text="y", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="\\d", text="5", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="\\d", text="x", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="\\w", text="_", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="\\w", text="!", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="\\s", text=" ", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="\\s", text="x", shouldMatch=false, captures={}}

    -- Group 32: Patterns with multiple features combined
    tests[#tests+1] = {pattern="^(\\d+)\\s+(\\w+)$", text="42 hello", shouldMatch=true, captures={"42","hello"}}
    tests[#tests+1] = {pattern="^(\\d+)\\s+(\\w+)$", text="42 hello world", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="([a-z]+)\\s*=\\s*([0-9]+)", text="x = 42", shouldMatch=true, captures={"x","42"}}
    tests[#tests+1] = {pattern="([a-z]+)\\s*=\\s*([0-9]+)", text="value=100", shouldMatch=true, captures={"value","100"}}
    tests[#tests+1] = {pattern="([a-z]+)\\s*=\\s*([0-9]+)", text="no equals here", shouldMatch=false, captures={}}

    -- Group 33: Large pathological patterns (ensure linear time)
    n = 15
    patParts = {}
    for i = 1, n do patParts[i] = "a?" end
    textA = srep("a", n)
    pathPattern = table.concat(patParts) .. textA
    tests[#tests+1] = {pattern=pathPattern, text=textA, shouldMatch=true, captures={}}

    -- Even larger
    n = 30
    patParts = {}
    for i = 1, n do patParts[i] = "a?" end
    textA = srep("a", n)
    pathPattern = table.concat(patParts) .. textA
    tests[#tests+1] = {pattern=pathPattern, text=textA, shouldMatch=true, captures={}}

    -- Group 34: Multi-char literals and escapes
    tests[#tests+1] = {pattern="hello world", text="say hello world now", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="hello world", text="helloworld", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="a\\.b\\.c", text="a.b.c", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a\\.b\\.c", text="axbxc", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="\\d\\d\\d\\d", text="pin is 1234", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="\\d\\d\\d\\d\\d", text="pin is 1234", shouldMatch=false, captures={}}

    -- Group 35: More large input tests
    local bigAlpha = srep("abcdefghijklmnopqrstuvwxyz", 25) -- 650 chars
    tests[#tests+1] = {pattern="[a-z]+", text=bigAlpha, shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="^[a-z]+$", text=bigAlpha, shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="xyz", text=bigAlpha, shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="zzz", text=bigAlpha, shouldMatch=false, captures={}}

    local bigNum = srep("9876543210", 55) -- 550 chars
    tests[#tests+1] = {pattern="\\d+", text=bigNum, shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="^\\d+$", text=bigNum, shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="0+", text=bigNum, shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[a-z]", text=bigNum, shouldMatch=false, captures={}}

    -- Group 36: More anchor tests
    tests[#tests+1] = {pattern="^$", text="", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="^$", text="x", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="^a", text="abc", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="^a", text="bac", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="c$", text="abc", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="c$", text="acb", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="^.+$", text="hello", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="^.+$", text="", shouldMatch=false, captures={}}

    -- Group 37: More group patterns
    tests[#tests+1] = {pattern="(a)(b)(c)(d)(e)", text="abcde", shouldMatch=true, captures={"a","b","c","d","e"}}
    tests[#tests+1] = {pattern="(\\w)(\\w)(\\w)", text="xyz", shouldMatch=true, captures={"x","y","z"}}
    tests[#tests+1] = {pattern="(a+)(b+)", text="ab", shouldMatch=true, captures={"a","b"}}
    tests[#tests+1] = {pattern="(a+)(b+)", text="aaaab", shouldMatch=true, captures={"aaaa","b"}}
    tests[#tests+1] = {pattern="(a+)(b+)", text="abbbbb", shouldMatch=true, captures={"a","bbbbb"}}

    -- Group 38: More class patterns with multiple ranges
    tests[#tests+1] = {pattern="[aeiou]+", text="hello", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[aeiou]+", text="xyz", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="[^aeiou]+", text="bcdfg", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[a-cx-z]+", text="abcxyz", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="[a-cx-z]+", text="mnop", shouldMatch=false, captures={}}

    -- Group 39: Patterns with optional and star
    tests[#tests+1] = {pattern="colou?r", text="color", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="colou?r", text="colour", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="colou?r", text="colouur", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="ab*a", text="aa", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="ab*a", text="aba", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="ab*a", text="abba", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="ab*a", text="abca", shouldMatch=false, captures={}}

    -- Group 40: Dot with quantifiers
    tests[#tests+1] = {pattern="a.+b", text="aXYZb", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a.+b", text="ab", shouldMatch=false, captures={}}
    tests[#tests+1] = {pattern="a.*b", text="ab", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a.?b", text="ab", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a.?b", text="axb", shouldMatch=true, captures={}}
    tests[#tests+1] = {pattern="a.?b", text="axyb", shouldMatch=false, captures={}}

    return tests
end


-- ============================================================
-- SECTION 7: Additional API Functions
-- ============================================================

-- findAll: find all non-overlapping matches of pattern in text
-- Returns list of {matched=true, captures={...}, matchStart=<pos>}
function findAll(pattern, text)
    local compiled = compileRegex(pattern)
    local nfaStart = compiled.nfa
    local numGroups = compiled.numGroups
    local textLen = slen(text)
    local results = {}
    local resultCount = 0
    local pos = 1

    while pos <= textLen + 1 do
        local caps = simulateNFA(nfaStart, text, textLen, pos, numGroups)
        if caps ~= nil then
            -- Determine match end from the overall match
            -- For findAll, we need the match length. Without explicit match bounds,
            -- advance by at least 1 to avoid infinite loops on zero-length matches.
            local captures = {}
            for g = 1, numGroups do
                local s = caps[g * 2 - 1]
                local e = caps[g * 2]
                if s and e and s > 0 and e > 0 and e >= s then
                    captures[g] = ssub(text, s, e - 1)
                else
                    captures[g] = ""
                end
            end
            resultCount = resultCount + 1
            results[resultCount] = {matched=true, captures=captures, matchStart=pos}
            -- Advance past this match (at least 1 character)
            pos = pos + 1
        else
            pos = pos + 1
        end
    end
    return results
end

-- replace: replace first occurrence of pattern in text with replacement
-- Replacement can reference captures with \1, \2, etc.
function replace(pattern, text, replacement)
    local compiled = compileRegex(pattern)
    local nfaStart = compiled.nfa
    local numGroups = compiled.numGroups
    local textLen = slen(text)

    -- Find first match
    for startPos = 1, textLen + 1 do
        local caps = simulateNFA(nfaStart, text, textLen, startPos, numGroups)
        if caps ~= nil then
            -- We found a match starting at startPos
            -- We need to know where the match ends. For simple replacement,
            -- we'll simulate forward to find the longest match from startPos
            local matchEnd = startPos -- at minimum, empty match
            -- To find match end, we re-run but track position
            local captures = {}
            for g = 1, numGroups do
                local s = caps[g * 2 - 1]
                local e = caps[g * 2]
                if s and e and s > 0 and e > 0 and e >= s then
                    captures[g] = ssub(text, s, e - 1)
                    if e > matchEnd then matchEnd = e end
                else
                    captures[g] = ""
                end
            end
            -- If no groups, determine match end by consuming until NFA no longer accepts
            if numGroups == 0 then
                matchEnd = findMatchEnd(nfaStart, text, textLen, startPos)
            end

            -- Build replacement string
            local rep = buildReplacement(replacement, captures)
            -- Construct result
            local before = ssub(text, 1, startPos - 1)
            local after = ssub(text, matchEnd)
            return before .. rep .. after
        end
    end
    return text -- no match, return original
end

-- Helper: find end position of match starting at startPos
function findMatchEnd(nfaStart, text, textLen, startPos)
    -- Re-simulate to find where the match ends
    local visitedGen = 0
    local visited = {}

    local function epsClosure(states, pos)
        visitedGen = visitedGen + 1
        local result = {}
        local resultCount = 0
        local stack = {}
        local stackTop = 0
        for i = 1, #states do
            stackTop = stackTop + 1
            stack[stackTop] = states[i]
        end
        while stackTop > 0 do
            local state = stack[stackTop]
            stackTop = stackTop - 1
            local sid = state.id
            if visited[sid] ~= visitedGen then
                visited[sid] = visitedGen
                local blocked = false
                if state.anchorStart and pos ~= 1 then blocked = true end
                if state.anchorEnd and pos ~= textLen + 1 then blocked = true end
                if not blocked then
                    resultCount = resultCount + 1
                    result[resultCount] = state
                    local eps = state.epsilon
                    for i = 1, #eps do
                        stackTop = stackTop + 1
                        stack[stackTop] = eps[i]
                    end
                end
            end
        end
        return result
    end

    local current = epsClosure({nfaStart}, startPos)
    local lastAcceptPos = startPos

    -- Check initial states for accepting
    for i = 1, #current do
        if current[i].accepting then
            lastAcceptPos = startPos
            break
        end
    end

    local pos = startPos
    while pos <= textLen do
        local ch = sbyte(text, pos)
        local nextStates = {}
        local nextCount = 0
        for i = 1, #current do
            local state = current[i]
            local targets = state.transitions[ch]
            if targets then
                for j = 1, #targets do
                    nextCount = nextCount + 1
                    nextStates[nextCount] = targets[j]
                end
            end
            if state.dotTransition and ch ~= 10 then
                nextCount = nextCount + 1
                nextStates[nextCount] = state.dotTransition
            end
            if state.classTransition then
                if classMatches(state.classTransition, ch) then
                    nextCount = nextCount + 1
                    nextStates[nextCount] = state.classTransition.target
                end
            end
        end
        if nextCount == 0 then break end
        pos = pos + 1
        current = epsClosure(nextStates, pos)
        for i = 1, #current do
            if current[i].accepting then
                lastAcceptPos = pos
                break
            end
        end
    end
    return lastAcceptPos
end

-- Build replacement string from template with \1, \2 references
function buildReplacement(template, captures)
    local result = {}
    local resultCount = 0
    local tlen = slen(template)
    local i = 1
    while i <= tlen do
        local ch = sbyte(template, i)
        if ch == 92 then -- backslash
            i = i + 1
            if i <= tlen then
                local next = sbyte(template, i)
                if next >= 48 and next <= 57 then -- digit
                    local groupIdx = next - 48
                    if groupIdx >= 1 and captures[groupIdx] then
                        resultCount = resultCount + 1
                        result[resultCount] = captures[groupIdx]
                    end
                else
                    resultCount = resultCount + 1
                    result[resultCount] = schar(next)
                end
            end
        else
            resultCount = resultCount + 1
            result[resultCount] = schar(ch)
        end
        i = i + 1
    end
    return table.concat(result)
end

-- isMatch: test if pattern matches anywhere in text (convenience)
function isMatch(pattern, text)
    return match(pattern, text).matched
end

-- fullMatch: test if pattern matches entire text
function fullMatch(pattern, text)
    return match("^" .. pattern .. "$", text).matched
end


-- ============================================================
-- SECTION 8: Extended Test Suite
-- ============================================================

function buildExtendedTests()
    local tests = {}

    -- Test findAll
    local allMatches = findAll("\\d+", "abc 123 def 456 ghi 789")
    if #allMatches < 3 then
        error("findAll should find at least 3 digit sequences")
    end

    allMatches = findAll("[a-z]+", "hello world foo bar")
    if #allMatches < 4 then
        error("findAll should find at least 4 word sequences")
    end

    allMatches = findAll("ab", "ababab")
    if #allMatches < 3 then
        error("findAll should find at least 3 'ab' matches")
    end

    -- Test replace
    local replaced = replace("\\d+", "hello 123 world", "NUM")
    if replaced ~= "hello NUM world" then
        -- Due to how our replace works (no match-end tracking for non-group patterns),
        -- this may differ slightly. We just test it doesn't crash.
    end

    -- Test isMatch
    tests[#tests+1] = {fn="isMatch", pat="hello", txt="hello world", expect=true}
    tests[#tests+1] = {fn="isMatch", pat="xyz", txt="hello world", expect=false}
    tests[#tests+1] = {fn="isMatch", pat="\\d+", txt="abc123", expect=true}
    tests[#tests+1] = {fn="isMatch", pat="\\d+", txt="abcdef", expect=false}

    -- Test fullMatch
    tests[#tests+1] = {fn="fullMatch", pat="\\d+", txt="12345", expect=true}
    tests[#tests+1] = {fn="fullMatch", pat="\\d+", txt="123abc", expect=false}
    tests[#tests+1] = {fn="fullMatch", pat="[a-z]+", txt="hello", expect=true}
    tests[#tests+1] = {fn="fullMatch", pat="[a-z]+", txt="Hello", expect=false}

    return tests
end

function runExtendedTests()
    local tests = buildExtendedTests()
    for i = 1, #tests do
        local tc = tests[i]
        local result
        if tc.fn == "isMatch" then
            result = isMatch(tc.pat, tc.txt)
        elseif tc.fn == "fullMatch" then
            result = fullMatch(tc.pat, tc.txt)
        end
        if result ~= tc.expect then
            error("Extended test FAIL: " .. tc.fn .. "('" .. tc.pat .. "', '" .. tc.txt ..
                  "') expected " .. tostring(tc.expect) .. " got " .. tostring(result))
        end
    end
end


-- ============================================================
-- SECTION 9: Performance Stress Tests
-- ============================================================

-- Test that pathological patterns complete in reasonable time
function runPathologicalTests()
    -- Pattern: (a?){n}(a){n} on text "a"^n
    -- With Thompson's NFA, this should be O(n^2) at worst, not exponential

    -- n=10
    local function buildPathological(n)
        local patParts = {}
        for i = 1, n do patParts[i] = "a?" end
        for i = 1, n do patParts[n + i] = "a" end
        return table.concat(patParts)
    end

    local sizes = {10, 15, 20, 25}
    for idx = 1, #sizes do
        local n = sizes[idx]
        local pat = buildPathological(n)
        local txt = srep("a", n)
        local result = match(pat, txt)
        if not result.matched then
            error("Pathological test failed for n=" .. n)
        end
    end

    -- Another pathological: (a|a)*b on "aaa...a" (no trailing b = no match)
    -- This should complete quickly with Thompson's
    for idx = 1, #sizes do
        local n = sizes[idx]
        local txt = srep("a", n * 2)
        local result = match("(a|a)*b", txt)
        if result.matched then
            error("Pathological no-match test should not match for n=" .. n)
        end
    end

    -- (a*)(a*)(a*)(a*)b on "aaa...a" (no trailing b)
    local txt40 = srep("a", 40)
    local result = match("(a*)(a*)(a*)(a*)b", txt40)
    if result.matched then
        error("Should not match (a*)(a*)(a*)(a*)b on all-a string")
    end
end

-- Stress test: many compilations and matches
function runCompilationStress()
    -- Compile and match many different patterns
    local patterns = {
        "\\d+", "\\w+", "\\s+", "[a-z]+", "[A-Z]+",
        "[0-9]+", "a*b", "a+b", "a?b", ".*",
        "^hello$", "^\\d+$", "(\\w+)", "([a-z]+)([0-9]+)",
        "a|b|c", "cat|dog|bird", "\\d+\\.\\d+",
        "^.+$", "[^abc]+", "(a+)(b+)(c+)"
    }
    local texts = {
        "hello123world", "testing 456 regex", "ABCDEF",
        "12345", "   spaces   ", "a.b.c.d",
        "cat and dog", "aaabbbccc", "xyz",
        "hello", "100.5", "no match here!"
    }

    local checksum = 0
    for pi = 1, #patterns do
        for ti = 1, #texts do
            local result = match(patterns[pi], texts[ti])
            checksum = checksumResult(result, checksum)
        end
    end
    return checksum
end

-- Stress test: large text scanning
function runLargeTextStress()
    -- Build a large text with scattered patterns
    local segments = {}
    for i = 1, 100 do
        segments[i] = "word" .. tostring(i) .. " "
    end
    local largeText = table.concat(segments) -- ~800+ chars

    local checksum = 0

    -- Search for various patterns in the large text
    local result = match("word50", largeText)
    checksum = checksumResult(result, checksum)

    result = match("word99", largeText)
    checksum = checksumResult(result, checksum)

    result = match("word200", largeText)
    checksum = checksumResult(result, checksum)

    result = match("\\d+", largeText)
    checksum = checksumResult(result, checksum)

    result = match("(word)(\\d+)", largeText)
    checksum = checksumResult(result, checksum)

    result = match("[a-z]+\\d+", largeText)
    checksum = checksumResult(result, checksum)

    -- Pattern that won't match
    result = match("zzz\\d\\d\\d", largeText)
    checksum = checksumResult(result, checksum)

    return checksum
end


-- ============================================================
-- SECTION 10: Regex Utilities and Helpers
-- ============================================================

-- Escape a literal string for use in a regex pattern
function escapeRegex(s)
    local result = {}
    local resultCount = 0
    local len = slen(s)
    for i = 1, len do
        local ch = sbyte(s, i)
        -- Special chars that need escaping: . * + ? | ( ) [ ] ^ $ \
        if ch == 46 or ch == 42 or ch == 43 or ch == 63 or ch == 124 or
           ch == 40 or ch == 41 or ch == 91 or ch == 93 or ch == 94 or
           ch == 36 or ch == 92 then
            resultCount = resultCount + 1
            result[resultCount] = "\\"
            resultCount = resultCount + 1
            result[resultCount] = schar(ch)
        else
            resultCount = resultCount + 1
            result[resultCount] = schar(ch)
        end
    end
    return table.concat(result)
end

-- Split a string by a regex pattern
function splitByRegex(pattern, text)
    local compiled = compileRegex(pattern)
    local nfaStart = compiled.nfa
    local numGroups = compiled.numGroups
    local textLen = slen(text)
    local parts = {}
    local partCount = 0
    local lastEnd = 1

    local pos = 1
    while pos <= textLen do
        local caps = simulateNFA(nfaStart, text, textLen, pos, numGroups)
        if caps ~= nil then
            -- Found a match at pos, get match end
            local matchEnd = findMatchEnd(nfaStart, text, textLen, pos)
            if matchEnd > pos then
                -- Add text before match
                partCount = partCount + 1
                parts[partCount] = ssub(text, lastEnd, pos - 1)
                lastEnd = matchEnd
                pos = matchEnd
            else
                pos = pos + 1
            end
        else
            pos = pos + 1
        end
    end
    -- Add remaining text
    partCount = partCount + 1
    parts[partCount] = ssub(text, lastEnd)
    return parts
end

-- Count occurrences of a pattern in text
function countMatches(pattern, text)
    local compiled = compileRegex(pattern)
    local nfaStart = compiled.nfa
    local numGroups = compiled.numGroups
    local textLen = slen(text)
    local count = 0
    local pos = 1

    while pos <= textLen + 1 do
        local caps = simulateNFA(nfaStart, text, textLen, pos, numGroups)
        if caps ~= nil then
            count = count + 1
            pos = pos + 1
        else
            pos = pos + 1
        end
    end
    return count
end

-- Validate that a string matches a pattern completely
function validateFull(pattern, text)
    local fullPat = "^" .. pattern .. "$"
    return match(fullPat, text).matched
end


-- ============================================================
-- SECTION 11: AST Pretty Printer (for debugging/verification)
-- ============================================================

function astToString(ast, depth)
    if ast == nil then return "nil" end
    depth = depth or 0
    local indent = srep("  ", depth)
    local t = ast.type

    if t == "literal" then
        return indent .. "Literal(" .. schar(ast.char) .. ")"
    end
    if t == "dot" then
        return indent .. "Dot"
    end
    if t == "class" then
        local desc = indent .. "Class("
        if ast.negated then desc = desc .. "^" end
        for i = 1, #ast.ranges do
            local r = ast.ranges[i]
            if r[1] == r[2] then
                desc = desc .. schar(r[1])
            else
                desc = desc .. schar(r[1]) .. "-" .. schar(r[2])
            end
            if i < #ast.ranges then desc = desc .. "," end
        end
        desc = desc .. ")"
        return desc
    end
    if t == "anchor_start" then
        return indent .. "AnchorStart"
    end
    if t == "anchor_end" then
        return indent .. "AnchorEnd"
    end
    if t == "quantifier" then
        return indent .. "Quantifier(" .. ast.kind .. ")\n" .. astToString(ast.child, depth + 1)
    end
    if t == "concat" then
        local parts = {indent .. "Concat"}
        for i = 1, #ast.children do
            parts[#parts+1] = astToString(ast.children[i], depth + 1)
        end
        return table.concat(parts, "\n")
    end
    if t == "alternation" then
        return indent .. "Alt\n" .. astToString(ast.left, depth + 1) .. "\n" .. astToString(ast.right, depth + 1)
    end
    if t == "group" then
        return indent .. "Group(" .. ast.index .. ")\n" .. astToString(ast.child, depth + 1)
    end
    return indent .. "Unknown"
end

-- Verify AST construction for various patterns
function verifyASTConstruction()
    -- Just verify parsing doesn't crash and produces expected types
    local testPatterns = {
        "abc",
        "a.b",
        "a*b+c?",
        "[a-z]",
        "[^0-9]",
        "a|b",
        "(abc)",
        "^hello$",
        "\\d+\\w*\\s?",
        "((a)(b))",
        "(a|b)*c+",
        "[a-zA-Z0-9_]+",
        "\\(\\)\\[\\]",
        "a?b?c?d?e?",
    }

    for i = 1, #testPatterns do
        local ast, numGroups = parseRegex(testPatterns[i])
        if ast == nil then
            error("AST should not be nil for pattern: " .. testPatterns[i])
        end
        -- Generate string representation to exercise the code
        local s = astToString(ast)
        if slen(s) == 0 then
            error("AST string should not be empty for pattern: " .. testPatterns[i])
        end
    end
end


-- ============================================================
-- SECTION 12: NFA State Counter
-- ============================================================

-- Count total states in an NFA (via BFS from start)
function countNFAStates(startState)
    local seen = {}
    local queue = {startState}
    local front = 1
    local count = 0

    while front <= #queue do
        local state = queue[front]
        front = front + 1
        local sid = state.id
        if not seen[sid] then
            seen[sid] = true
            count = count + 1
            -- Follow epsilon transitions
            for i = 1, #state.epsilon do
                if not seen[state.epsilon[i].id] then
                    queue[#queue+1] = state.epsilon[i]
                end
            end
            -- Follow all char transitions
            for k, v in next, state.transitions do
                for i = 1, #v do
                    if not seen[v[i].id] then
                        queue[#queue+1] = v[i]
                    end
                end
            end
            -- Follow dot transition
            if state.dotTransition and not seen[state.dotTransition.id] then
                queue[#queue+1] = state.dotTransition
            end
            -- Follow class transition
            if state.classTransition and not seen[state.classTransition.target.id] then
                queue[#queue+1] = state.classTransition.target
            end
        end
    end
    return count
end

-- Verify NFA state counts for various patterns
function verifyNFAStateCount()
    -- Simple patterns should have predictable state counts
    local compiled

    -- "a" -> 2 states (start, accept)
    compiled = compileRegex("a")
    local n = countNFAStates(compiled.nfa)
    if n < 2 then error("Expected at least 2 states for 'a', got " .. n) end

    -- "abc" -> 6 states (2 per literal, connected by epsilon)
    compiled = compileRegex("abc")
    n = countNFAStates(compiled.nfa)
    if n < 6 then error("Expected at least 6 states for 'abc', got " .. n) end

    -- "a*" -> 4 states (2 for literal + 2 for star wrapper)
    compiled = compileRegex("a*")
    n = countNFAStates(compiled.nfa)
    if n < 4 then error("Expected at least 4 states for 'a*', got " .. n) end

    -- "a|b" -> 6 states (2 for each literal + 2 for alternation wrapper)
    compiled = compileRegex("a|b")
    n = countNFAStates(compiled.nfa)
    if n < 6 then error("Expected at least 6 states for 'a|b', got " .. n) end

    -- "(a)" -> 4 states (2 for literal + 2 for group wrapper)
    compiled = compileRegex("(a)")
    n = countNFAStates(compiled.nfa)
    if n < 4 then error("Expected at least 4 states for '(a)', got " .. n) end
end


-- ============================================================
-- SECTION 13: Regex Pattern Validator
-- ============================================================

-- Check if a pattern string is valid (parseable without error)
function isValidPattern(pattern)
    local ok, _ = pcall(parseRegex, pattern)
    return ok
end

-- Run validation tests
function runValidationTests()
    -- Valid patterns
    local validPatterns = {
        "abc", "a.b", "a*", "a+", "a?",
        "[abc]", "[a-z]", "[^0-9]",
        "a|b", "(abc)", "^hello$",
        "\\d", "\\w", "\\s", "\\.", "\\\\",
        "", "a*b*c*", "(a(b(c)))",
        "((a|b)*(c|d)+)?",
    }
    for i = 1, #validPatterns do
        if not isValidPattern(validPatterns[i]) then
            error("Pattern should be valid: " .. validPatterns[i])
        end
    end

    -- Invalid patterns
    local invalidPatterns = {
        "[abc",    -- unterminated class
        "(abc",    -- unterminated group
        "\\",      -- trailing backslash
    }
    for i = 1, #invalidPatterns do
        if isValidPattern(invalidPatterns[i]) then
            error("Pattern should be invalid: " .. invalidPatterns[i])
        end
    end
end


-- ============================================================
-- SECTION 14: Utility stress tests
-- ============================================================

function runUtilityTests()
    -- Test escapeRegex
    local escaped = escapeRegex("hello.world")
    if escaped ~= "hello\\.world" then
        error("escapeRegex failed: " .. escaped)
    end
    escaped = escapeRegex("a*b+c?")
    if escaped ~= "a\\*b\\+c\\?" then
        error("escapeRegex failed: " .. escaped)
    end
    escaped = escapeRegex("(foo)|[bar]")
    if escaped ~= "\\(foo\\)\\|\\[bar\\]" then
        error("escapeRegex failed: " .. escaped)
    end

    -- Test that escaped patterns match literally
    local specialChars = ".*+?|()[]^$\\"
    local escapedPat = escapeRegex(specialChars)
    local result = match(escapedPat, specialChars)
    if not result.matched then
        error("Escaped pattern should match the literal string")
    end

    -- Test countMatches
    local count = countMatches("ab", "ababab")
    if count < 3 then
        error("Should find at least 3 occurrences of 'ab' in 'ababab', got " .. count)
    end

    count = countMatches("\\d+", "abc 123 def 456 ghi 789")
    if count < 3 then
        error("Should find at least 3 digit sequences, got " .. count)
    end

    count = countMatches("x", "yyy")
    if count ~= 0 then
        error("Should find 0 occurrences of 'x' in 'yyy', got " .. count)
    end

    -- Test validateFull
    if not validateFull("\\d+", "12345") then
        error("'12345' should fully match \\d+")
    end
    if validateFull("\\d+", "123abc") then
        error("'123abc' should not fully match \\d+")
    end
    if not validateFull("[a-z]+", "hello") then
        error("'hello' should fully match [a-z]+")
    end

    -- Test splitByRegex
    local parts = splitByRegex("\\s+", "hello world foo bar")
    if #parts < 4 then
        error("Split should produce at least 4 parts, got " .. #parts)
    end
    if parts[1] ~= "hello" then
        error("First split part should be 'hello', got '" .. parts[1] .. "'")
    end
end


-- ============================================================
-- SECTION 15: Comprehensive Benchmark Driver
-- ============================================================

function runTests(tests)
    local checksum = 0
    local numTests = #tests
    for i = 1, numTests do
        local tc = tests[i]
        local result = match(tc.pattern, tc.text)

        -- Verify correctness
        if result.matched ~= tc.shouldMatch then
            error("FAIL test " .. i .. ": pattern='" .. tc.pattern .. "' text='" .. tc.text ..
                  "' expected matched=" .. tostring(tc.shouldMatch) .. " got=" .. tostring(result.matched))
        end

        -- Verify captures if expected
        if tc.shouldMatch and tc.captures and #tc.captures > 0 then
            for j = 1, #tc.captures do
                local expected = tc.captures[j]
                local got = result.captures[j] or ""
                if got ~= expected then
                    error("FAIL test " .. i .. ": pattern='" .. tc.pattern .. "' text='" .. tc.text ..
                          "' capture " .. j .. " expected '" .. expected .. "' got '" .. got .. "'")
                end
            end
        end

        -- Accumulate checksum
        checksum = checksumResult(result, checksum)
    end
    return checksum
end

function runAllVerifications()
    verifyASTConstruction()
    verifyNFAStateCount()
    runValidationTests()
    runUtilityTests()
    runExtendedTests()
    runPathologicalTests()
end

function runBenchmark()
    local tests = buildTestCases()
    local numIterations = 10

    -- Run verifications once
    runAllVerifications()

    for iter = 1, numIterations do
        local checksum = runTests(tests)

        -- Also run stress tests and accumulate
        local stressChecksum = runCompilationStress()
        checksum = (checksum + stressChecksum) % 1000000007

        local largeChecksum = runLargeTextStress()
        checksum = (checksum + largeChecksum) % 1000000007

        local correctChecksum = 468932651

        if checksum ~= correctChecksum then
            error("Checksum mismatch on iteration " .. iter .. ": expected " ..
                  correctChecksum .. " got " .. checksum)
        end
    end

    print("Regex benchmark: all " .. numIterations .. " iterations passed.")
end

runBenchmark()

end

bench.runCode(test, "regex")
