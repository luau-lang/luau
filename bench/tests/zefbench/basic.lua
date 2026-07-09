local function prequire(name) local success, result = pcall(require, name); return success and result end
local bench = script and require(script.Parent.bench_support) or prequire("bench_support") or require("../../bench_support")

function test()


-- Basic from ARES-6 by fpizlo benchmark, converted from the JS ARES-6 Basic benchmark using declawd.
-- Target runtimes: Luau (lute), Lua 5.5, and LuaJIT.
-- This single file contains the lexer, parser, AST evaluator, and self-checking tests.

-- ===== Bit operations (cross-VM) =====
local band, bor, bxor, lshift, rshift
local _bit32 = rawget(_G, "bit32")
local _bit = rawget(_G, "bit")
if type(_bit32) == "table" then
    band, bor, bxor, lshift, rshift =
        _bit32.band, _bit32.bor, _bit32.bxor, _bit32.lshift, _bit32.rshift
elseif type(_bit) == "table" then
    band, bor, bxor, lshift, rshift =
        _bit.band, _bit.bor, _bit.bxor, _bit.lshift, _bit.rshift
else
    -- Lua 5.3+ native bitwise operators, loaded dynamically so this file still
    -- parses in older Luas.
    band = assert(load("local a,b = ... return (a & b) & 0xffffffff"))
    bor = assert(load("local a,b = ... return (a | b) & 0xffffffff"))
    bxor = assert(load("local a,b = ... return (a ~ b) & 0xffffffff"))
    lshift = assert(load("local a,b = ... return (a << b) & 0xffffffff"))
    rshift = assert(load("local a,b = ... return ((a & 0xffffffff) >> b) & 0xffffffff"))
end

-- ===== Utility =====
local floor = math.floor
local mabs = math.abs
local msqrt = math.sqrt
local mpow = math.pow or function(a, b) return a ^ b end
local mlog = math.log
local msin = math.sin
local mcos = math.cos
local mtan = math.tan
local matan = math.atan
local mexp = math.exp
local mmax = math.max
local unpack_ = table.unpack or unpack

local function msign(x)
    if x > 0 then return 1 elseif x < 0 then return -1 else return 0 end
end

-- Emulate JS ""+number: integer-valued numbers render without a decimal point,
-- to match the expected outputs that were produced by JS.
local function formatNumber(n)
    if type(n) ~= "number" then return tostring(n) end
    if n ~= n then return "NaN" end
    if n == math.huge then return "Infinity" end
    if n == -math.huge then return "-Infinity" end
    if n == floor(n) and mabs(n) < 1e16 then
        return string.format("%d", n)
    end
    return tostring(n)
end

local function strlower(s) return string.lower(s) end

-- ===== CaselessMap =====
local CaselessMap = {}
CaselessMap.__index = CaselessMap

function CaselessMap.new(other)
    local self = setmetatable({ _map = {} }, CaselessMap)
    if other then
        for k, v in pairs(other._map) do self._map[k] = v end
    end
    return self
end

function CaselessMap:set(key, value)
    self._map[strlower(key)] = value
end

function CaselessMap:has(key)
    return self._map[strlower(key)] ~= nil
end

function CaselessMap:get(key)
    return self._map[strlower(key)]
end

-- ===== Number/Array/Function values =====
local NumberValue = {}
NumberValue.__index = NumberValue

function NumberValue.new(value)
    return setmetatable({ value = value or 0 }, NumberValue)
end

function NumberValue:apply(state, parameters)
    if #parameters ~= 0 then
        state:abort("Should not pass arguments to simple numeric variables")
    end
    return self.value
end

function NumberValue:leftApply(state, parameters)
    if #parameters ~= 0 then
        state:abort("Should not pass arguments to simple numeric variables")
    end
    return self
end

function NumberValue:assign(v)
    self.value = v
end

local NumberArray = {}
NumberArray.__index = NumberArray

function NumberArray.new(dim)
    local function allocate(index)
        local result = {}
        local size = dim[index]
        if index + 1 <= #dim then
            for i = 1, size do result[i] = allocate(index + 1) end
        else
            for i = 1, size do result[i] = NumberValue.new() end
        end
        return result
    end
    return setmetatable({ _array = allocate(1), _dim = dim }, NumberArray)
end

function NumberArray:apply(state, parameters)
    return self:leftApply(state, parameters):apply(state, {})
end

function NumberArray:leftApply(state, parameters)
    if #self._dim ~= #parameters then
        state:abort("Expected " .. #self._dim .. " arguments but " .. #parameters .. " were passed.")
    end
    local result = self._array
    local base = state.program.base
    for i = 1, #parameters do
        local idx = floor(parameters[i])
        local size = self._dim[i]
        if not (idx >= base) or not (idx < size) then
            state:abort("Index out of bounds: " .. idx)
        end
        result = result[idx + 1]  -- Lua is 1-indexed; stored 0..dim-1 as 1..dim
    end
    return result
end

local NativeFunction = {}
NativeFunction.__index = NativeFunction

function NativeFunction.new(nargs, callback)
    return setmetatable({ _nargs = nargs, _callback = callback }, NativeFunction)
end

function NativeFunction:apply(state, parameters)
    if self._nargs ~= #parameters then
        state:abort("Expected " .. self._nargs .. " arguments but " .. #parameters .. " were passed")
    end
    if self._nargs == 0 then return self._callback() end
    if self._nargs == 1 then return self._callback(parameters[1]) end
    return self._callback(unpack_(parameters))
end

function NativeFunction:leftApply(state, _)
    state:abort("Cannot use a native function as an lvalue")
end

-- ===== RNG (Robert Jenkins 32-bit, matching Octane/Apple ARES-6) =====
local function createRNG(seed)
    seed = seed % 0x100000000
    return function()
        seed = (seed + 0x7ed55d16 + lshift(seed, 12)) % 0x100000000
        seed = bxor(bxor(seed, 0xc761c23c), rshift(seed, 19)) % 0x100000000
        seed = (seed + 0x165667b1 + lshift(seed, 5)) % 0x100000000
        seed = bxor(seed + 0xd3a2646c, lshift(seed, 9)) % 0x100000000
        seed = (seed + 0xfd7046c5 + lshift(seed, 3)) % 0x100000000
        seed = bxor(bxor(seed, 0xb55a4f09), rshift(seed, 16)) % 0x100000000
        return band(seed, 0xfffffff) / 0x10000000
    end
end

local function createRNGWithFixedSeed()
    return createRNG(49734321)
end

-- ===== State =====
local State = {}
State.__index = State

function State.new(program)
    local self = setmetatable({}, State)
    self.values = CaselessMap.new()
    self.stringValues = CaselessMap.new()
    self.sideState = {}  -- keyed by AST node table
    self.statement = nil
    self.nextLineNumber = 0
    self.subStack = {}
    self.dataIndex = 0
    self.program = program
    self.rng = createRNGWithFixedSeed()
    self.output = ""

    local rng = self.rng
    self.values:set("abs", NativeFunction.new(1, function(x) return mabs(x) end))
    self.values:set("atn", NativeFunction.new(1, function(x) return matan(x) end))
    self.values:set("cos", NativeFunction.new(1, function(x) return mcos(x) end))
    self.values:set("exp", NativeFunction.new(1, function(x) return mexp(x) end))
    self.values:set("int", NativeFunction.new(1, function(x) return floor(x) end))
    self.values:set("log", NativeFunction.new(1, function(x) return mlog(x) end))
    self.values:set("rnd", NativeFunction.new(0, function() return rng() end))
    self.values:set("sgn", NativeFunction.new(1, function(x) return msign(x) end))
    self.values:set("sin", NativeFunction.new(1, function(x) return msin(x) end))
    self.values:set("sqr", NativeFunction.new(1, function(x) return msqrt(x) end))
    self.values:set("tan", NativeFunction.new(1, function(x) return mtan(x) end))
    return self
end

function State:getValue(name, numParameters)
    if self.values:has(name) then return self.values:get(name) end
    local result
    if numParameters == 0 then
        result = NumberValue.new()
    else
        local dim = {}
        for i = 1, numParameters do dim[i] = 11 end
        result = NumberArray.new(dim)
    end
    self.values:set(name, result)
    return result
end

function State:getSideState(key)
    local s = self.sideState[key]
    if not s then
        s = {}
        self.sideState[key] = s
    end
    return s
end

function State:abort(text)
    if not self.statement then
        error("At beginning of execution: " .. text)
    end
    error("At " .. self.statement.sourceLineNumber .. ": " .. text)
end

function State:validate(predicate, text)
    if not predicate then self:abort(text) end
end

-- ===== AST evaluators =====
local Basic = {}

function Basic.NumberApply(self, state)
    local params = {}
    for i, v in ipairs(self.parameters) do params[i] = v:evaluate(state) end
    return state:getValue(self.name, #params):apply(state, params)
end

function Basic.Variable(self, state)
    local params = {}
    for i, v in ipairs(self.parameters) do params[i] = v:evaluate(state) end
    return state:getValue(self.name, #params):leftApply(state, params)
end

function Basic.Const(self, _)
    return self.value
end

function Basic.NumberPow(self, state)
    return self.left:evaluate(state) ^ self.right:evaluate(state)
end

function Basic.NumberMul(self, state)
    return self.left:evaluate(state) * self.right:evaluate(state)
end

function Basic.NumberDiv(self, state)
    return self.left:evaluate(state) / self.right:evaluate(state)
end

function Basic.NumberNeg(self, state)
    return -self.term:evaluate(state)
end

function Basic.NumberAdd(self, state)
    return self.left:evaluate(state) + self.right:evaluate(state)
end

function Basic.NumberSub(self, state)
    return self.left:evaluate(state) - self.right:evaluate(state)
end

function Basic.StringVar(self, state)
    local value = state.stringValues:get(self.name)
    if value == nil then state:abort("Could not find string variable " .. self.name) end
    return value
end

function Basic.Equals(self, state)
    return self.left:evaluate(state) == self.right:evaluate(state)
end

function Basic.NotEquals(self, state)
    return self.left:evaluate(state) ~= self.right:evaluate(state)
end

function Basic.LessThan(self, state)
    return self.left:evaluate(state) < self.right:evaluate(state)
end

function Basic.GreaterThan(self, state)
    return self.left:evaluate(state) > self.right:evaluate(state)
end

function Basic.LessEqual(self, state)
    return self.left:evaluate(state) <= self.right:evaluate(state)
end

function Basic.GreaterEqual(self, state)
    return self.left:evaluate(state) >= self.right:evaluate(state)
end

-- Statement processors. Unlike the JS version these are plain functions rather
-- than generators; Print writes directly to state.output and Input reads from
-- state.inputs (neither is needed for the self-check but is retained).

function Basic.GoTo(self, state)
    state.nextLineNumber = self.target
end

function Basic.GoSub(self, state)
    table.insert(state.subStack, state.nextLineNumber)
    state.nextLineNumber = self.target
end

function Basic.Let(self, state)
    self.variable:evaluate(state):assign(self.expression:evaluate(state))
end

function Basic.If(self, state)
    if self.condition:evaluate(state) then
        state.nextLineNumber = self.target
    end
end

function Basic.Return(self, state)
    state:validate(#state.subStack > 0, "Not in a subroutine")
    state.nextLineNumber = table.remove(state.subStack)
end

function Basic.Stop(_, state)
    state.nextLineNumber = nil
end

function Basic.On(self, state)
    local index = self.expression:evaluate(state)
    if not (index >= 1) or not (index <= #self.targets) then
        state:abort("Index out of bounds: " .. index)
    end
    state.nextLineNumber = self.targets[floor(index) + 1]
end

function Basic.For(self, state)
    local sideState = state:getSideState(self)
    sideState.variable = state:getValue(self.variable, 0):leftApply(state, {})
    sideState.initialValue = self.initial:evaluate(state)
    sideState.limitValue = self.limit:evaluate(state)
    sideState.stepValue = self.step:evaluate(state)
    sideState.variable:assign(sideState.initialValue)
    local limit = sideState.limitValue
    local signStep = msign(sideState.stepValue)
    sideState.shouldStop = function()
        return (sideState.variable.value - limit) * signStep > 0
    end
    if sideState.shouldStop() then
        state.nextLineNumber = self.target.lineNumber + 1
    end
end

function Basic.Next(self, state)
    local sideState = state:getSideState(self.target)
    sideState.variable:assign(sideState.variable.value + sideState.stepValue)
    if sideState.shouldStop() then return end
    state.nextLineNumber = self.target.lineNumber + 1
end

function Basic.Print(self, state)
    local s = ""
    for _, item in ipairs(self.items) do
        local kind = item.kind
        if kind == "comma" then
            while #s % 14 ~= 0 do s = s .. " " end
        elseif kind == "tab" then
            local v = item.value:evaluate(state)
            v = mmax(floor(v + 0.5), 1)
            while #s % v ~= 0 do s = s .. " " end
        elseif kind == "string" then
            s = s .. item.value:evaluate(state)
        elseif kind == "number" then
            s = s .. formatNumber(item.value:evaluate(state))
        else
            error("Bad item kind: " .. tostring(kind))
        end
    end
    state.output = state.output .. s .. "\n"
end

function Basic.Input(self, state)
    local results = state:consumeInput(#self.items)
    state:validate(results ~= nil and #results == #self.items,
        "Input did not get the right number of items")
    for i, item in ipairs(self.items) do
        item:evaluate(state):assign(results[i])
    end
end

function Basic.Read(self, state)
    for _, item in ipairs(self.items) do
        state:validate(state.dataIndex < #state.program.data,
            "Attempting to read past the end of data")
        state.dataIndex = state.dataIndex + 1
        item:assign(state.program.data[state.dataIndex])
    end
end

function Basic.Restore(_, state)
    state.dataIndex = 0
end

function Basic.Dim(self, state)
    for _, item in ipairs(self.items) do
        state:validate(not state.values:has(item.name),
            "Variable " .. item.name .. " already exists")
        state:validate(#item.bounds > 0, "Dim statement is for arrays")
        local dim = {}
        for i, b in ipairs(item.bounds) do dim[i] = b + 1 end
        state.values:set(item.name, NumberArray.new(dim))
    end
end

function Basic.End(_, state)
    state.nextLineNumber = nil
end

-- Mark statements that terminate a block (for parseStatements)
local blockEndProcs = {}
blockEndProcs[Basic.Next] = true
blockEndProcs[Basic.End] = true

function Basic.Program(self, state)
    state:validate(state.program == self, "State must match program")
    local maxLineNumber = 0
    for k, _ in pairs(self.statements) do
        if k > maxLineNumber then maxLineNumber = k end
    end
    while state.nextLineNumber ~= nil do
        state:validate(state.nextLineNumber <= maxLineNumber,
            "Went out of bounds of the program")
        local lineNum = state.nextLineNumber
        state.nextLineNumber = lineNum + 1
        local statement = self.statements[lineNum]
        if statement ~= nil and statement.process ~= nil then
            state.statement = statement
            statement:process(state)
        end
    end
end

-- ===== Lexer =====
-- Pattern helpers: Lua patterns are simpler than JS regex. We match explicitly.

local KEYWORDS = {
    base=true, data=true, def=true, dim=true, ["end"]=true, ["for"]=true,
    go=true, gosub=true, ["goto"]=true, ["if"]=true, input=true, let=true,
    next=true, ["on"]=true, option=true, print=true, randomize=true,
    read=true, restore=true, ["return"]=true, step=true, stop=true,
    sub=true, ["then"]=true, to=true
}

local function isDigit(c) return c >= "0" and c <= "9" end
local function isAlpha(c)
    return (c >= "a" and c <= "z") or (c >= "A" and c <= "Z") or c == "_"
end
local function isAlnum(c) return isAlpha(c) or isDigit(c) end

local function lex(source)
    local tokens = {}
    local sourceLineNumber = 0
    for rawLine in (source .. "\n"):gmatch("([^\n]*)\n") do
        sourceLineNumber = sourceLineNumber + 1
        local line = rawLine
        local pos = 1
        local len = #line

        local function skipWs()
            while pos <= len do
                local c = line:sub(pos, pos)
                if c == " " or c == "\t" or c == "\r" then
                    pos = pos + 1
                else
                    break
                end
            end
        end

        skipWs()
        if pos > len then
            -- blank line: emit nothing (the JS lexer yields a newline, but the
            -- parser expects a userLineNumber to start a statement; our source
            -- always has statements, and we emit newLine at end-of-line below
            -- only if we saw a line number).
            -- Actually the JS lexer will throw on a blank line due to the line
            -- number check. We accept blank lines quietly.
        else
            -- Consume the leading line number
            local numStart = pos
            while pos <= len and isDigit(line:sub(pos, pos)) do pos = pos + 1 end
            if numStart == pos then
                error("At line " .. sourceLineNumber .. ": Expect line number: " .. line:sub(numStart))
            end
            local numStr = line:sub(numStart, pos - 1)
            local userLineNumber = tonumber(numStr)
            tokens[#tokens + 1] = {
                kind = "userLineNumber", string = numStr,
                sourceLineNumber = sourceLineNumber, userLineNumber = userLineNumber
            }

            skipWs()

            while pos <= len do
                local c = line:sub(pos, pos)

                -- Remark: "rem " followed by anything
                if (c == "r" or c == "R") and pos + 3 <= len then
                    local c2 = line:sub(pos + 1, pos + 1)
                    local c3 = line:sub(pos + 2, pos + 2)
                    local c4 = line:sub(pos + 3, pos + 3)
                    if (c2 == "e" or c2 == "E") and (c3 == "m" or c3 == "M")
                            and (c4 == " " or c4 == "\t") then
                        local rest = line:sub(pos)
                        tokens[#tokens + 1] = {
                            kind = "remark", string = rest,
                            sourceLineNumber = sourceLineNumber,
                            userLineNumber = userLineNumber
                        }
                        pos = len + 1
                        break
                    end
                end

                if isAlpha(c) then
                    -- identifier or keyword
                    local start = pos
                    pos = pos + 1
                    while pos <= len and isAlnum(line:sub(pos, pos)) do
                        pos = pos + 1
                    end
                    local word = line:sub(start, pos - 1)
                    local kind
                    if KEYWORDS[strlower(word)] then
                        kind = "keyword"
                    else
                        kind = "identifier"
                    end
                    tokens[#tokens + 1] = {
                        kind = kind, string = word,
                        sourceLineNumber = sourceLineNumber,
                        userLineNumber = userLineNumber
                    }
                elseif isDigit(c) or (c == "." and pos + 1 <= len and isDigit(line:sub(pos + 1, pos + 1))) then
                    -- number: int, int.frac?, .frac, optional e[+-]?digits
                    local start = pos
                    while pos <= len and isDigit(line:sub(pos, pos)) do pos = pos + 1 end
                    if pos <= len and line:sub(pos, pos) == "." then
                        pos = pos + 1
                        while pos <= len and isDigit(line:sub(pos, pos)) do pos = pos + 1 end
                    end
                    local e = pos <= len and line:sub(pos, pos)
                    if e == "e" or e == "E" then
                        pos = pos + 1
                        local s = pos <= len and line:sub(pos, pos)
                        if s == "+" or s == "-" then pos = pos + 1 end
                        while pos <= len and isDigit(line:sub(pos, pos)) do pos = pos + 1 end
                    end
                    local str = line:sub(start, pos - 1)
                    tokens[#tokens + 1] = {
                        kind = "number", string = str, value = tonumber(str),
                        sourceLineNumber = sourceLineNumber,
                        userLineNumber = userLineNumber
                    }
                elseif c == '"' then
                    local start = pos
                    pos = pos + 1
                    while pos <= len do
                        if line:sub(pos, pos) == '"' then
                            if pos + 1 <= len and line:sub(pos + 1, pos + 1) == '"' then
                                pos = pos + 2
                            else
                                pos = pos + 1
                                break
                            end
                        else
                            pos = pos + 1
                        end
                    end
                    local str = line:sub(start, pos - 1)
                    local value = ""
                    local i = 2
                    while i <= #str - 1 do
                        local ch = str:sub(i, i)
                        if ch == '"' then i = i + 1 end  -- skip the escape quote
                        value = value .. ch
                        i = i + 1
                    end
                    tokens[#tokens + 1] = {
                        kind = "string", string = str, value = value,
                        sourceLineNumber = sourceLineNumber,
                        userLineNumber = userLineNumber
                    }
                else
                    -- Operator
                    local two = pos + 1 <= len and line:sub(pos, pos + 1) or nil
                    local opStr
                    if two == "<>" or two == "<=" or two == ">=" then
                        opStr = two
                        pos = pos + 2
                    elseif c == "-" or c == "+" or c == "*" or c == "/" or c == "^"
                            or c == "(" or c == ")" or c == "<" or c == ">" or c == "="
                            or c == "," or c == "$" or c == ";" then
                        opStr = c
                        pos = pos + 1
                    else
                        error("At line " .. sourceLineNumber .. ": Cannot lex token: " .. line:sub(pos))
                    end
                    tokens[#tokens + 1] = {
                        kind = "operator", string = opStr,
                        sourceLineNumber = sourceLineNumber,
                        userLineNumber = userLineNumber
                    }
                end

                skipWs()
            end

            tokens[#tokens + 1] = {
                kind = "newLine", string = "\n",
                sourceLineNumber = sourceLineNumber,
                userLineNumber = userLineNumber
            }
        end
    end
    return tokens
end

-- ===== Parser =====
local function parse(tokens)
    local program
    local idx = 1
    local pushBack = {}

    local function nextToken()
        if #pushBack > 0 then
            return table.remove(pushBack)
        end
        if idx > #tokens then
            return { kind = "endOfFile", string = "<end of file>" }
        end
        local t = tokens[idx]
        idx = idx + 1
        return t
    end

    local function pushToken(t) pushBack[#pushBack + 1] = t end

    local function peekToken()
        local t = nextToken()
        pushToken(t)
        return t
    end

    local function consumeKind(kind)
        local t = nextToken()
        if t.kind ~= kind then
            error("At " .. tostring(t.sourceLineNumber) .. ": expected " .. kind .. " but got: " .. t.string)
        end
        return t
    end

    local function consumeToken(str)
        local t = nextToken()
        if strlower(t.string) ~= strlower(str) then
            error("At " .. tostring(t.sourceLineNumber) .. ": expected " .. str .. " but got: " .. t.string)
        end
        return t
    end

    local parseNumericExpression
    local parseStringExpression
    local isStringExpression

    local function parseVariable()
        local name = consumeKind("identifier").string
        local result = { evaluate = Basic.Variable, name = name, parameters = {} }
        if peekToken().string == "(" then
            repeat
                nextToken()
                result.parameters[#result.parameters + 1] = parseNumericExpression()
            until peekToken().string ~= ","
            consumeToken(")")
        end
        return result
    end

    parseNumericExpression = function()
        local function parsePrimary()
            local t = nextToken()
            if t.kind == "identifier" then
                local r = { evaluate = Basic.NumberApply, name = t.string, parameters = {} }
                if peekToken().string == "(" then
                    repeat
                        nextToken()
                        r.parameters[#r.parameters + 1] = parseNumericExpression()
                    until peekToken().string ~= ","
                    consumeToken(")")
                end
                return r
            elseif t.kind == "number" then
                return { evaluate = Basic.Const, value = t.value }
            elseif t.kind == "operator" and t.string == "(" then
                local r = parseNumericExpression()
                consumeToken(")")
                return r
            end
            error("At " .. tostring(t.sourceLineNumber) .. ": expected identifier, number, or (, but got: " .. t.string)
        end

        local function parseFactor()
            local primary = parsePrimary()
            while true do
                if peekToken().string == "^" then
                    nextToken()
                    primary = { evaluate = Basic.NumberPow, left = primary, right = parsePrimary() }
                else break end
            end
            return primary
        end

        local function parseTerm()
            local factor = parseFactor()
            while true do
                local s = peekToken().string
                if s == "*" then
                    nextToken()
                    factor = { evaluate = Basic.NumberMul, left = factor, right = parseFactor() }
                elseif s == "/" then
                    nextToken()
                    factor = { evaluate = Basic.NumberDiv, left = factor, right = parseFactor() }
                else break end
            end
            return factor
        end

        local negate = false
        local s = peekToken().string
        if s == "+" then nextToken()
        elseif s == "-" then negate = true; nextToken() end

        local term = parseTerm()
        if negate then term = { evaluate = Basic.NumberNeg, term = term } end

        while true do
            local s2 = peekToken().string
            if s2 == "+" then
                nextToken()
                term = { evaluate = Basic.NumberAdd, left = term, right = parseTerm() }
            elseif s2 == "-" then
                nextToken()
                term = { evaluate = Basic.NumberSub, left = term, right = parseTerm() }
            else break end
        end
        return term
    end

    isStringExpression = function()
        local t = nextToken()
        if t.kind == "string" then
            pushToken(t); return true
        end
        if t.kind == "identifier" then
            local result = peekToken().string == "$"
            pushToken(t)
            return result
        end
        pushToken(t)
        return false
    end

    parseStringExpression = function()
        local t = nextToken()
        if t.kind == "string" then
            return { evaluate = Basic.Const, value = t.value }
        elseif t.kind == "identifier" then
            consumeToken("$")
            return { evaluate = Basic.StringVar, name = t.string }
        end
        error("At " .. tostring(t.sourceLineNumber) .. ": expected string expression but got " .. t.string)
    end

    local function parseRelationalExpression()
        if isStringExpression() then
            local left = parseStringExpression()
            local op = nextToken()
            local ev
            if op.string == "=" then ev = Basic.Equals
            elseif op.string == "<>" then ev = Basic.NotEquals
            else error("At " .. tostring(op.sourceLineNumber) .. ": expected a string comparison operator but got: " .. op.string) end
            return { evaluate = ev, left = left, right = parseStringExpression() }
        end
        local left = parseNumericExpression()
        local op = nextToken()
        local ev
        if op.string == "=" then ev = Basic.Equals
        elseif op.string == "<>" then ev = Basic.NotEquals
        elseif op.string == "<" then ev = Basic.LessThan
        elseif op.string == ">" then ev = Basic.GreaterThan
        elseif op.string == "<=" then ev = Basic.LessEqual
        elseif op.string == ">=" then ev = Basic.GreaterEqual
        else error("At " .. tostring(op.sourceLineNumber) .. ": expected a numeric comparison operator but got: " .. op.string) end
        return { evaluate = ev, left = left, right = parseNumericExpression() }
    end

    local function parseNonNegativeInteger()
        local t = nextToken()
        if not t.string:match("^[0-9]+$") then
            error("At " .. tostring(t.sourceLineNumber) .. ": expected a line number but got: " .. t.string)
        end
        return t.value
    end

    local parseStatement
    local parseStatements

    parseStatements = function()
        local statement
        repeat
            statement = parseStatement()
        until statement.process and blockEndProcs[statement.process]
        return statement
    end

    parseStatement = function()
        local statement = {}
        statement.lineNumber = consumeKind("userLineNumber").userLineNumber
        program.statements[statement.lineNumber] = statement

        local command = nextToken()
        statement.sourceLineNumber = command.sourceLineNumber

        if command.kind == "keyword" then
            local cmd = strlower(command.string)
            if cmd == "def" then
                statement.process = nil  -- not exercised by benchmark; keep minimal
                statement.name = consumeKind("identifier")
                statement.parameters = {}
                if peekToken().string == "(" then
                    repeat
                        nextToken()
                        statement.parameters[#statement.parameters + 1] = consumeKind("identifier")
                    until peekToken().string ~= ","
                end
                statement.expression = parseNumericExpression()
            elseif cmd == "let" then
                statement.process = Basic.Let
                statement.variable = parseVariable()
                consumeToken("=")
                statement.expression = parseNumericExpression()
            elseif cmd == "go" then
                local nxt = nextToken()
                if strlower(nxt.string) == "to" then
                    statement.process = Basic.GoTo
                    statement.target = parseNonNegativeInteger()
                elseif strlower(nxt.string) == "sub" then
                    statement.process = Basic.GoSub
                    statement.target = parseNonNegativeInteger()
                else
                    error("At " .. tostring(nxt.sourceLineNumber) .. ": expected to or sub but got: " .. nxt.string)
                end
            elseif cmd == "goto" then
                statement.process = Basic.GoTo
                statement.target = parseNonNegativeInteger()
            elseif cmd == "gosub" then
                statement.process = Basic.GoSub
                statement.target = parseNonNegativeInteger()
            elseif cmd == "if" then
                statement.process = Basic.If
                statement.condition = parseRelationalExpression()
                consumeToken("then")
                statement.target = parseNonNegativeInteger()
            elseif cmd == "return" then
                statement.process = Basic.Return
            elseif cmd == "stop" then
                statement.process = Basic.Stop
            elseif cmd == "on" then
                statement.process = Basic.On
                statement.expression = parseNumericExpression()
                if peekToken().string == "go" then
                    consumeToken("go"); consumeToken("to")
                else
                    consumeToken("goto")
                end
                statement.targets = {}
                while true do
                    statement.targets[#statement.targets + 1] = parseNonNegativeInteger()
                    if peekToken().string ~= "," then break end
                    nextToken()
                end
            elseif cmd == "for" then
                statement.process = Basic.For
                statement.variable = consumeKind("identifier").string
                consumeToken("=")
                statement.initial = parseNumericExpression()
                consumeToken("to")
                statement.limit = parseNumericExpression()
                if peekToken().string == "step" then
                    nextToken()
                    statement.step = parseNumericExpression()
                else
                    statement.step = { evaluate = Basic.Const, value = 1 }
                end
                consumeKind("newLine")
                local lastStatement = parseStatements()
                if lastStatement.process ~= Basic.Next then
                    error("At " .. tostring(lastStatement.sourceLineNumber) .. ": expected next statement")
                end
                if lastStatement.variable ~= statement.variable then
                    error("At " .. tostring(lastStatement.sourceLineNumber) .. ": expected next for " ..
                          statement.variable .. " but got " .. lastStatement.variable)
                end
                lastStatement.target = statement
                statement.target = lastStatement
                return statement
            elseif cmd == "next" then
                statement.process = Basic.Next
                statement.variable = consumeKind("identifier").string
            elseif cmd == "print" then
                statement.process = Basic.Print
                statement.items = {}
                while true do
                    local s = peekToken().string
                    if s == "," then
                        nextToken()
                        statement.items[#statement.items + 1] = { kind = "comma" }
                    elseif s == ";" then
                        nextToken()
                    elseif s == "tab" then
                        nextToken()
                        consumeToken("(")
                        statement.items[#statement.items + 1] =
                            { kind = "tab", value = parseNumericExpression() }
                    elseif s == "\n" then
                        break
                    else
                        if isStringExpression() then
                            statement.items[#statement.items + 1] =
                                { kind = "string", value = parseStringExpression() }
                        else
                            statement.items[#statement.items + 1] =
                                { kind = "number", value = parseNumericExpression() }
                        end
                    end
                end
            elseif cmd == "input" then
                statement.process = Basic.Input
                statement.items = {}
                while true do
                    statement.items[#statement.items + 1] = parseVariable()
                    if peekToken().string ~= "," then break end
                    nextToken()
                end
            elseif cmd == "read" then
                statement.process = Basic.Read
                statement.items = {}
                while true do
                    statement.items[#statement.items + 1] = parseVariable()
                    if peekToken().string ~= "," then break end
                    nextToken()
                end
            elseif cmd == "restore" then
                statement.process = Basic.Restore
            elseif cmd == "data" then
                while true do
                    -- parseConstant, simplified: +n, -n, string, number
                    local s = peekToken().string
                    if s == "+" then
                        nextToken()
                        program.data[#program.data + 1] = consumeKind("number").value
                    elseif s == "-" then
                        nextToken()
                        program.data[#program.data + 1] = -consumeKind("number").value
                    else
                        if isStringExpression() then
                            program.data[#program.data + 1] = consumeKind("string").value
                        else
                            program.data[#program.data + 1] = consumeKind("number").value
                        end
                    end
                    if peekToken().string ~= "," then break end
                    nextToken()
                end
            elseif cmd == "dim" then
                statement.process = Basic.Dim
                statement.items = {}
                while true do
                    local name = consumeKind("identifier").string
                    consumeToken("(")
                    local bounds = {}
                    bounds[#bounds + 1] = parseNonNegativeInteger()
                    if peekToken().string == "," then
                        nextToken()
                        bounds[#bounds + 1] = parseNonNegativeInteger()
                    end
                    consumeToken(")")
                    statement.items[#statement.items + 1] = { name = name, bounds = bounds }
                    if peekToken().string ~= "," then break end
                    consumeToken(",")
                end
            elseif cmd == "option" then
                consumeToken("base")
                local base = parseNonNegativeInteger()
                if base ~= 0 and base ~= 1 then
                    error("At " .. tostring(command.sourceLineNumber) .. ": unexpected base: " .. base)
                end
                program.base = base
            elseif cmd == "randomize" then
                -- Basic.Randomize would reseed from a random source. Our tests
                -- don't use it; left as a no-op processor.
                statement.process = function(_, state)
                    state.rng = createRNGWithFixedSeed()
                end
            elseif cmd == "end" then
                statement.process = Basic.End
            else
                error("At " .. tostring(command.sourceLineNumber) .. ": unexpected command but got: " .. command.string)
            end
        elseif command.kind == "remark" then
            -- Ignore
        else
            error("At " .. tostring(command.sourceLineNumber) .. ": expected command but got: " .. command.string .. " (of kind " .. command.kind .. ")")
        end

        consumeKind("newLine")
        return statement
    end

    local function parseProgram()
        program = {
            process = Basic.Program,
            statements = {},
            data = {},
            base = 0,
        }
        local lastStatement = parseStatements()
        if lastStatement.process ~= Basic.End then
            error("At " .. tostring(lastStatement.sourceLineNumber) .. ": expected end")
        end
        return program
    end

    return { program = parseProgram }
end

-- ===== Driver =====
local function prepare(source)
    local tokens = lex(source)
    local program = parse(tokens).program()
    local state = State.new(program)
    function state:consumeInput(n)
        local items = self.inputs or {}
        local out = {}
        for i = 1, n do out[i] = items[i] end
        for i = 1, n do table.remove(items, 1) end
        return out
    end
    program:process(state)
    return state
end

local function simulate(source, inputs)
    local tokens = lex(source)
    local program = parse(tokens).program()
    local state = State.new(program)
    state.inputs = {}
    if inputs then
        for i, v in ipairs(inputs) do state.inputs[i] = v end
    end
    function state:consumeInput(n)
        local out = {}
        for i = 1, n do out[i] = self.inputs[i] end
        for i = 1, n do table.remove(self.inputs, 1) end
        return out
    end
    program:process(state)
    return state.output
end

-- ===== Tests (self-check, matching benchmark.js) =====
local function expect(program, expected, ...)
    local inputs = { ... }
    local result = simulate(program, inputs)
    if result ~= expected then
        error("Program " .. program .. " produced:\n" .. result ..
              "\nbut we expected:\n" .. expected)
    end
end

local EXPECTED_HELLO = "hello, world!\n"

local EXPECTED_COUNT = "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n"

local EXPECTED_RND100 = "98\n"

-- Long expected outputs are stored in long-bracket strings below the function
-- to keep things readable. Forward-declared here so runIteration can see them.
local EXPECTED_RND2000 = [[
1974
697
1126
1998
1658
264
1650
1677
226
117
492
861
877
1969
38
1039
197
1261
1102
1522
916
1683
1943
1835
476
1898
939
176
966
908
474
614
1326
564
1916
728
524
162
1303
758
832
1279
1856
1876
982
6
1613
1781
681
1238
494
1583
1953
788
1026
347
1116
1465
514
583
463
1970
1573
412
1256
1453
838
1538
1984
1598
209
411
1700
546
861
91
132
884
378
693
11
433
1719
860
164
472
231
1786
806
811
106
1697
118
980
890
1199
227
1667
1933
1903
1390
1595
923
1746
39
1361
117
1297
923
901
1180
818
1444
269
933
327
1744
1082
1527
1260
622
528
318
856
296
1796
1574
585
1871
111
827
1725
1320
1868
1695
1914
216
63
1847
156
671
893
127
1867
811
279
913
310
814
907
1363
1624
1670
478
714
436
355
1484
1628
1208
800
611
917
829
830
273
1791
340
214
992
1444
442
1555
144
1194
282
180
1228
1251
1883
678
1555
347
72
1661
1828
1090
1183
957
1685
930
475
103
759
1725
1902
1662
1587
61
614
863
1418
321
1050
505
1622
1425
803
589
1511
1098
1051
1554
1898
27
747
813
1544
332
728
1363
771
759
1145
1098
1991
385
230
520
1369
1840
1285
1562
1845
102
760
1874
748
361
575
277
1661
1764
1117
332
757
1766
1722
143
474
1507
1294
1180
1578
904
845
321
496
1911
1784
1116
938
1591
1403
1374
533
1085
452
708
1096
1634
522
564
1397
1357
980
978
1760
1088
1361
1184
314
1242
217
133
1187
1723
646
605
591
46
135
1420
1821
1147
1211
61
244
1307
1551
449
1122
1336
140
880
22
1155
1326
590
1499
1376
112
1771
1897
1071
938
1685
1963
1203
1296
804
1275
453
1387
482
1262
1883
1381
418
1417
1222
1208
1263
632
450
1422
1285
1408
644
665
275
363
1012
165
354
80
609
291
1661
1724
117
407
59
906
1224
136
855
1275
1468
482
1537
1283
1784
1568
1832
452
867
1546
1467
800
45
1225
1890
465
1372
47
1608
193
1345
1847
1059
1788
518
52
1052
1003
1210
1135
1433
519
1558
39
1249
1017
39
1713
1449
1245
1354
82
1140
916
1595
838
607
389
1270
821
247
1692
1305
1211
1960
429
1703
1635
575
1618
1490
1495
682
1256
964
420
1520
1429
1997
396
382
856
1182
296
1295
298
1892
990
711
934
1939
1339
682
1631
1533
742
1520
1281
1332
1042
656
1576
1253
1608
375
169
14
414
1586
1562
1508
1245
303
715
1053
340
915
160
1796
111
925
1872
735
350
107
1913
1653
987
825
1893
1601
460
1228
1526
1613
1359
1854
1352
542
665
109
1874
467
533
1188
1629
851
630
1060
1530
1853
743
765
126
1540
1411
858
1741
284
299
577
1848
1495
283
1886
284
129
1077
1245
1364
1505
176
1012
1663
1306
1586
410
315
660
256
1102
1289
1292
939
762
601
1140
574
1851
44
560
1948
1142
1787
947
948
280
1210
1139
1072
1033
92
1244
1589
1079
22
1514
163
157
1742
1058
514
196
1858
565
354
1413
792
183
526
1724
1007
158
1229
1802
99
1514
708
1276
1802
1564
1387
1235
1132
715
1584
617
1664
1559
1625
1037
601
1175
1713
107
88
384
1634
904
1835
1472
212
1145
443
1617
866
1963
937
1917
855
1215
1867
520
892
1483
1898
1747
1441
289
1609
328
566
271
458
1616
843
1107
507
1090
854
1094
806
166
408
661
334
230
1917
1323
927
1912
673
311
952
1783
1549
1714
1500
450
1498
530
442
607
609
1226
370
1769
1815
788
536
293
115
947
290
1764
243
1219
1851
289
599
1528
150
1859
297
279
1542
1719
1910
551
401
952
1764
946
1835
647
1309
271
275
70
129
1518
972
1164
816
1125
575
588
1456
1154
290
1681
1133
561
343
1360
1035
1158
1365
744
781
58
531
271
1612
1774
28
1480
1312
1855
666
1574
613
42
456
351
727
1503
1115
333
1972
822
1575
848
1087
1262
1671
710
460
1816
287
172
492
1079
582
1236
1756
1792
1095
1205
1894
22
1930
1529
1547
1383
1768
364
1108
1972
287
200
230
1335
187
486
1722
20
963
792
1114
633
1862
1433
829
737
215
1570
378
1677
944
1301
1160
500
150
886
1337
662
1062
290
460
592
1867
872
155
1613
1913
1548
1847
855
1702
952
1894
587
1813
1021
21
654
254
910
1696
1606
679
1222
696
1319
368
447
549
905
1194
189
1766
616
278
1418
1965
872
998
1268
1673
1647
1163
533
1650
1849
1124
1252
1412
703
944
468
1485
1352
681
864
1432
1771
497
956
1794
363
1099
1804
457
1227
1487
446
1993
1576
272
709
1810
330
876
1107
1187
122
1625
472
676
314
1257
1509
350
741
366
33
536
293
1663
1039
1527
126
923
1937
1767
1302
1510
1518
1343
91
1551
1614
1687
1748
137
75
738
1977
751
237
313
566
24
202
889
1716
1460
129
1760
1597
96
1057
1323
1188
1373
537
955
65
1679
1441
1315
398
647
1470
1335
617
331
796
129
1635
1497
836
855
1472
1828
568
862
690
1370
1657
819
45
420
258
1980
672
615
358
852
1148
1897
1306
1092
1405
719
1752
1456
1338
332
351
479
747
249
1977
1671
1061
1685
306
254
1060
764
420
1139
1452
426
835
929
1424
1336
697
191
1697
1897
644
546
982
359
1201
1095
1623
1947
215
10
855
297
551
1037
945
396
211
1059
423
1521
1770
203
1828
879
1179
1912
1028
1416
1845
698
715
1857
817
50
473
1122
126
70
1773
40
1970
1311
826
355
1921
23
526
1717
1397
1932
1075
1652
997
1039
1481
779
415
49
1330
317
1701
690
245
1824
639
799
1240
422
344
1639
20
546
912
1930
1368
1541
1109
369
66
1564
444
1928
1963
1899
744
1593
1702
100
]]

local EXPECTED_PRIMES = [[
2
3
5
7
11
13
17
19
23
29
31
37
41
43
47
53
59
61
67
71
73
79
83
89
97
101
103
107
109
113
127
131
137
139
149
151
157
163
167
173
179
181
191
193
197
199
211
223
227
229
233
239
241
251
257
263
269
271
277
281
283
293
307
311
313
317
331
337
347
349
353
359
367
373
379
383
389
397
401
409
419
421
431
433
439
443
449
457
461
463
467
479
487
491
499
503
509
521
523
541
547
557
563
569
571
577
587
593
599
601
607
613
617
619
631
641
643
647
653
659
661
673
677
683
691
701
709
719
727
733
739
743
751
757
761
769
773
787
797
809
811
821
823
827
829
839
853
857
859
863
877
881
883
887
907
911
919
929
937
941
947
953
967
971
977
983
991
997
1009
1013
1019
1021
1031
1033
1039
1049
1051
1061
1063
1069
1087
1091
1093
1097
1103
1109
1117
1123
1129
1151
1153
1163
1171
1181
1187
1193
1201
1213
1217
1223
1229
1231
1237
1249
1259
1277
1279
1283
1289
1291
1297
1301
1303
1307
1319
1321
1327
1361
1367
1373
1381
1399
1409
1423
1427
1429
1433
1439
1447
1451
1453
1459
1471
1481
1483
1487
1489
1493
1499
1511
1523
1531
1543
1549
1553
1559
1567
1571
1579
1583
1597
1601
1607
1609
1613
1619
1621
1627
1637
1657
1663
1667
1669
1693
1697
1699
1709
1721
1723
1733
1741
1747
1753
1759
1777
1783
1787
1789
1801
1811
1823
1831
1847
1861
1867
1871
1873
1877
1879
1889
1901
1907
1913
1931
1933
1949
1951
1973
1979
1987
1993
1997
1999
]]

local function runIteration()
    expect("10 print \"hello, world!\"\n20 end", EXPECTED_HELLO)
    expect("10 let x = 0\n20 let x = x + 1\n30 print x\n40 if x < 10 then 20\n50 end",
           EXPECTED_COUNT)
    expect("10 print int(rnd * 100)\n20 end\n", EXPECTED_RND100)
    expect("10 let value = int(rnd * 2000)\n20 print value\n30 if value <> 100 then 10\n40 end",
           EXPECTED_RND2000)
    expect("10 dim a(2000)\n20 for i = 2 to 2000\n30 let a(i) = 1\n40 next i\n50 for i = 2 to sqr(2000)\n60 if a(i) = 0 then 100\n70 for j = i ^ 2 to 2000 step i\n80 let a(j) = 0\n90 next j\n100 next i\n110 for i = 2 to 2000\n120 if a(i) = 0 then 140\n130 print i\n140 next i\n150 end\n",
           EXPECTED_PRIMES)
end

-- Run
local numIterations = 30
for i = 1, numIterations do
    runIteration()
end

print("Basic benchmark: all " .. numIterations .. " iterations passed.")

end

bench.runCode(test, "basic")
