-- Tests for interpolated string function calls (parentheses-free syntax)
-- and the string.interpparse / string.interp library functions

-- No expressions: desugars to a single string argument, like f "hello"
local function single(s)
    return s
end

assert(single `hello` == "hello")
assert(single `hello world` == "hello world")

-- With expressions: desugars to (template, values)
local function capture(template, values)
    return template, values
end

local x = 42
local tmpl, vals = capture `hello {x}`
assert(tmpl == "hello {x}")
assert(vals[1] == 42)

-- Multiple expressions
local a = 1
local b = 2
local tmpl2, vals2 = capture `{a} and {b}`
assert(tmpl2 == "{a} and {b}")
assert(vals2[1] == 1)
assert(vals2[2] == 2)

-- Complex expressions (function calls)
local function double(n)
    return n * 2
end

local tmpl3, vals3 = capture `result is {double(a)}`
assert(tmpl3 == "result is {double(a)}")
assert(vals3[1] == 2)

-- Method calls
local obj = {}
function obj:Info(template, values)
    return template, values
end

local tmpl4, vals4 = obj:Info `hello {x}`
assert(tmpl4 == "hello {x}")
assert(vals4[1] == 42)

-- Chaining: f `interp` {table} should work (currying pattern)
local function curried(template, values)
    return function(context)
        return template, values, context
    end
end

local ct, cv, cc = curried `hello {x}` {key = 1}
assert(ct == "hello {x}")
assert(cv[1] == 42)
assert(cc.key == 1)

-- Side effects: each expression evaluated exactly once, in order
local counter = 0
local function inc()
    counter = counter + 1
    return counter
end

local tmpl5, vals5 = capture `{inc()} and {inc()}`
assert(vals5[1] == 1)
assert(vals5[2] == 2)
assert(counter == 2)

-- Member expression
local user = {name = "Bob"}
local tmpl6, vals6 = capture `{user.name} joined`
assert(tmpl6 == "{user.name} joined")
assert(vals6[1] == "Bob")

-- Escaped braces: \{ in source produces {{ in template
local tmpl7, vals7 = capture `price is \{100} {x}`
assert(tmpl7 == "price is {{100}} {x}")
assert(vals7[1] == 42)

-- ======================================================
-- string.interpparse tests
-- ======================================================

local exprs1 = string.interpparse("Hello {name}")
assert(#exprs1 == 1)
assert(exprs1[1] == "name")

local exprs2 = string.interpparse("{a} + {b} = {a + b}")
assert(#exprs2 == 3)
assert(exprs2[1] == "a")
assert(exprs2[2] == "b")
assert(exprs2[3] == "a + b")

local exprs3 = string.interpparse("No interpolations")
assert(#exprs3 == 0)

local exprs4 = string.interpparse("{user.name} logged in from {ip}")
assert(#exprs4 == 2)
assert(exprs4[1] == "user.name")
assert(exprs4[2] == "ip")

-- interpparse with doubled braces (escaped literals)
local exprs5 = string.interpparse("price is {{100}} {x}")
assert(#exprs5 == 1)
assert(exprs5[1] == "x")

-- ======================================================
-- string.interp tests
-- ======================================================

-- Sequential values
assert(string.interp("Hello {name}", {"Alice"}) == "Hello Alice")
assert(string.interp("{a} + {b} = {a + b}", {1, 2, 3}) == "1 + 2 = 3")

-- Associative values
assert(string.interp("Hello {name}", {name = "Alice"}) == "Hello Alice")

-- No expressions
assert(string.interp("plain string", {}) == "plain string")

-- Doubled braces in template
assert(string.interp("price is {{100}}", {}) == "price is {100}")

-- Multiple types
assert(string.interp("{x} is {y}", {true, nil}) == "true is nil")

-- Integration: round-trip with interpparse
local template = "{user.name} logged in from {ip}"
local values = {"Alice", "10.0.0.1"}
local rendered = string.interp(template, values)
assert(rendered == "Alice logged in from 10.0.0.1")
local exprs = string.interpparse(template)
assert(exprs[1] == "user.name")
assert(exprs[2] == "ip")

-- Integration: use with actual interpolated string call
local function logInfo(self, template, values)
    local rendered = string.interp(template, values)
    local exprs = string.interpparse(template)
    return rendered, exprs
end

local logger = {Info = logInfo}
local name = "Alice"
local rendered2, exprs6 = logger:Info `Hello {name}`
assert(rendered2 == "Hello Alice")
assert(exprs6[1] == "name")

return "OK"
