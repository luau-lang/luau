-- Tests for interpolated string function calls (parentheses-free syntax)

-- No expressions: desugars to a single string argument, like f "hello"
local function single(s)
    return s
end

assert(single `hello` == "hello")
assert(single `hello world` == "hello world")

-- With expressions: desugars to (template, values, offsets)
local function capture(template, values, offsets)
    return template, values, offsets
end

local x = 42
local tmpl, vals, offs = capture `hello {x}`
assert(tmpl == "hello {x}")
assert(vals[1] == 42)
assert(offs[1][1] == 7)
assert(offs[1][2] == 3)

-- Multiple expressions
local a = 1
local b = 2
local tmpl2, vals2, offs2 = capture `{a} and {b}`
assert(tmpl2 == "{a} and {b}")
assert(vals2[1] == 1)
assert(vals2[2] == 2)
assert(offs2[1][1] == 1)
assert(offs2[1][2] == 3)
assert(offs2[2][1] == 9)
assert(offs2[2][2] == 3)

-- Complex expressions (function calls)
local function double(n)
    return n * 2
end

local tmpl3, vals3, offs3 = capture `result is {double(a)}`
assert(tmpl3 == "result is {double(a)}")
assert(vals3[1] == 2)
assert(offs3[1][1] == 11)
assert(offs3[1][2] == 11)

-- Method calls
local obj = {}
function obj:Info(template, values, offsets)
    return template, values, offsets
end

local tmpl4, vals4, offs4 = obj:Info `hello {x}`
assert(tmpl4 == "hello {x}")
assert(vals4[1] == 42)
assert(offs4[1][1] == 7)
assert(offs4[1][2] == 3)

-- Chaining: f `interp` {table} should work (currying pattern)
local function curried(template, values, offsets)
    return function(context)
        return template, values, offsets, context
    end
end

local ct, cv, co, cc = curried `hello {x}` {key = 1}
assert(ct == "hello {x}")
assert(cv[1] == 42)
assert(cc.key == 1)

-- Expression text extraction via offsets
local name = "Alice"
local tmpl5, vals5, offs5 = capture `User {name} logged in`
local span = string.sub(tmpl5, offs5[1][1], offs5[1][1] + offs5[1][2] - 1)
assert(span == "{name}")
local expr = string.sub(tmpl5, offs5[1][1] + 1, offs5[1][1] + offs5[1][2] - 2)
assert(expr == "name")

-- Side effects: each expression evaluated exactly once, in order
local counter = 0
local function inc()
    counter = counter + 1
    return counter
end

local tmpl6, vals6, offs6 = capture `{inc()} and {inc()}`
assert(vals6[1] == 1)
assert(vals6[2] == 2)
assert(counter == 2)

-- String with escape sequences: offsets are into the processed template
local tmpl7, vals7, offs7 = capture `hello\n{x}`
assert(tmpl7 == "hello\n{x}")
assert(vals7[1] == 42)
assert(offs7[1][1] == 7)
assert(offs7[1][2] == 3)

-- Member expression
local user = {name = "Bob"}
local tmpl8, vals8, offs8 = capture `{user.name} joined`
assert(tmpl8 == "{user.name} joined")
assert(vals8[1] == "Bob")
assert(offs8[1][1] == 1)
assert(offs8[1][2] == 11)

return "OK"
