export local x = 5
x = x + 1

export local s = "a"
s = s .. "b"

export local n
n = 42

export local step = 0
step = step + 1
step = step + 1
step = step + 1

export local sum = 0
for i = 1, 3 do
    sum += i
end

export local prod = 1
local i = 1
while i <= 4 do
    prod = prod * i
    i += 1
end

export local t = { x = 1 }
t = { x = t.x + 1 }

export local a = 1
local b
a, b = a + 1, a + 2

export local closure = 0
closure = (function() return closure + 1 end)()

export local fw
function fw()
    return "forward"
end

export local f = math.max

local function inner(...)
    return f(...)
end

assert(inner("3", 7) == 7)
