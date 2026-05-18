--!nocheck

local function expectpass(s, f)
    f()
    print(`ok: {s}`)
end

local function expectfail(s, expected, f)
    local success, actual = pcall(f)
    assert(not success)
    assert(type(actual) == "string", `{s}: error message was not a string, but a {type(actual)}`)
    assert(string.find(actual, expected), `{s} expected:\n\t{expected}\nActual:\n\t{actual}`)
    print(`ok: {s}`)
end

class Point
    public x
    public y

    function magnitude(self)
        return math.sqrt(self.x * self.x + self.y * self.y)
    end

    function __mul(self, other)
        return Point { x = self.x * other.x, y = self.y * other.y }
    end

    function __add(self, other)
        return Point { x = self.x + other.x, y = self.y + other.y }
    end

    function __eq(self, other)
        return self.x == other.x and self.y == other.y
    end

    function zero()
        return Point { x = 0, y = 0 }
    end

    function asserttriple(self)
        local mag = self:magnitude()
        assert(mag == math.ceil(mag), "Not a pythagorean triple!")
    end

    function __tostring(self)
        return `Point(x={self.x}, y={self.y})`
    end

end

expectpass("basic printing", function ()
    assert(typeof(Point) == "classobject")
    assert(typeof(Point.zero) == "function")
    -- For now we can pass tables to instance methods that expect objects
    assert(Point.magnitude({x = 4, y = 3}) == 5)
end)

expectfail("classobject missing key", "this classobject does not have a key named 'doesnotexist'", function ()
    return Point.doesnotexist
end)

expectfail("classobject invalid key", "cannot index classobject with a table", function ()
    return Point[{}]
end)

expectfail("classobject set method", "attempt to index classobject with 'zero'", function ()
    Point.zero = function () end
end)

expectpass("classobject construction", function ()
    local p = Point({ x = 1, y = 2 })
    assert(p.x == 1 and p.y == 2)
end)

expectfail("classobject bad construction", "attempt to index number with 'x'", function ()
    local _ = Point(42)
end)

expectpass("classobject no construction arg", function ()
    local p = Point()
    assert(not p.x and not p.y)
end)

expectpass("classinstance basics", function()
    local p = Point({ x = 3, y = 4 })
    assert(p.x == 3 and p.y == 4)
    assert(p:magnitude() == 5)
    local pzero = Point.zero()
    assert(pzero.x == 0 and pzero.y == 0)
    assert(pzero:magnitude() == 0)
    local success, res = pcall(Point.magnitude, p)
    assert(success and res == 5)
    local success, res = pcall(Point.magnitude, "wtf")
    assert(not success)
end)

expectpass("classinstance mutable", function ()
    local p = Point()
    p.x = 84
    p.y = 13
    assert(p:magnitude() == 85)
end)

expectpass("classinstance pcall method", function()
    local p = Point({ x = 1, y = 2})
    local success, msg = pcall(Point.asserttriple, p)
    assert(not success, "Expected pcall to fail")
    assert(msg:match("Not a pythagorean triple!"), `Message was: {msg}`)
end)

expectfail("classinstance assign nonexistent prop", "this classinstance does not have a key named 'huh'", function()
    local p = Point({ x = 3, y = 4 })
    p.huh = "???"
end)

expectfail("classinstance get nonexistent prop", "this classinstance does not have a key named 'huh'", function()
    local p = Point {}
    print(p.huh)
end)

expectfail("classinstance assign method", "attempt to index classinstance with 'zero'", function()
    local p = Point({ x = 3, y = 4 })
    p.zero = "???"
end)

expectfail("classinstance call missing method", "this classinstance does not have a key named 'hmm'", function()
    local p = Point {}
    print(p:hmm())
end)

expectpass("classinstance dynamic lookup", function ()
    local p = Point { x = 42, y = 13 }
    local function getfield(s)
        return p[s]
    end
    assert(getfield("x") == 42)
end)

expectpass("classinstance operator overloads", function ()
    local p1 = Point { x = 1, y = 4 }
    local p2 = Point { x = 2, y = 5 }
    local p3 = p1 + p2
    assert(p3.x == 3 and p3.y == 9)
    local p4 = p1 * p2
    assert(p4.x == 2 and p4.y == 20)
end)

expectpass("classinstance partially constructed class", function ()
    local p = Point { y = 99 }
    assert(not p.x and p.y == 99)
    p.x = 20
    assert(p:magnitude() == 101)
end)

class Secret

    function make(s)
        return Secret { cb = function() return s end }
    end

    function __concat(lhs, rhs)
        return Secret {
            cb = function ()
                return lhs:render() .. rhs:render()
            end
        }
    end

    public cb

    function render(self)
        return self.cb()
    end
end

expectpass("classinstance more operator overloads", function()
    local s1 = Secret.make("I am the ")
    local s2 = Secret.make("modren man!")
    local s3 = s1 .. s2
    assert(s3:render() == "I am the modren man!")
end)

expectpass("classobject first class function", function ()
    local function makeone(C, args)
        return C(args)
    end

    local p = makeone(Point, {x = 10, y = 2})
    assert(typeof(p) == "classinstance")
    assert(p.x == 10 and p.y == 2)
end)

local count = 0

class Counter
    public count
    function make()
        count += 1
        return Counter { count = count }
    end
end

expectpass("classinstance upvalues", function()
    local c1 = Counter.make()
    assert(c1.count == 1)
    local c2 = Counter.make()
    assert(c2.count == 2 and c1.count == 1)
end)

class Box
    public item
end

expectpass("classes have referential equality", function()
    local b1 = Box { item = "gold coin" }
    local b2 = Box { item = "gold coin" }
    assert(b1 == b1)
    assert(b1 ~= b2)
    assert(Box == Box)
    assert(Point ~= Box)
end)

expectfail("classes do not support <", "attempt to compare classobject < classobject", function()
    local _ = Box < Point
end)

expectfail("classes do not support <=", "attempt to compare classobject <= classobject", function()
    local _ = Box <= Point
end)

expectpass("classes use __eq", function ()
    local p1 = Point { x = 1, y = 2 }
    local p2 = Point { x = 1, y = 2 }
    assert(p1 == p2)
    assert(p1 ~= Point.zero())
end)

class Entry
    public tier: string
    public ordering: number

    function __lt(self, other)
        if self.tier ~= other.tier then
            if self.tier == "S" then
                return true
            elseif other.tier == "S" then
                return false
            else
                -- Assume it's a letter tier.
                return self.tier < other.tier
            end
        end
        return self.ordering < other.ordering
    end

end

expectpass("classes can be sorted", function()
    local e1 = Entry { tier = "A", ordering = 2 }
    local e2 = Entry { tier = "S", ordering = 1 }
    local e3 = Entry { tier = "A", ordering = 1 }
    local e4 = Entry { tier = "B", ordering = 1 }
    local entries = { e1, e2, e3, e4 }
    table.sort(entries)
    assert(entries[1] == e2)
    assert(entries[2] == e3)
    assert(entries[3] == e1)
    assert(entries[4] == e4)
end)

@native
local function istoptier(entry)
    return entry.tier == "S" or entry.tier == "A"
end

@native
local function makeentry(tier, ordering)
    return Entry { tier = tier, ordering = ordering }
end

expectpass("classes gracefully handle NCG", function()
    local hightier = Entry { tier = "A", ordering = 3 }
    local midtier =  Entry { tier = "C", ordering = 1 }
    local lowtier =  Entry { tier = "D", ordering = 2 }
    assert(istoptier(hightier))
    assert(not istoptier(midtier))
    assert(not istoptier(lowtier))
    local fromncg = makeentry("S", 4)
    assert(fromncg.tier == "S" and fromncg.ordering == 4)
end)

class PropertyWithMeta
    public __add
end

expectfail("class metamethods are methods", "attempt to perform arithmetic", function ()
    local pwm = PropertyWithMeta { __add = function (...) return 42 end }
    local _ = pwm + pwm
end)

expectfail("classes cannot be iterated over", "attempt to iterate over a classinstance value", function ()
    local p = Point { x = 1, y = 2}
    for k, v in p do
        -- This should be unreachable!
        print(k, v)
    end
end)

class Set
    function __iter(self)
        return next, {1, 2, 3, 4}
    end
end

expectpass("classes with __iter can be iterated over", function ()
    local s = Set {}
    for k, v in s do
        assert(typeof(k) == "number" and typeof(v) == "number")
    end
end)

class Test
    public a
    public b
    public c
end

expectpass("instance survives GC during __index construction", function()
    local poison = setmetatable({}, {
        __index = function(_, key)
            collectgarbage()

            if key == "a" then return 1 end
            if key == "b" then return 2 end
            if key == "c" then return 3 end
            return nil
        end
    })

    local v = Test(poison)
    assert(v.a == 1, `expected a=1, got a={v.a}`)
    assert(v.b == 2, `expected b=2, got b={v.b}`)
    assert(v.c == 3, `expected c=3, got c={v.c}`)
end)

return 'OK'