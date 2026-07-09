--!nocheck

local function expectpass(s, f)
    f()
    print(`ok: {s}`)
end

local function expectfail(s, expected, f)
    local success, actual = pcall(f)
    assert(not success)
    assert(type(actual) == "string", `{s}: error message was not a string, but a {type(actual)}`)
    -- We have pass true for the last argument to string.find to disable pattern matching, since error messages
    -- can have characters in them that would be interpreted as patterns, and we want to match the error message exactly.
    -- e.g. parens in error messages can be interpreted as pattern groups
    assert(string.find(actual, expected, 1, true), `{s} expected:\n\t{expected}\nActual:\n\t{actual}`)
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

class NotAPoint
    public x
end

expectpass("class_first_arg_is_instance", function()
    local p1 = Point({ x = 1, y = 2})
    local p2 = Point({ x = 0, y = 0})
    assert(class.isinstance(p1, Point))
    assert(class.isinstance(p2, Point))
end)

expectpass("class_first_arg_is_not_instance", function()
    local notAPoint = NotAPoint({ x = 1})
    assert(not class.isinstance(notAPoint, Point))
end)

expectpass("class_first_arg_is_not_instance_and_isnt_class", function()
    assert(not class.isinstance(nil, Point))
    assert(not class.isinstance(false, Point))
    assert(not class.isinstance(true, Point))
    assert(not class.isinstance("hello", Point))
    assert(not class.isinstance(5, Point))
    assert(not class.isinstance({}, Point))
end)

expectfail("class_second_arg_is_not_classobject", "invalid argument #2 to 'isinstance' (class expected, got string)", function()
    class.isinstance(nil, "not a class")
end)

expectfail("class_second_arg_is_not_classobject", "invalid argument #2 to 'isinstance' (class expected, got string)", function()
    class.isinstance(Point({x = 1, y = 2}), "not a class")
end)

expectfail("classof raises on missing first argument", "missing argument #1", function()
    class.isinstance()
end)

expectfail("classof raises on missing second argument", "missing argument #2", function()
    class.isinstance(nil)
end)

expectpass("classof returns nil for non-classinstance values", function()
    assert(class.classof(nil) == nil)
    assert(class.classof(false) == nil)
    assert(class.classof(42) == nil)
    assert(class.classof("hello") == nil)
    assert(class.classof({}) == nil)
    -- A class object is not itself a class instance.
    assert(class.classof(Point) == nil)
end)

expectfail("classof raises on missing argument", "missing argument #1", function()
    class.classof()
end)

expectpass("classof returns the class object for a class instance", function()
    local p = Point { x = 1, y = 2 }
    local c = class.classof(p)
    assert(c == Point)
    assert(typeof(c) == "class")
end)

expectpass("classof result is usable as the second arg to isinstance", function()
    local p = Point { x = 3, y = 4 }
    local notAPoint = NotAPoint ({ x = 1 })
    -- classof(notAPoint) is NotAPoint, so a Point instance is not an instance of it.
    assert(not class.isinstance(p, class.classof(notAPoint)))
    -- classof(p) is Point, so p is an instance of it.
    assert(class.isinstance(p, class.classof(p)))
end)

expectpass("basic printing", function ()
    assert(typeof(Point) == "class")
    assert(typeof(Point.zero) == "function")
    -- For now we can pass tables to instance methods that expect objects
    assert(Point.magnitude({x = 4, y = 3}) == 5)
end)

expectfail("classobject missing key", "this class does not have a key named 'doesnotexist'", function ()
    return Point.doesnotexist
end)

expectfail("classobject invalid key", "cannot index class with a table", function ()
    return Point[{}]
end)

expectfail("classobject set method", "attempt to index class with 'zero'", function ()
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

expectfail("classinstance assign nonexistent prop", "this object does not have a key named 'huh'", function()
    local p = Point({ x = 3, y = 4 })
    p.huh = "???"
end)

expectfail("classinstance get nonexistent prop", "this object does not have a key named 'huh'", function()
    local p = Point {}
    print(p.huh)
end)

expectfail("classinstance assign method", "attempt to index object with 'zero'", function()
    local p = Point({ x = 3, y = 4 })
    p.zero = "???"
end)

expectfail("classinstance call missing method", "this object does not have a key named 'hmm'", function()
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

expectpass("class instantiate first class function", function ()
    local function makeone(C, args)
        return C(args)
    end

    local p = makeone(Point, {x = 10, y = 2})
    assert(typeof(p) == "object")
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

expectfail("classes do not support <", "attempt to compare class < class", function()
    local _ = Box < Point
end)

expectfail("classes do not support <=", "attempt to compare class <= class", function()
    local _ = Box <= Point
end)

expectpass("classes use __eq", function ()
    local p1 = Point { x = 1, y = 2 }
    local p2 = Point { x = 1, y = 2 }
    assert(p1 == p2)
    assert(p1 ~= Point.zero())
end)

expectpass("classinstance __tostring", function ()
    local p = Point { x = 3, y = 4 }
    assert(tostring(p) == "Point(x=3, y=4)")
    assert(`{p}` == "Point(x=3, y=4)")
end)

expectpass("classinstance __tostring reflects properties", function ()
    local p = Point { x = 10, y = 20 }
    p.x = 99
    assert(tostring(p) == "Point(x=99, y=20)")
end)

expectpass("classinstance without __tostring uses default format", function ()
    local b = Box { item = "sword" }
    local s = tostring(b)
    assert(string.match(s, "^object:") ~= nil, `expected default format, got: {s}`)
end)

expectpass("classobject tostring uses default format", function ()
    local s = tostring(Point)
    assert(string.match(s, "^class:") ~= nil, `expected default format, got: {s}`)
end)

class BadToString
    function __tostring(self)
        return {}
    end
end

expectfail("classinstance __tostring must return string", "'__tostring' must return a string", function ()
    tostring(BadToString {})
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

expectfail("classes cannot be iterated over", "attempt to iterate over a object value", function ()
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

class Cls end

expectpass("type reports 'class' for class objects", function()
    assert(type(Cls) == "class", `expected type(Cls) == "class", got {type(Cls)}`)
    assert(typeof(Cls) == "class", `expected typeof(Cls) == "class", got {typeof(Cls)}`)
end)

expectpass("type reports 'object' for class instances", function()
    local inst = Cls {}
    assert(type(inst) == "object", `expected type(inst) == "object", got {type(inst)}`)
    assert(typeof(inst) == "object", `expected typeof(inst) == "object", got {typeof(inst)}`)
end)

expectpass("pcall constructors", function()
    local AlwaysRaises = setmetatable({}, {
        __index=function(self, name)
            error(`Cannot access {name}`)
        end
    })

    local success, res = pcall(Test, AlwaysRaises)
    assert(not success)
    assert("string" == typeof(res))

    local success, res = pcall(function(a) return Test(a) end, AlwaysRaises)
    assert(not success)
    assert("string" == typeof(res))
end)

class Foo
    public x
    public y
    public z
end

expectpass("stack reallocation in creation", function()

    local function deepStack(n)
        local a01, a02, a03, a04, a05, a06, a07, a08, a09, a10 = 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
        local a11, a12, a13, a14, a15, a16, a17, a18, a19, a20 = 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
        if n > 0 then
            return deepStack(n - 1)+a01+a02+a03+a04+a05+a06+a07+a08+a09+a10+a11+a12+a13+a14+a15+a16+a17+a18+a19+a20
        end
        return 1
    end

    local mt = setmetatable({}, {
        __index = function(t, k)
            local trigger = deepStack(80)
            if k == "x" then return 100 + trigger - trigger end
            if k == "y" then return 200 end
            if k == "z" then return 300 end
        end
    })

    local f = Foo(mt)
    assert(f.x == 100)
    assert(f.y == 200)
    assert(f.z == 300)
end)

return 'OK'
