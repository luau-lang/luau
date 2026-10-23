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

expectpass("access_with_brackets", function() 
    local p1 = Point { x = 1, y = 2 }
    assert(p1["x"] == 1)
    assert(p1["y"] == 2)
end)

class NotAPoint
    public x
end

expectpass("class_first_arg_is_instance", function()
    local p1 = Point { x = 1, y = 2 }
    local p2 = Point { x = 0, y = 0 }
    assert(class.isinstance(p1, Point))
    assert(class.isinstance(p2, Point))
end)

expectpass("class_first_arg_is_not_instance", function()
    local notAPoint = NotAPoint { x = 1}
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
    class.isinstance(Point {x = 1, y = 2}, "not a class")
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
    local notAPoint = NotAPoint { x = 1 }
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
    local p = Point { x = 1, y = 2 }
    assert(p.x == 1 and p.y == 2)
end)

expectfail("classobject bad construction", "attempt to index number with 'x'", function ()
    local _ = Point(42)
end)

expectfail("classobject no construction arg", "must be called with 2 arguments", function ()
    Point()
end)

expectpass("classinstance basics", function()
    local p = Point { x = 3, y = 4 }
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
    local p = Point {x=0, y=0}
    p.x = 84
    p.y = 13
    assert(p:magnitude() == 85)
end)

expectpass("classinstance pcall method", function()
    local p = Point { x = 1, y = 2}
    local success, msg = pcall(Point.asserttriple, p)
    assert(not success, "Expected pcall to fail")
    assert(msg:match("Not a pythagorean triple!"), `Message was: {msg}`)
end)

expectfail("classinstance assign nonexistent prop", "this object does not have a key named 'huh'", function()
    local p = Point { x = 3, y = 4 }
    p.huh = "???"
end)

expectfail("classinstance get nonexistent prop", "this object does not have a key named 'huh'", function()
    local p = Point {}
    print(p.huh)
end)

expectfail("classinstance assign method", "attempt to index object with 'zero'", function()
    local p = Point { x = 3, y = 4 }
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
        return Secret {cb = function ()
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
    local p = Point { x = 1, y = 2 }
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

class GCBarrierTest
    public a1 public b1 public c1 public d1 public e1
    public a2 public b2 public c2 public d2 public e2
    public a3 public b3 public c3 public d3 public e3
    public a4 public b4 public c4 public d4 public e4
    public a5 public b5 public c5 public d5 public e5
end

expectpass("instance properties survive GC during __index construction", function()
    local poison = setmetatable({}, {
        __index = function(_, key)
            return {key, 1, 2, 3}
        end
    })

    for iter = 1, 100 do
        local v = GCBarrierTest(poison)
        assert(type(v.a1) == "table" and v.a1[1] == "a1")
        assert(type(v.b2) == "table" and v.b2[1] == "b2")
        assert(type(v.c3) == "table" and v.c3[1] == "c3")
        assert(type(v.d4) == "table" and v.d4[1] == "d4")
        assert(type(v.e5) == "table" and v.e5[1] == "e5")
    end
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

expectpass("explicitly_invoke_default_constructor", function()
    local f = Foo {x=0, y=1, z=2}
    f:__init {x=3, y=2, z=1}

    assert(f.x == 3)
    assert(f.y == 2)
    assert(f.z == 1)
end)

class Quaternion
    public w: number
    public x: number
    public y: number
    public z: number

    function __init(self, w, x, y, z)
        self.w = w
        self.x = x
        self.y = y
        self.z = z
    end
end

expectpass("new_constructor", function()
    local q = Quaternion(3, 4, 5, 6)
    assert(q.w == 3)
    assert(q.x == 4)
    assert(q.y == 5)
    assert(q.z == 6)
end)

expectpass("explicitly_invoke_constructor", function()
    local q = Quaternion(3, 4, 5, 6)
    q:__init(0, 0, 0, 0)
    assert(q.w == 0)
    assert(q.x == 0)
    assert(q.y == 0)
end)

class Empty
    public x
    function __init(self)
        self.x = 42
    end
end

expectpass("constructor_no_args", function()
    local e = Empty()
    assert(e.x == 42)
end)

class Partial
    public a
    public b
    function __init(self, a)
        self.a = a
    end
end

expectpass("constructor_partial_init", function()
    local p = Partial(10)
    assert(p.a == 10)
    assert(p.b == nil)
end)

class ReturnsValue
    public x
    function __init(self)
        self.x = 1
        return 999
    end
end

expectpass("constructor_return_discarded", function()
    local r = ReturnsValue()
    assert(r.x == 1)
    assert(r:__init() == 999)
end)

class Exploding
    public x
    function __init(self)
        error("kaboom")
    end
end
expectfail("constructor_throws", "kaboom", function()
    Exploding()
end)

class Variadic
    public sum
    function __init(self, ...)
        local s = 0
        for _, v in {...} do s += v end
        self.sum = s
    end
end
expectpass("constructor_varargs", function()
    local v = Variadic(1, 2, 3, 5)
    assert(v.sum == 11)
end)

open class Animal
	public species: string

	function __tostring(self)
		return "I am an animal."
	end

	function live(self)
		return "I am alive"
	end
end

open class Cat extends Animal
	public breed: string

	function __tostring(self): string
		return `{Animal.__tostring(self)} I am a {self.breed} cat.`
	end
end

expectpass("class inheritance basic fields and methods", function()
    local c = Cat { species = "Felis catus", breed = "Siamese" }
    assert(c.species == "Felis catus")
    assert(c.breed == "Siamese")
    assert(c:live() == "I am alive")
    assert(tostring(c) == "I am an animal. I am a Siamese cat.")
end)

expectpass("class inheritance nil fields", function()
    local c = Cat {}
    assert(c.species == nil)
    assert(c.breed == nil)
    assert(c:live() == "I am alive")
    assert(tostring(c) == "I am an animal. I am a nil cat.")
end)

expectpass("class inheritance classof returns most specific class", function()
    local c = Cat { species = "Felis catus", breed = "Siamese" }
    assert(class.classof(c) == Cat)
end)

class EmptyAnimal extends Animal
end

expectpass("class inheritance empty child", function()
    local ea = EmptyAnimal { species = "Unknown" }
    assert(ea.species == "Unknown")
    assert(ea:live() == "I am alive")
    assert(tostring(ea) == "I am an animal.")
end)

expectpass("class inheritance isinstance with parent class", function()
    local c = Cat { species = "Felis catus", breed = "Siamese" }
    assert(class.isinstance(c, Cat))
    assert(class.isinstance(c, Animal))
end)

expectpass("class inheritance isinstance parent is not child", function()
    local a = Animal { species = "Canis lupus" }
    assert(class.isinstance(a, Animal))
    assert(not class.isinstance(a, Cat))
end)

expectpass("class inheritance static method accessible on child", function()
    assert(Cat.live ~= nil)
    assert(typeof(Cat.live) == "function")
    local c = Cat { species = "Felis catus", breed = "Tabby" }
    assert(Cat.live(c) == "I am alive")
end)

class Kitten extends Cat
    public name: string

    function __tostring(self): string
        return `{Cat.__tostring(self)} My name is {self.name}.`
    end
end

expectpass("class inheritance multi-level (grandchild)", function()
    local k = Kitten { species = "Felis catus", breed = "Persian", name = "Whiskers" }
    assert(k.species == "Felis catus")
    assert(k.breed == "Persian")
    assert(k.name == "Whiskers")
    assert(k:live() == "I am alive")
    assert(tostring(k) == "I am an animal. I am a Persian cat. My name is Whiskers.")
end)

class Dog extends Animal
end

expectpass("class inheritance multi-level isinstance", function()
    local k = Kitten { species = "Felis catus", breed = "Persian", name = "Whiskers" }
    assert(class.isinstance(k, Kitten))
    assert(class.isinstance(k, Cat))
    assert(class.isinstance(k, Animal))
    assert(not class.isinstance(k, Dog))
    local c = Cat { species = "Felis catus", breed = "Tabby" }
    assert(not class.isinstance(c, Kitten))
end)

expectpass("class inheritance multi-level classof", function()
    local k = Kitten { species = "Felis catus", breed = "Persian", name = "Whiskers" }
    assert(class.classof(k) == Kitten)
    assert(class.classof(k) ~= Cat)
    assert(class.classof(k) ~= Animal)
end)

open class Vehicle
    public speed: number

    function __add(self, other)
        return Vehicle { speed = self.speed + other.speed }
    end

    function __lt(self, other)
        return self.speed < other.speed
    end

    function __tostring(self)
        return `Vehicle(speed={self.speed})`
    end

    function describe(self)
        return "I am a vehicle."
    end
end

class Car extends Vehicle
    public doors: number

    function describe(self)
        return "I am a car."
    end
end

class Truck extends Vehicle
    public payload: number

    function __tostring(self)
        return `Truck(speed={self.speed}, payload={self.payload})`
    end
end

expectpass("class inheritance operator overload inherited", function()
    local c1 = Car { speed = 60, doors = 4 }
    local c2 = Car { speed = 40, doors = 2 }
    local combined = c1 + c2
    assert(combined.speed == 100)
    assert(c1 > c2)
    assert(c2 < c1)
end)

expectpass("class inheritance operator overload overridden", function()
    local t = Truck { speed = 80, payload = 5000 }
    assert(tostring(t) == "Truck(speed=80, payload=5000)")
    -- __lt and __add should still be inherited from Vehicle
    local t2 = Truck { speed = 60, payload = 3000 }
    assert(t2 < t)
    local combined = t + t2
    assert(combined.speed == 140)
end)

expectpass("class inheritance method override", function()
    local v = Vehicle { speed = 50 }
    local c = Car { speed = 70, doors = 4 }
    assert(v:describe() == "I am a vehicle.")
    assert(c:describe() == "I am a car.")
end)

open class Shape
    public sides: number

    function perimeter(self)
        return self.sides * self:sideLength()
    end

    function sideLength(self)
        return 1
    end
end

class RegularPolygon extends Shape
    public length: number

    function sideLength(self)
        return self.length
    end
end

expectpass("class inheritance virtual dispatch via self", function()
    local rp = RegularPolygon { sides = 6, length = 5 }
    -- perimeter() is inherited from Shape, but it calls self:sideLength()
    -- which should dispatch to RegularPolygon's override
    assert(rp:perimeter() == 30)
end)

open class IterParent
    function __iter(self)
        return next, {10, 20, 30}
    end
end

class IterChild extends IterParent
    public tag: string
end

expectpass("class inheritance __iter inherited", function()
    local ic = IterChild { tag = "test" }
    local sum = 0
    for _, v in ic do
        sum += v
    end
    assert(sum == 60)
    assert(ic.tag == "test")
end)

open class EqParent
    public val: number

    function __eq(self, other)
        return self.val == other.val
    end
end

class EqChild extends EqParent
    public label: string
end

class EqUnrelated
    public val: number

    function __eq(self, other)
        return self.val == other.val
    end
end

class EqUnrelatedWithMetamethod
    public val: number

    function __eq(self, other)
        return self.val == other.val
    end
end

expectpass("class inheritance __eq inherited", function()
    local a = EqChild { val = 42, label = "a" }
    local b = EqChild { val = 42, label = "b" }
    local c = EqChild { val = 99, label = "a" }
    local d = EqParent { val = 42 }
    local e = EqUnrelated { val = 42 }
    local f = EqUnrelatedWithMetamethod { val = 42 }
    assert(a == b)
    assert(b == a)
    assert(a ~= c)
    assert(c ~= a)
    assert(a == d)
    assert(b == a)

    -- We want to assert that b and d are not equal to any of e and f, and that
    -- e and f are themselves not equal.
    local function checkTwice(left, right)
        assert(left ~= right)
        assert(right ~= left)
    end
    checkTwice(d, e)
    checkTwice(b, e)
    checkTwice(d, f)
    checkTwice(b, f)
    checkTwice(e, f)
end)

expectpass("class inheritance GC stress", function()
    for i = 1, 100 do
        local c = Cat { species = "species" .. tostring(i), breed = "breed" .. tostring(i) }
        assert(c:live() == "I am alive")
        if i % 10 == 0 then
            collectgarbage()
        end
    end
    collectgarbage()
    -- Verify objects created after GC still work
    local c = Cat { species = "final", breed = "test" }
    assert(c.species == "final")
    assert(c:live() == "I am alive")
end)

expectpass("class inheritance GC stress multi-level", function()
    for i = 1, 50 do
        local k = Kitten { species = "s" .. tostring(i), breed = "b" .. tostring(i), name = "n" .. tostring(i) }
        assert(k:live() == "I am alive")
        assert(k.name == "n" .. tostring(i))
    end
    collectgarbage()
    local k = Kitten { species = "post-gc", breed = "test", name = "survivor" }
    assert(k.name == "survivor")
    assert(tostring(k) == "I am an animal. I am a test cat. My name is survivor.")
end)

open class ParentDefinesAConstructor
    function __init(self)
    end
end

open class ChildDoesntDefineAConstructor extends ParentDefinesAConstructor
end

class GrandChildDoesntDefineAConstructorEither extends ChildDoesntDefineAConstructor
end

expectfail("A class must define a constructor if its base class defines one.", "must define a constructor", function()
    ChildDoesntDefineAConstructor()
end)

expectfail("A class must define a constructor if its base class defines one.", "must define a constructor", function()
    GrandChildDoesntDefineAConstructorEither()
end)

class ChildDefinesAConstructor extends ParentDefinesAConstructor
    function __init(self)
    end
end

expectpass("A class must define a constructor if its base class defines one.", function()
    ChildDefinesAConstructor()
end)

class GrandChildDefinesAConstructor extends ChildDoesntDefineAConstructor
    function __init(self)
    end
end

expectpass("A class must define a constructor if any ancestor defines one.", function()
    GrandChildDefinesAConstructor()
end)

class PrivatePoint
    private _x: number
    private _y: number

    function __init(self, x, y)
        self._x = x
        self._y = y
    end

    function getX(self)
        return self._x
    end

    function getY(self)
        return self._y
    end

    function setX(self, x)
        self._x = x
    end
end

expectpass("private fields are readable and writable from methods of the declaring class", function()
    local p = PrivatePoint(3, 4)
    assert(p:getX() == 3)
    assert(p:getY() == 4)
    p:setX(10)
    assert(p:getX() == 10)
    assert(p:getY() == 4)
end)

class PartialPrivate
    private _a
    private _b

    function __init(self, a)
        self._a = a
    end

    function values(self)
        return self._a, self._b
    end
end

expectpass("private fields left uninitialized by the constructor are nil", function()
    local a, b = PartialPrivate(1):values()
    assert(a == 1)
    assert(b == nil)
end)

class Vec
    private _x
    private _y

    private function _magnitudeOf(x, y)
        return math.sqrt(x * x + y * y)
    end

    private function _scaled(self, k)
        return Vec(self._x * k, self._y * k)
    end

    function __init(self, x, y)
        self._x = x
        self._y = y
    end

    function coords(self)
        return self._x, self._y
    end
 
    function magnitude(self)
        return Vec._magnitudeOf(self._x, self._y)
    end

    function double(self)
        return self:_scaled(2)
    end
end

expectpass("private instance methods are callable from methods of the declaring class", function()
    local x, y = Vec(3, 4):double():coords()
    assert(x == 6 and y == 8)
end)

expectpass("private static methods are callable from methods of the declaring class", function()
    assert(Vec(3, 4):magnitude() == 5)
    assert(Vec(0, 0):magnitude() == 0)
end)

class StaticNamecall
    private _x

    -- Namecall syntax on the class object passes the class object itself as the first
    -- argument, since there is no instance involved.
    private function _receiver(receiver)
        return receiver
    end

    function __init(self, x)
        self._x = x
    end

    function staticReceiver(self)
        return StaticNamecall:_receiver()
    end
end

expectpass("private static methods are callable on the class object with namecall syntax", function()
    local s = StaticNamecall(1)
    local receiver = s:staticReceiver()
    assert(typeof(receiver) == "class")
    assert(receiver == StaticNamecall)
end)

open class Money
    private _cents: number

    function __init(self, cents)
        self._cents = cents
    end

    -- Private fields of any instance of the enclosing class are available, not just self's.
    function __eq(self, other)
        return self._cents == other._cents
    end

    function plus(self, other)
        return Money(self._cents + other._cents)
    end

    function cents(self)
        return self._cents
    end
end

expectpass("methods can read private fields of any instance of the declaring class", function()
    assert(Money(150):plus(Money(25)):cents() == 175)
end)

expectpass("__eq can read private fields of the other operand", function()
    assert(Money(100) == Money(100))
    assert(Money(100) ~= Money(101))
end)

class PrivateStringy
    private _name

    function __init(self, name)
        self._name = name
    end

    function __tostring(self)
        return `PrivateStringy({self._name})`
    end
end

expectpass("metamethods can read private fields", function()
    assert(tostring(PrivateStringy("hi")) == "PrivateStringy(hi)")
    assert(`{PrivateStringy("interp")}` == "PrivateStringy(interp)")
end)

expectfail("private fields are not readable outside the declaring class", "this object does not have a key named '_x'", function()
    local p = PrivatePoint(1, 2)
    return p._x
end)

expectfail("private fields are not writable outside the declaring class", "this object does not have a key named '_x'", function()
    local p = PrivatePoint(1, 2)
    p._x = 5
end)

expectfail("private instance methods are not callable outside the declaring class", "this object does not have a key named '_scaled'", function()
    return Vec(1, 2):_scaled(2)
end)

expectfail("private static methods are not accessible on the class object", "this class does not have a key named '_magnitudeOf'", function()
    return Vec._magnitudeOf(3, 4)
end)

class Impostor
    public x
    public y
end

expectfail("private field access on an instance of an unrelated class raises", "access private field", function()
    -- PrivatePoint.getX reads self._x, so it must reject a receiver that is not a PrivatePoint
    -- even though Impostor has similarly-named public fields.
    PrivatePoint.getX(Impostor { x = 1, y = 2 })
end)

expectfail("private field access on a table raises", "access private field", function()
    PrivatePoint.getX({ _x = 1, _y = 2 })
end)

expectfail("private field access on a number raises", "access private field", function()
    PrivatePoint.getX(42)
end)

expectfail("private field read on an unrelated class declaring the same private field raises", "access private field", function()
    PrivatePoint.getX(Vec(1, 2))
end)

expectfail("private field write on an unrelated class declaring the same private field raises", "assign private field", function()
    PrivatePoint.setX(Vec(1, 2), 5)
end)

open class HasPrivateAndConstructor
    private _secret

    function __init(self, secret)
        self._secret = secret
    end

    function secret(self)
        return self._secret
    end
end

class DerivesFromPrivateWithConstructor extends HasPrivateAndConstructor
    public label

    function __init(self, secret, label)
        HasPrivateAndConstructor.__init(self, secret)
        self.label = label
    end
end

expectpass("a derived class initializes base private fields by delegating to the base constructor", function()
    local d = DerivesFromPrivateWithConstructor("shh", "tag")
    assert(d:secret() == "shh")
    assert(d.label == "tag")
end)

open class ShadowBase
    private _v

    function __init(self, v)
        self._v = v
    end

    function baseV(self)
        return self._v
    end

    function setBaseV(self, v)
        self._v = v
    end
end

class ShadowDerived extends ShadowBase
    private _v

    function __init(self, baseVal, derivedVal)
        ShadowBase.__init(self, baseVal)
        self._v = derivedVal
    end

    function derivedV(self)
        return self._v
    end

    function setDerivedV(self, v)
        self._v = v
    end
end

expectpass("base and derived private fields with the same name do not collide", function()
    local d = ShadowDerived("base", "derived")
    assert(d:baseV() == "base")
    assert(d:derivedV() == "derived")
    d:setDerivedV("derived2")
    assert(d:baseV() == "base")
    assert(d:derivedV() == "derived2")
    d:setBaseV("base2")
    assert(d:baseV() == "base2")
    assert(d:derivedV() == "derived2")
end)

open class Vector1
    private _x: number

    function __init(self, x)
        self._x = x
    end

    function privateX(self)
        return self._x
    end

    -- Reads of other._x resolve to the private field of Vector1, never to a
    -- public field that a subclass happens to declare.
    function comparePrivateX(self, other)
        return self._x == other._x
    end
end

class Vector1WithPublicX extends Vector1
    public x: number

    function __init(self, privateX, publicX)
        Vector1.__init(self, privateX)
        self.x = publicX
    end
end

expectpass("a derived class's public field does not shadow the base class's private field", function()
    local base = Vector1(5)
    local derived = Vector1WithPublicX(5, 99)
    assert(derived.x == 99)
    assert(derived:privateX() == 5)
    -- Base methods still see the private _x of the derived instance, not its public x.
    assert(base:comparePrivateX(derived))
    assert(derived:comparePrivateX(base))
    assert(not Vector1(1):comparePrivateX(derived))
end)

open class PrivateGrandparent
    private _g

    function __init(self, g)
        self._g = g
    end

    function g(self)
        return self._g
    end
end

open class PrivateParent extends PrivateGrandparent
    private _p

    function __init(self, g, p)
        PrivateGrandparent.__init(self, g)
        self._p = p
    end

    function p(self)
        return self._p
    end
end

class PrivateChild extends PrivateParent
    private _c

    function __init(self, g, p, c)
        PrivateParent.__init(self, g, p)
        self._c = c
    end

    function c(self)
        return self._c
    end
end

expectpass("private fields are allocated for every class in the ancestry chain", function()
    local child = PrivateChild(1, 2, 3)
    assert(child:g() == 1)
    assert(child:p() == 2)
    assert(child:c() == 3)
    local parent = PrivateParent(10, 20)
    assert(parent:g() == 10)
    assert(parent:p() == 20)
end)

class TableUser
    private _data

    function __init(self)
        self._data = { _key = "value" }
    end

    -- Inside a method body, `.` on an underscore-prefixed name is private property
    -- access, so an underscore-prefixed table key must be reached via index notation.
    function readKey(self)
        return self._data["_key"]
    end

    function writeKey(self, v)
        self._data["_key"] = v
        return self._data["_key"]
    end
end

expectpass("underscore-prefixed table keys are reachable from method bodies via index notation", function()
    local t = TableUser()
    assert(t:readKey() == "value")
    assert(t:writeKey("replaced") == "replaced")
end)

class PrivateCallback
    private _cb
    public str: string

    function __init(self, str, prefix)
        self._cb = function(self, suffix)
            return prefix .. self.str .. suffix
        end
        self.str = str
    end

    function rebind(self, cb)
        self._cb = cb
    end

    function invoke(self, suffix)
        return self:_cb(suffix)
    end
end

expectpass("closures stored in private fields are callable from methods of the declaring class", function()
    local c = PrivateCallback("hello ", "prefix-")
    assert(c:invoke("world") == "prefix-hello world")
    c:rebind(function(self, suffix)
        return `prefix-{self.str}{suffix}`
    end)
    assert(c:invoke("world") == "prefix-hello world")
    collectgarbage()
    assert(c:invoke("again") == "prefix-hello again")
end)

expectfail("closures stored in private fields are not callable outside the declaring class", "this object does not have a key named '_cb'", function()
    local c = PrivateCallback("hello ", "prefix-")
    return c._cb("world")
end)

expectpass("classof and isinstance work on classes with private fields", function()
    local p = PrivatePoint(1, 2)
    assert(typeof(p) == "object")
    assert(class.classof(p) == PrivatePoint)
    assert(class.isinstance(p, PrivatePoint))
    assert(not class.isinstance(p, Vec))
    local d = ShadowDerived("base", "derived")
    assert(class.classof(d) == ShadowDerived)
    assert(class.isinstance(d, ShadowDerived))
    assert(class.isinstance(d, ShadowBase))
end)

class PrivateBox
    private _x

    public function __init(self, x)
        self._x = x
    end

    public function get(self)
        return self._x
    end

    public function set(self, val)
        self._x = val
    end
end

expectpass("classes with private fields have referential equality by default", function()
    local b1 = PrivateBox("same")
    local b2 = PrivateBox("same")
    assert(b1 == b1)
    assert(b1 ~= b2)
end)

expectpass("private fields of an inherited instance survive GC", function()
    local kept = {}
    for i = 1, 50 do
        kept[i] = PrivateChild({i}, {i + 1}, {i + 2})
        if i % 10 == 0 then
            collectgarbage()
        end
    end
    collectgarbage()
    for i = 1, 50 do
        assert(kept[i]:g()[1] == i)
        assert(kept[i]:p()[1] == i + 1)
        assert(kept[i]:c()[1] == i + 2)
    end
end)

-- 255 is the parser's maximum number of private fields per class, so this class sits
-- exactly on the boundary of what the private field storage must be able to represent.
open class ManyPrivate
    private _p1 private _p2 private _p3 private _p4 private _p5 private _p6 private _p7 private _p8 private _p9 private _p10 private _p11 private _p12 private _p13 private _p14 private _p15 private _p16
    private _p17 private _p18 private _p19 private _p20 private _p21 private _p22 private _p23 private _p24 private _p25 private _p26 private _p27 private _p28 private _p29 private _p30 private _p31 private _p32
    private _p33 private _p34 private _p35 private _p36 private _p37 private _p38 private _p39 private _p40 private _p41 private _p42 private _p43 private _p44 private _p45 private _p46 private _p47 private _p48
    private _p49 private _p50 private _p51 private _p52 private _p53 private _p54 private _p55 private _p56 private _p57 private _p58 private _p59 private _p60 private _p61 private _p62 private _p63 private _p64
    private _p65 private _p66 private _p67 private _p68 private _p69 private _p70 private _p71 private _p72 private _p73 private _p74 private _p75 private _p76 private _p77 private _p78 private _p79 private _p80
    private _p81 private _p82 private _p83 private _p84 private _p85 private _p86 private _p87 private _p88 private _p89 private _p90 private _p91 private _p92 private _p93 private _p94 private _p95 private _p96
    private _p97 private _p98 private _p99 private _p100 private _p101 private _p102 private _p103 private _p104 private _p105 private _p106 private _p107 private _p108 private _p109 private _p110 private _p111 private _p112
    private _p113 private _p114 private _p115 private _p116 private _p117 private _p118 private _p119 private _p120 private _p121 private _p122 private _p123 private _p124 private _p125 private _p126 private _p127 private _p128
    private _p129 private _p130 private _p131 private _p132 private _p133 private _p134 private _p135 private _p136 private _p137 private _p138 private _p139 private _p140 private _p141 private _p142 private _p143 private _p144
    private _p145 private _p146 private _p147 private _p148 private _p149 private _p150 private _p151 private _p152 private _p153 private _p154 private _p155 private _p156 private _p157 private _p158 private _p159 private _p160
    private _p161 private _p162 private _p163 private _p164 private _p165 private _p166 private _p167 private _p168 private _p169 private _p170 private _p171 private _p172 private _p173 private _p174 private _p175 private _p176
    private _p177 private _p178 private _p179 private _p180 private _p181 private _p182 private _p183 private _p184 private _p185 private _p186 private _p187 private _p188 private _p189 private _p190 private _p191 private _p192
    private _p193 private _p194 private _p195 private _p196 private _p197 private _p198 private _p199 private _p200 private _p201 private _p202 private _p203 private _p204 private _p205 private _p206 private _p207 private _p208
    private _p209 private _p210 private _p211 private _p212 private _p213 private _p214 private _p215 private _p216 private _p217 private _p218 private _p219 private _p220 private _p221 private _p222 private _p223 private _p224
    private _p225 private _p226 private _p227 private _p228 private _p229 private _p230 private _p231 private _p232 private _p233 private _p234 private _p235 private _p236 private _p237 private _p238 private _p239 private _p240
    private _p241 private _p242 private _p243 private _p244 private _p245 private _p246 private _p247 private _p248 private _p249 private _p250 private _p251 private _p252 private _p253 private _p254 private _p255

    function __init(self)
        self._p1 = 1
        self._p254 = 254
        self._p255 = 255
    end

    function first(self)
        return self._p1
    end

    function middle(self)
        return self._p128
    end

    function secondToLast(self)
        return self._p254
    end

    function last(self)
        return self._p255
    end
end

expectpass("a class can have the maximum number of private fields", function()
    local m = ManyPrivate()
    assert(m:first() == 1)
    assert(m:middle() == nil)
    assert(m:secondToLast() == 254)
    assert(m:last() == 255)
    collectgarbage()
    assert(m:last() == 255)
end)

class OneMorePrivate extends ManyPrivate
    private _myPrivate

    function __init(self)
        ManyPrivate.__init(self)
        self._myPrivate = 256
    end

    function getMyPrivate(self)
        return self._myPrivate
    end
end

expectpass("why we need more than 8 bits for number of private instance fields", function()
    local p = OneMorePrivate()
    assert(p:getMyPrivate() == 256)
end)

open class Base
    public a

    function __init(self, a) self.a = a end
end

class Child extends Base
    private _q

    private function _m1(self) return 1 end
    private function _m2(self) return 2 end
    private function _m3(self) return 3 end

    function __init(self)
        self.a = 0
        self._q = 5
    end

    function sum(self)
        return self:_m1() + self:_m2() + self:_m3() + self._q
    end
end

expectpass("Child class with private methods and fields", function()
    for i = 1, 100 do
        local c = Child()
        assert(c:sum() == 11)
    end
end)

open class PrivateWithoutConstructor
    private _x
    public y

    function getX(self)
        return self._x
    end
end

expectfail("a class with private fields must define a constructor", "must define a constructor because it has private fields", function()
    PrivateWithoutConstructor { y = 1 }
end)

class DerivesFromPrivateWithoutConstructor extends PrivateWithoutConstructor
    public z
end

expectfail("a class must define a constructor if a base class has private fields", "must define a constructor because it or a class it inherits from has private fields", function()
    DerivesFromPrivateWithoutConstructor { z = 1 }
end)

open class PublicOnlyBase
    public a
end

class PrivateDerivedFromPublicOnly extends PublicOnlyBase
    private _b
end

expectfail("a derived class with private fields must define a constructor even if its base class does not", "must define a constructor because it or a class it inherits from has private fields", function()
    PrivateDerivedFromPublicOnly { a = 1 }
end)

open class BaseOther
    private function _secret(cls) return "secret" end
    function create(cls) return cls:_secret() end
end

class DerivedOther extends BaseOther
end

expectpass("subclasses can access private static methods of their base class", function()
    BaseOther.create(BaseOther) -- class access to private static `_secret` is allowed for base class
    BaseOther.create(DerivedOther{}) -- object access to private static `_secret` is allowed for derived object
    BaseOther.create(DerivedOther) -- class access to private static `_secret` is allowed for derived class
end)

return 'OK'
