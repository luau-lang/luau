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
        return Point.new({ x = self.x * other.x, y = self.y * other.y })
    end

    function __add(self, other)
        return Point.new({ x = self.x + other.x, y = self.y + other.y })
    end

    function __eq(self, other)
        return self.x == other.x and self.y == other.y
    end

    function zero()
        return Point.new({ x = 0, y = 0 })
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
    local p1 = Point.new({ x = 1, y = 2 })
    local p2 = Point.new({ x = 0, y = 0 })
    assert(class.isinstance(p1, Point))
    assert(class.isinstance(p2, Point))
end)

expectpass("class_first_arg_is_not_instance", function()
    local notAPoint = NotAPoint.new({ x = 1})
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
    class.isinstance(Point.new({x = 1, y = 2}), "not a class")
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
    local p = Point.new({ x = 1, y = 2 })
    local c = class.classof(p)
    assert(c == Point)
    assert(typeof(c) == "class")
end)

expectpass("classof result is usable as the second arg to isinstance", function()
    local p = Point.new({ x = 3, y = 4 })
    local notAPoint = NotAPoint.new({ x = 1 })
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
    local p = Point.new({ x = 1, y = 2 })
    assert(p.x == 1 and p.y == 2)
end)

expectfail("classobject bad construction", "attempt to index number with 'x'", function ()
    local _ = Point.new(42)
end)

expectfail("classobject no construction arg", "must be called with 2 arguments", function ()
    Point.new()
end)

expectpass("classinstance basics", function()
    local p = Point.new({ x = 3, y = 4 })
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
    local p = Point.new({x=0, y=0})
    p.x = 84
    p.y = 13
    assert(p:magnitude() == 85)
end)

expectpass("classinstance pcall method", function()
    local p = Point.new({ x = 1, y = 2})
    local success, msg = pcall(Point.asserttriple, p)
    assert(not success, "Expected pcall to fail")
    assert(msg:match("Not a pythagorean triple!"), `Message was: {msg}`)
end)

expectfail("classinstance assign nonexistent prop", "this object does not have a key named 'huh'", function()
    local p = Point.new({ x = 3, y = 4 })
    p.huh = "???"
end)

expectfail("classinstance get nonexistent prop", "this object does not have a key named 'huh'", function()
    local p = Point.new({})
    print(p.huh)
end)

expectfail("classinstance assign method", "attempt to index object with 'zero'", function()
    local p = Point.new({ x = 3, y = 4 })
    p.zero = "???"
end)

expectfail("classinstance call missing method", "this object does not have a key named 'hmm'", function()
    local p = Point.new({})
    print(p:hmm())
end)

expectpass("classinstance dynamic lookup", function ()
    local p = Point.new({ x = 42, y = 13 })
    local function getfield(s)
        return p[s]
    end
    assert(getfield("x") == 42)
end)

expectpass("classinstance operator overloads", function ()
    local p1 = Point.new({ x = 1, y = 4 })
    local p2 = Point.new({ x = 2, y = 5 })
    local p3 = p1 + p2
    assert(p3.x == 3 and p3.y == 9)
    local p4 = p1 * p2
    assert(p4.x == 2 and p4.y == 20)
end)

expectpass("classinstance partially constructed class", function ()
    local p = Point.new({ y = 99 })
    assert(not p.x and p.y == 99)
    p.x = 20
    assert(p:magnitude() == 101)
end)

class Secret

    function make(s)
        return Secret.new({ cb = function() return s end })
    end

    function __concat(lhs, rhs)
        return Secret.new({cb = function ()
                return lhs:render() .. rhs:render()
            end
        })
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
        return C.new(args)
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
        return Counter.new({ count = count })
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
    local b1 = Box.new({ item = "gold coin" })
    local b2 = Box.new({ item = "gold coin" })
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
    local p1 = Point.new({ x = 1, y = 2 })
    local p2 = Point.new({ x = 1, y = 2 })
    assert(p1 == p2)
    assert(p1 ~= Point.zero())
end)

expectpass("classinstance __tostring", function ()
    local p = Point.new({ x = 3, y = 4 })
    assert(tostring(p) == "Point(x=3, y=4)")
    assert(`{p}` == "Point(x=3, y=4)")
end)

expectpass("classinstance __tostring reflects properties", function ()
    local p = Point.new({ x = 10, y = 20 })
    p.x = 99
    assert(tostring(p) == "Point(x=99, y=20)")
end)

expectpass("classinstance without __tostring uses default format", function ()
    local b = Box.new({ item = "sword" })
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
    tostring(BadToString.new({}))
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
    local e1 = Entry.new({ tier = "A", ordering = 2 })
    local e2 = Entry.new({ tier = "S", ordering = 1 })
    local e3 = Entry.new({ tier = "A", ordering = 1 })
    local e4 = Entry.new({ tier = "B", ordering = 1 })
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
    return Entry.new({ tier = tier, ordering = ordering })
end

expectpass("classes gracefully handle NCG", function()
    local hightier = Entry.new({ tier = "A", ordering = 3 })
    local midtier =  Entry.new({ tier = "C", ordering = 1 })
    local lowtier =  Entry.new({ tier = "D", ordering = 2 })
    assert(istoptier(hightier))
    assert(not istoptier(midtier))
    assert(not istoptier(lowtier))
    local fromncg = makeentry("S", 4)
    assert(fromncg.tier == "S" and fromncg.ordering == 4)
end)

expectfail("classes cannot be iterated over", "attempt to iterate over a object value", function ()
    local p = Point.new({ x = 1, y = 2 })
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
    local s = Set.new({})
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

    local v = Test.new(poison)
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
        local v = GCBarrierTest.new(poison)
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
    local inst = Cls.new({})
    assert(type(inst) == "object", `expected type(inst) == "object", got {type(inst)}`)
    assert(typeof(inst) == "object", `expected typeof(inst) == "object", got {typeof(inst)}`)
end)

expectpass("pcall constructors", function()
    local AlwaysRaises = setmetatable({}, {
        __index=function(self, name)
            error(`Cannot access {name}`)
        end
    })

    local success, res = pcall(Test.new, AlwaysRaises)
    assert(not success)
    assert("string" == typeof(res))

    local success, res = pcall(function(a) return Test.new(a) end, AlwaysRaises)
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

    local f = Foo.new(mt)
    assert(f.x == 100)
    assert(f.y == 200)
    assert(f.z == 300)
end)

expectpass("explicitly_invoke_default_constructor", function()
    local f = Foo.new {x=0, y=1, z=2}
    f:__init {x=3, y=2, z=1}

    assert(f.x == 3)
    assert(f.y == 2)
    assert(f.z == 1)
end)

expectpass("hoisting: read before declaration is nil", function()
    assert(typeof(Hoisted) == "nil")
end)

class Hoisted
    public x
end

expectpass("hoisting: read after declaration is class", function()
    assert(typeof(Hoisted) == "class")
    local h = Hoisted.new({ x = 1 })
    assert(h.x == 1)
end)

class Parent
    public children: {Child}

    function createChild(self)
        local child = Child.new({parent = self}) -- forward reference to Child
        table.insert(self.children, child)
        return child
    end
end

class Child
    public parent: Parent

    function createParent(self)
        local p = Parent.new({children = {self}})
        return p
    end
end

expectpass("hoisting: mutually referential classes work via capture by ref", function()
    local p = Parent.new({children = {}})
    local c = p:createChild()
    assert(class.isinstance(c, Child))
    assert(c.parent == p)
    assert(#p.children == 1)
    assert(p.children[1] == c)

    local p2 = c:createParent()
    assert(class.isinstance(p2, Parent))
end)

local function makeWidget(id)
    return Widget.new({ id = id }) -- forward reference to Widget
end

class Widget
    public id
end

expectpass("hoisting: function defined before class works via capture by ref", function()
    local w = makeWidget(42)
    assert(class.isinstance(w, Widget))
    assert(w.id == 42)
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
    local q = Quaternion.new(3, 4, 5, 6)
    assert(q.w == 3)
    assert(q.x == 4)
    assert(q.y == 5)
    assert(q.z == 6)
end)

expectpass("explicitly_invoke_constructor", function()
    local q = Quaternion.new(3, 4, 5, 6)
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
    local e = Empty.new()
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
    local p = Partial.new(10)
    assert(p.a == 10)
    assert(p.b == nil)
end)

expectfail("constructor_class_not_callable", "attempt to call a class value", function()
    Quaternion({})
end)

class ReturnsValue
    public x
    function __init(self)
        self.x = 1
        return 999
    end
end

expectpass("constructor_return_discarded", function()
    local r = ReturnsValue.new()
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
    Exploding.new()
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
    local v = Variadic.new(1, 2, 3, 5)
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
    local c = Cat.new({ species = "Felis catus", breed = "Siamese" })
    assert(c.species == "Felis catus")
    assert(c.breed == "Siamese")
    assert(c:live() == "I am alive")
    assert(tostring(c) == "I am an animal. I am a Siamese cat.")
end)

expectpass("class inheritance nil fields", function()
    local c = Cat.new({})
    assert(c.species == nil)
    assert(c.breed == nil)
    assert(c:live() == "I am alive")
    assert(tostring(c) == "I am an animal. I am a nil cat.")
end)

expectpass("class inheritance classof returns most specific class", function()
    local c = Cat.new({ species = "Felis catus", breed = "Siamese" })
    assert(class.classof(c) == Cat)
end)

class EmptyAnimal extends Animal
end

expectpass("class inheritance empty child", function()
    local ea = EmptyAnimal.new({ species = "Unknown" })
    assert(ea.species == "Unknown")
    assert(ea:live() == "I am alive")
    assert(tostring(ea) == "I am an animal.")
end)

expectpass("class inheritance isinstance with parent class", function()
    local c = Cat.new { species = "Felis catus", breed = "Siamese" }
    assert(class.isinstance(c, Cat))
    assert(class.isinstance(c, Animal))
end)

expectpass("class inheritance isinstance parent is not child", function()
    local a = Animal.new { species = "Canis lupus" }
    assert(class.isinstance(a, Animal))
    assert(not class.isinstance(a, Cat))
end)

expectpass("class inheritance static method accessible on child", function()
    assert(Cat.live ~= nil)
    assert(typeof(Cat.live) == "function")
    local c = Cat.new({ species = "Felis catus", breed = "Tabby" })
    assert(Cat.live(c) == "I am alive")
end)

class Kitten extends Cat
    public name: string

    function __tostring(self): string
        return `{Cat.__tostring(self)} My name is {self.name}.`
    end
end

expectpass("class inheritance multi-level (grandchild)", function()
    local k = Kitten.new({ species = "Felis catus", breed = "Persian", name = "Whiskers" })
    assert(k.species == "Felis catus")
    assert(k.breed == "Persian")
    assert(k.name == "Whiskers")
    assert(k:live() == "I am alive")
    assert(tostring(k) == "I am an animal. I am a Persian cat. My name is Whiskers.")
end)

class Dog extends Animal
end

expectpass("class inheritance multi-level isinstance", function()
    local k = Kitten.new { species = "Felis catus", breed = "Persian", name = "Whiskers" }
    assert(class.isinstance(k, Kitten))
    assert(class.isinstance(k, Cat))
    assert(class.isinstance(k, Animal))
    assert(not class.isinstance(k, Dog))
    local c = Cat.new({ species = "Felis catus", breed = "Tabby" })
    assert(not class.isinstance(c, Kitten))
end)

expectpass("class inheritance multi-level classof", function()
    local k = Kitten.new({ species = "Felis catus", breed = "Persian", name = "Whiskers" })
    assert(class.classof(k) == Kitten)
    assert(class.classof(k) ~= Cat)
    assert(class.classof(k) ~= Animal)
end)

open class Vehicle
    public speed: number

    function __add(self, other)
        return Vehicle.new({ speed = self.speed + other.speed })
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
    local c1 = Car.new({ speed = 60, doors = 4 })
    local c2 = Car.new({ speed = 40, doors = 2 })
    local combined = c1 + c2
    assert(combined.speed == 100)
    assert(c1 > c2)
    assert(c2 < c1)
end)

expectpass("class inheritance operator overload overridden", function()
    local t = Truck.new({ speed = 80, payload = 5000 })
    assert(tostring(t) == "Truck(speed=80, payload=5000)")
    -- __lt and __add should still be inherited from Vehicle
    local t2 = Truck.new({ speed = 60, payload = 3000 })
    assert(t2 < t)
    local combined = t + t2
    assert(combined.speed == 140)
end)

expectpass("class inheritance method override", function()
    local v = Vehicle.new({ speed = 50 })
    local c = Car.new({ speed = 70, doors = 4 })
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
    local rp = RegularPolygon.new({ sides = 6, length = 5 })
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
    local ic = IterChild.new({ tag = "test" })
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

expectpass("class inheritance __eq inherited", function()
    local a = EqChild.new({ val = 42, label = "a" })
    local b = EqChild.new({ val = 42, label = "b" })
    local c = EqChild.new({ val = 99, label = "a" })
    local d = EqParent.new({ val = 42 })
    assert(a == b)
    assert(b == a)
    assert(a ~= c)
    assert(c ~= a)
    assert(a == d)
    assert(b == a)
end)

expectpass("class inheritance GC stress", function()
    for i = 1, 100 do
        local c = Cat.new({ species = "species" .. tostring(i), breed = "breed" .. tostring(i) })
        assert(c:live() == "I am alive")
        if i % 10 == 0 then
            collectgarbage()
        end
    end
    collectgarbage()
    -- Verify objects created after GC still work
    local c = Cat.new({ species = "final", breed = "test" })
    assert(c.species == "final")
    assert(c:live() == "I am alive")
end)

expectpass("class inheritance GC stress multi-level", function()
    for i = 1, 50 do
        local k = Kitten.new({ species = "s" .. tostring(i), breed = "b" .. tostring(i), name = "n" .. tostring(i) })
        assert(k:live() == "I am alive")
        assert(k.name == "n" .. tostring(i))
    end
    collectgarbage()
    local k = Kitten.new({ species = "post-gc", breed = "test", name = "survivor" })
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
    ChildDoesntDefineAConstructor.new()
end)

expectfail("A class must define a constructor if its base class defines one.", "must define a constructor", function()
    GrandChildDoesntDefineAConstructorEither.new()
end)

class ChildDefinesAConstructor extends ParentDefinesAConstructor
    function __init(self)
    end
end

expectpass("A class must define a constructor if its base class defines one.", function()
    ChildDefinesAConstructor.new()
end)

class GrandChildDefinesAConstructor extends ChildDoesntDefineAConstructor
    function __init(self)
    end
end

expectpass("A class must define a constructor if any ancestor defines one.", function()
    GrandChildDefinesAConstructor.new()
end)

return 'OK'
