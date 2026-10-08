class Point
    public x: number
    public y

    function length(self)
        return math.sqrt(self.x * self.x + self.y * self.y)
    end

    function __add(self, other: Point)
        return Point { x = self.x + other.x, y = self.y + other.y }
    end

    function __tostring(self)
        return `Point \{ x = {self.x}, y = {self.y} \}`
    end

    function new(x, y)
        return Point { x = x, y = y }
    end
end

local p = Point.new(3, 4)
print(`Check out my cool point: {p}  length = {p:length()}`)

local p1 = Point { x = 1, y = 2 }
local sum = p + p1
print(`p + p1 = {sum}`)