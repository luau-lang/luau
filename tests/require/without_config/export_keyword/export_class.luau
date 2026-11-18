export class Point
    public x: number
    public y: number

    function getX(self): number
        return self.x
    end

    function getY(self): number
        return self.y
    end

    function setY(self, y: number): number
        self.y = y
    end

    function setX(self, x: number): number
        self.x = x
    end

    function dot(self, other: Point): number
        return self.x * other.x + self.y * other.y
    end
end

export class FactoryFactory
    function make()
        -- if exported classes are improperly handled, we try to grab the export table upvalue here
        -- however, these functions are at top level, so there is no upvalue to grab
        return FactoryFactory {}
    end
end
