--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true

open class Point
    public x: number
    public y: number
end

class Point1 extends Point
    public x: string
    public y: number
end

open class PointChild extends Point end

class BadPointGrandchild extends PointChild
    public x: number
end