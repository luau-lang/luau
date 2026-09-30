-- nested table, where the binding is immutable but contents mutable
export const triangle = {}

triangle.name = "triangle"
triangle.sides = 3

function triangle.draw()
    return "drawing " .. triangle.name
end
