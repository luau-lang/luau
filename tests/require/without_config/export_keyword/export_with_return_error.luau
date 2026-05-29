-- returning a custom value with value/function exports should error
export function distance(a, b)
    return math.sqrt((a.x - b.x)^2 + (a.y - b.y)^2)
end

return { distance = distance }
