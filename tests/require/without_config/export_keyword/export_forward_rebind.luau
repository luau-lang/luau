export local recursive
recursive = function(n)
    if n <= 1 then
        return 1
    end
    return n * recursive(n - 1)
end

export local mut
function mut()
    return "function-stat"
end

export function rebindMut()
    mut = function()
        return "rebound"
    end
end
