export local foo = function()
    return "original"
end

export function rebind()
    function foo()
        return "rebound"
    end
end

export function call_foo()
    return foo()
end
