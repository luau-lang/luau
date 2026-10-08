-- exported function referenced as upvalue from nested closure
export function outer()
    local function inner()
        return outer()
    end
    return 42
end

export function makeAdder(x)
    return function(y)
        return x + y
    end
end
