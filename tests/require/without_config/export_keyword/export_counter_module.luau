export local counter = 5
counter = 0

export function set(v)
    counter = v
end

export function add(x)
    counter += x
end

export function get()
    return counter
end
