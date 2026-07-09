export local counter = 10
export local text = "hello"

export function setCounter(v)
    counter = v
end

export function addCounter(n)
    counter += n
end

export function concatText(s)
    text ..= s
end
