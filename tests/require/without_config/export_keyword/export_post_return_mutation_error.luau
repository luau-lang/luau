-- exported locals should become readonly once the module returns
export local counter = 0

export function increment()
    counter += 1
end
