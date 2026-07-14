-- basic export of a function
export function add(a: number, b: number): number
    return a + b
end

export function greet(name: string): string
    return "Hello, " .. name
end

export function noop()
    -- do nothing
end
