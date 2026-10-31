-- A compound assignment writes its target, so the result of the operation is checked against the
-- variable's binding type rather than against a refinement that happens to be in scope here.
-- `a ..= x` has to be accepted exactly like the expanded `a = a .. x`.
local a: string = ``
if a ~= `` then
    a ..= ``
    a = a .. ``
end

-- The same applies to the arithmetic compound assignments, whose result widens past a singleton
-- refinement such as `n == 1`.
local n: number = 1
if n == 1 then
    n += 1
    n -= 1
    n *= 2
    n /= 2
    n //= 2
    n %= 2
    n ^= 2
end

-- Equality against a literal refines the target just the same.
local s: string = ""
if s == "" then
    s ..= ""
end

