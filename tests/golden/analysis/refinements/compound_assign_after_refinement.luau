-- A compound assignment writes its target, so the result of the operation is checked against the
-- variable's binding type rather than against a refinement that happens to be in scope here.
-- `a ..= x` has to be accepted exactly like the expanded `a = a .. x`.
local a: string = ``
if a ~= `` then
    a ..= ``
    a = a .. ``
end
