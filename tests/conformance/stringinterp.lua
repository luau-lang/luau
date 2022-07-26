local function assertEq(left, right)
	assert(typeof(left) == "string")
	assert(typeof(right) == "string")

	if left ~= right then
		error(string.format("%q ~= %q", left, right))
	end
end

assertEq(`hello {"world"}`, "hello world")

assertEq(`2 + 2 = {2 + 2}`, "2 + 2 = 4")

-- assertEq(`{1} {2} {3} {4} {5} {6} {7}`, "1 2 3 4 5 6 7")

-- local combo = {5, 2, 8, 9}
-- assert(`The lock combinations are: {table.concat(combo, ", ")}` == "The lock combinations are: 5, 2, 8, 9")

-- assert(`true = {true}` == "true = true")

-- -- INTERP TODO: Syntax error
-- -- assert(string.find(`{{ "nested braces!" }}`, "table"))

-- local name = "Luau"
-- assert(`Welcome to {
-- 	name
-- }!` == "Welcome to Luau!")
-- assert(`Welcome to \
-- {name}!` == "Welcome to\nLuau!")

-- assert(`Escaped brace: \{} ({1})` == "Escaped brace: { (1)")
-- assert(`Backslash \ that escapes the space is not a part of the string... ({2})` == "Backslash  that escapes the space is not a part of the string... (2)")
-- assert(`Escaped backslash \\ ({3})` == "Escaped backslash \\ (3)")
-- assert(`Escaped backtick: \` ({4})` == "Escaped backtick: ` (4)")

-- assert(`Hello {`from inside {"a nested string"}`}` == "Hello from inside a nested string")

-- assert(`1 {`2 {`3 {4}`}`}` == "1 2 3 4")

-- local health = 50
-- assert(`You have {health}% health` == "You have 50% health")

-- INTERP TODO: Test with shadowing `string` (both as a string and not)

return "OK"
