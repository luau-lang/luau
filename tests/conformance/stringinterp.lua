local function assertEq(left, right)
	assert(typeof(left) == "string")
	assert(typeof(right) == "string")

	if left ~= right then
		error(string.format("%q ~= %q", left, right))
	end
end

assertEq(`hello {"world"}`, "hello world")

assertEq(`2 + 2 = {2 + 2}`, "2 + 2 = 4")

assertEq(`{1} {2} {3} {4} {5} {6} {7}`, "1 2 3 4 5 6 7")

local combo = {5, 2, 8, 9}
assertEq(`The lock combinations are: {table.concat(combo, ", ")}`, "The lock combinations are: 5, 2, 8, 9")

assertEq(`true = {true}`, "true = true")

local name = "Luau"
assertEq(`Welcome to {
	name
}!`, "Welcome to Luau!")

local nameNotConstantEvaluated = (function() return "Luau" end)()
assertEq(`Welcome to {nameNotConstantEvaluated}!`, "Welcome to Luau!")

assertEq(`This {localName} does not exist`, "This nil does not exist")

assertEq(`Welcome to \
{name}!`, "Welcome to \nLuau!")

assertEq(`Escaped brace: \{} ({1})`, "Escaped brace: {} (1)")
assertEq(`Backslash \ that escapes the space is not a part of the string... ({2})`, "Backslash  that escapes the space is not a part of the string... (2)")
assertEq(`Escaped backslash \\ ({3})`, "Escaped backslash \\ (3)")
assertEq(`Escaped backtick: \` ({4})`, "Escaped backtick: ` (4)")

assertEq(`Hello {`from inside {"a nested string"}`}`, "Hello from inside a nested string")

assertEq(`1 {`2 {`3 {4}`}`}`, "1 2 3 4")

local health = 50
assert(`You have {health}% health` == "You have 50% health")

local function shadowsString(string)
	return `Value is {string}`
end

assertEq(shadowsString("hello"), "Value is hello")
assertEq(shadowsString(1), "Value is 1")

return "OK"
