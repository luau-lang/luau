# Arrow functions

## Summary
Reduce boilerplate and improve readability by introducing an arrow function syntax

## Motivation
Luau is a very flexible language where functions are often used as values. Many libraries use that extensively:
- [roblox-lua-promise](https://github.com/evaera/roblox-lua-promise)
- [Fusion](https://github.com/dphfox/Fusion)

Unfortunately, heavy usage of functions as values leads to a lot of unnecessary boilerplate:
```lua
Promise.new(function(resolve)
		somethingThatYields()
		resolve(1)
	end)
	:andThen(function(x)
		return Promise.new(function(resolve)
			somethingThatYields()
			resolve(x + 1)
		end)
	end)
	:andThen(print) --> 2
```

## Design
This RFC proposes the addition of the arrow function syntax, which exists in many languages such as [javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions), [C#](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/lambda-operator), etc.

The syntax goes as follows:
```lua
(...) => expression

(...) =>
	statements
end
```

The only difference is the syntax. Under the hood these still evaluate to regular functions:
```lua
function(...)
	return expression
end
```

For single expression arrow functions, you don't need the `end` keyword. They also return the expression:

```lua
local myArrowFunc = (n) => n * 2

print(myArrowFunc(3)) -- 6
```

Adding multiple expressions or statements should cause an error. When you want your arrow function to do more, you can insert the `end` keyword. This effectively makes it so that the body of the arrow function is treated as a regular function:

```lua
local myOtherArrowFunc = (n) =>
	n *= 2
	n -= 1
	return n
end

print(myOtherArrowFunc(5)) -- 9
```

### Preview of the arrow function in real use cases
Promises
```lua
Promise.new((resolve) =>
		somethingThatYields()
		resolve(1)
	end)
	:andThen((x) => Promise.new((resolve) =>
			somethingThatYields()
			resolve(x + 1)
		end)
	end)
	:andThen(print) --> 2
```

Fusion
```lua
local text = Value("hello")
local upperText = Computed((use) => use(text):upper())

New "TextButton" {
	Text = upperText,

	[OnEvent "Activated"] = () =>
		print(":)")
		print(peek(upperText))
	end
}
```

## Drawbacks
I'm not aware of any drawbacks other than slightly increasing the complexity of the language. The current proposed syntax is invalid, so it should be fully backwards compatible.

## Alternatives
Instead of `=>`, `->` could be used. Personally I'm not a big fan of that because `->` overlaps with the function type syntax. Arrow functions could potentially also work without the arrow, but I reckon it would be a nightmare to parse.
