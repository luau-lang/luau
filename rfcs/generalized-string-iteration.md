# Generalized String Iteration

## Summary

Introduce support for iterating over strings without using `string.split`/`gmatch`/`sub` by defining a default `__iter` implementation for strings.

## Motivation

Luau recently introduced an `__iter` metamethod to create a generic iteration fashion for arrays, dictionaries, and custom objects. However, strings were overlooked during the initial implementation of this feature and do not support generalized iteration like other objects.

This proposal allows strings to be supported in generalized iteration for performance enhancement, consistent `__iter` behavior, and easier code for iteration through strings.

## Design

Because `__iter` already exists, implementing this behavior should be very easy. The most simple and proper approach would be to add an `__iter` metamethod to the string metatable that allows iterating through strings as if they are a character array. The current string metatable only consists of an `__index` field that points to the `string` library, and no `__iter` method is defined.

### Behavior

As stated before, the iterator will treat strings as if they are a character array:

```lua
for position: number, character: string in "Luau" do
	print(position, character)
end
```

Output:

```
1	L
2	u
3	a
4	u
```

## Drawbacks

This implementation should have no known drawbacks and checks all the boxes. The behavior follows similar generalized-iteration behavior to other objects. Additionally, many other languages support direct iteration through strings (C#, etc.), so it will be recognizable by those transferring from other languages to Luau. Finally, `__iter` already exists and there is no current `__iter` method for strings.

## Alternatives

This behavior can be implemented in Lua, but there are multiple problems with this:
1. Code that emulates this behavior is *much* slower than a native C iterator would be.
2. Code that emulates this behavior is more complicated to write and harder for beginners.

### Lua Implementations

#### 1. string.sub Iteration

```lua
local myString = "Luau"
for position = 1, #myString do
	local character = string.sub(myString, position, position)
	....
end
```

#### 2. string.gmatch Iteration

```lua
local position = 1
for character in string.gmatch("Luau", ".") do
	....
	position += 1
end
```

#### 3. string.split Iteration

```lua
for position, character in string.split("Luau", "") do 
	....
end
```

### Results

```
char[]        	0.6009999999998854
string.sub    	5.2380000000000560
string.gmatch 	8.6480000000001380
string.split  	12.916999999999916
```

In a benchmark of 1,000,000 iterations of the previously listed alternatives versus an iteration over an array of the same characters, the generalized iteration outperformed the Lua implementations in every case. Not only do the alternatives require more potentially unfamiliar syntax & functions, but they also have significantly lower performance. While this benchmark isn\'t perfect, it is still reasonable to assume that a dedicated and optimized C `__iter` method for strings will outperform all alternatives.
