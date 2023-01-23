# Constrained Generics

## Summary

Allows you to give a function generic a constrained type.

## Motivation

Some functions, such as the one below, allow you to feed it a table, and the table's keys are fed into the base object. With the current implementation of generics, this cannot be done and as such, the autocomplete information of the object is unavailable

```lua
type Enum<T> = {
	GetEnumItems: (Enum) -> {[string]: EnumItem}
} & T

local function Enum<T>(description: T): Enum<T>
	local enum = {}

	-- merge keys into the enum
	for _, enumItem in description do -- Cannot call non-function T
		table.insert(enum, enumItem)
	end
```

Without the generics, this would type would become

```lua
{GetEnumItems(Enum) -> {[string]: number}, items: {[string[: number}}
```

In reality, we'd prefer it if items and GetEnumItems return `T`, instead of explicitly declared dictionaries.

This constraining is mainly for concrete table types and functions, since types like `number`, `string`, and `boolean` are literal types.

## Design

The way to implement this is to have new syntax under the generic's brackets: `<T: typedef>`. This would constrain the type from the generic def and give the typechecker better hints.
```lua
local function Enum<T: {[string]: any}>(description: T): Enum<T>
	local enum = {}

	-- merge keys into the enum
	for _, enumItem in description do -- Typechecker now knows T is at least a string table, so this becomes somewhat valid
		table.insert(enum, enumItem)
	end
```

This will also help the typechecker from the outside looking into the function, for example:
```lua
Enum {None = 0, Some = 1, All = 2} --OK
Enum {"None", "Some", "All"} -- Not OK (generic must be have string keys)
Enum(0) -- Not ok (generic must be a table)
```

## Drawbacks

More of the complexity budget spent.

This adds unusual syntax, as well as adding to the complexity of the language.

This also doesn't address how this should be approached around type packs.

## Alternatives

Use something similar to Rust's `where` syntax:
```lua
local function Enum<T>(tab: T) where T: {[string]: any}
```

Do nothing, and use the `assert(type(T) == T)` syntax, though this creates extra bytecode. It also casts the object as `never`, which removes the type errors, however no autocomplete information is generated.
