# Named function type arguments

**Status**: Implemented

## Summary

Introduce syntax for optional names of function type arguments.

## Motivation

This feature will be useful to improve code documentation and provide additional information to LSP clients.

## Design

This proposal uses the same syntax that functions use to name the arguments: `(a: number, b: string) -> string`

Names can be provided in any place where function type is used, for example:

* in type aliases:
```
type MyFunc = (cost: number, name: string) -> string
```

* in definition files for table types:
```
declare string: {
    rep: (pattern: string, repeats: number) -> string,
    sub: (string, start: number, end: number?) -> string -- names are optional, here the first argument doesn't use a name
}
```

* for variables:
```
local cb: (amount: number) -> number
local function foo(cb: (name: string) -> ())
```

Variadic arguments cannot have a name, they are already written as ...: number.

This feature can be found in other languages:

* TypeScript (names are required): `let func: (p: type) => any`
* C++: `void (*f)(int cost, std::string name) = nullptr;`

Implementation will store the names inside the function type description.

Parsing the argument list will require a single-token lookahead that we already support.
Argument list parser will check if current token is an identifier and if the lookahead token is a colon, in which case it will consume both tokens.

Function type comparisons will ignore the argument names, this proposal doesn't change the semantics of the language and how typechecking is performed.

## Drawbacks

Argument names require that we create unique function types even when these types are 'identical', so we can't compare types using pointer identity.

This is already the case in current Luau implementation, but it might reduce the optimization opportunities in the future.

There might also be cases of pointer identity checks that are currently hidden and named arguments might expose places where correct unification is required in the type checker.
