# Constant variables

## Summary

Add a new style of variable declaration (`const foo = XYZ`) to mark a variable as a constant. The `const` keyword would be context-sensitive to preserve backwards compatibility. Constant variables would be scoped similarly to local variables and would be read-only, meaning they could not be written to. All values could be set as constants, though tables would be frozen as protection against tampering.

## Motivation

Constants are present in almost every major programming language, and are useful for a variety of reasons, ranging from compiler optimizations to simple ergonomics. Often, values like error messages or magic numbers don't make sense to inline because they're used repeatedly, so they're put into variables. These variables are typically left unmodified because changing them would have far-reaching consequences, but there is no guarantee that they aren't modified. It's desirable to have a way to ensure this (by convention in Luau this is done by naming constants in 'screaming case' — LIKE_THIS), but Luau currently lacks an official one.

Ergonomics wise, it makes sense to have a constant keyword: people use constants anyway, and having a way to indicate a variable as one is a good thing. This extends to tooling, which can make less assumptions about what is and isn't a constant based on the use of the keyword.

## Design

The syntax is very straight forward so it will be brief. Constants would be declared using `const foo` where `foo` is any identifier and `const` would be a context sensitive keyword (similar to `continue`) that was only a keyword when followed by an identifier. It would be a drop-in replacement for `local foo`. So, the following code would all be valid:

```lua
local normal = "fine" -- Standard assignments would still be fine, obviously
const constant = "also fine!" -- And constants could be declared like this
local const = "context-sensitive means no problem :-)" -- `const` could still be used as a variable name with no problem
const const = "wait no" -- ...And regrettably this means that this would be fine as well, since constants follow the same naming rules as other variables
```

---

Constants would be scoped in the same way as a normal local variable:
```lua
const example_one = "example 1"
do
    const example_two = "example 2"
    print(example_one) -- "example 1"
    print(example_two) -- "example 2"
end
print(example_one) -- "example 1"
print(example_two) -- nil
```

Trying to write to a constant would be a runtime error:
```lua
const foo = 1
foo = 5 -- error here
```

Shadowing a constant would not be supported because it goes against the idea of a constant and variable shadowing is widely considered to be a mistake when it occurs (Luau has a lint against doing this). This is inconsistent with how normal variables function, but mistakes should not be repeated just for the sake of consistency.

---

There would be no type restrictions for what could be set as a constant. Given that tables and userdata **must** be allowed as constants for them to be useful, there isn't much of a point in restricting what types can be set as a constant. Userdata must be allowed because almost every useful datatype in Roblox is a userdata under the hood, and it should be possible to set them as a constant. Tables must be allowed for a very similar reason, as they're a basic part of Luau and a significant amount of user code uses them for custom datatypes.

Tables should be frozen when they're made constants to try to prevent tampering from other code. This isn't a security concern in of itself since any tables exposed to third parties are already open to being frozen, and people shouldn't be overwriting their own constants to begin with.

Userdata will by their nature be have to left mutable since there's no good way to freeze them (this is also undesirable behavior with far-reaching consequences). In Roblox, userdata is generally immutable anyway (with notable exceptions like Instances) so this is considered acceptable. In the future, it may be possible to lint against writing to userdata, but it will never be possible to prevent it during runtime without being invasive.

Coroutines are problematic under this proposal because they are stateful, and there is currently no suggested remedy for this issue. However, given coroutines are already a fairly advanced feature of Luau, there's probably no significant usability concern to allowing them to be set as a constant.

## Drawbacks

Constants complicate garbage collection because they are read-only once defined. This prevents things like tables and userdata from being garbage collected properly once declared as constants, which means it is on the user to not box themselves into a corner by defining unnecessary constants. This is mitigated by constants being scoped, but for long-living constants, especially for strings, tables, and userdata, it may be a problem.

This would introduce a new context-sensitive keyword, which complicates parsing and hurts readability in cases like `const const = foo` or `local const = foo`.

The semantics of the proposed implementation may be confusing to people. Constants should be actual constants, but this syntax has to not be so strict as to be unusable in environments like Roblox. This has lead to a design like the one proposed, where constants aren't necessarily constants and may in fact be mutable in the case of userdata or may have a state like coroutines.

## Alternatives

Python does not have constants and relies upon convention to indicate what is and isn't a constant. This is what Luau currently does. This has the drawback of making users trust that a 'constant' isn't ever changed, which isn't always easy to verify. It is however very practical provided every user obeys the suggestion of not modifying constants.

JavaScript *does* have constants that are declared in a similar manner (`const foo = bar;`) but have a caveat: constants are not immutable, they are effectively just read-only variables. This is basically worse than nothing and not worth the parsing and runtime cost since it comes with almost none of the benefits and all of the downsides. Most interpreted languages that have constants are either this or worse (Ruby, as an example, doesn't even make the variable readonly)

Constants could be limited to primitives and perhaps tables, guaranteeing that they are real constants and potentially allowing for compiler optimizations that aren't possible with arbitrary data type support. The usability limitation prevented this from being seriously considered, as not supporting userdata is considered to be a non-starter.

Freezing userdata was also — briefly — considered but the consequences of this are too severe, especially for environments like Roblox.

Suggesting a `__const` metamethod that triggered when a value was set as a constant was also considered, since this would allow userdata to be frozen correctly and be forward compatible. Ultimately however, it was decided against on merit because of its potential impact on performance (a branch in every userdata's implementation?).
