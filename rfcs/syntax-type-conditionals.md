# Conditional Types

## Summary

Conditional Types would be a way to procedurally evaluate a type based on a condition.

In a lot of programs, we have to make decisions based on input. Luau programs have that same versatility, but given the fact that values can be easily introspected, those decisions are also based on the types of the inputs. Conditional types help describe the relation between the types of inputs and outputs.

---

## Motivation

Whilst using Luau, I found myself trying to Exclude a type from a union once I had sanitized that my value wasn't of that type, in TypeScript this is simple;

```ts
type X = string | number;

type Exclude<U, X> = U extends X ? never : U;

let v: Exclude<X, string>;
```

In Luau, this is completely impossible right now, as we can't run conditions on types. This is a simple example, but it's a common use case for conditional types.

What this RFC proposes is a way to run ternary-ish conditionals on types, and return a type based on the result of that comparison.

```lua
type Vehicle = { horsepower: number };
type LandVehicle = Vehicle & { wheels: number };
type WaterVehicle = Vehicle & { propellers: number };
type MotorVehicle = LandVehicle & { engine: string };
type HumanPoweredVehicle<T> = T & { legs: number };

-- In FP+DOP, we'll commonly use functions like:
function getHorsepower(vehicle: Vehicle): number -- This is fine, we have no problems here.
    return vehicle.horsepower;
end

-- Now this is a bit more problematic, as we can't guarantee that the vehicle has wheels but we still want to know what drives it.
function getDevice<T>(vehicle: T): T is MotorVehicle ? string : T is HumanPoweredVehicle ? number : T is WaterVehicle ? number : never
    return vehicle.engine or vehicle.legs or vehicle.propellers;
end

-- This can be circumvented by just returning T["wheels"], but that's a bit of a hack.
function getWheels<T>(vehicle: T): T is LandVehicle ? number : never
    return vehicle.wheels;
end

-- Sometimes, we just don't want to have to deal with incorrect types; we **know** we have a MotorVehicle, so we don't want to have to deal with *possibly* getting a number:
local myVehicle: MotorVehicle = { horsepower = 100, wheels = 4, engine = "V8" };

getDevice(myVehicle) -- Without conditional types, this would return a (string | number), but we know it's a string, we just want Luau to also be able to know that.
```

### Other Benefits

The Luau language lacks in function overloading, and conditional types would allow us to get around that requirement. This is a very common use case in TypeScript, and it's something that I've found myself wanting in Luau repeatedly.

---

## Design

### Possibilities

Please note, the following type definition serves this statement:
```lua
-- Paginal navigation based on user input, has the possibility to have incorrect parameters, we want our types to reflect that;
-- getPage(userInput, userInput2) is **not** guaranteed to be safe, people should assert it before using it.
function getPage<PATH, PAGE>(path: PATH, page: PAGE): IsValidQuery<PATH, PAGE>
    if typeof(path) == "string" then
        if typeof(page) == "number" then
            return PageInstance;
        else
            return "Page is not a number";
        end
    else
        return "Path is not a string";
    end
end
```

#### `<CondStatement> is <CondStatement> ? <CondStatement> : <CondStatement>`

###### TypeScript-Inspired (`extends` → `is`)

This inherits TypeScript's ternary syntax, but with the `is` keyword instead of `extends`.

This probably makes the most sense. The main issue is reserving the `is` keyword, and the slight lack of fluidity with native ternaries (`if x then y else z`)

As an explanation of why we're suggesting `_ is _ ? _ : _` instead of the if-statement alternative, I'd like to take a look at the rest of Luau's system at the moment, of all reserved tokens in the types, none are keywords, all are symbols, `? _ : _` keeps closely in line with that, whilst `_ is _ ?` gives it the easy legibility that you'd expect from Luau code without producing overly long or exaggerated code. I've also found that it has higher potential for legibility.[^3]

[^3]: This is a personal opinion, and I'm open to discussion on this.
      
      The basis of it is that where `_ is _ ? _ : _` produces this code:
      ```lua
      type NestedCondition<A, B, C> =
        A is B
            ? B is C
                ? string
                : never
            : number;
      ```

      It's a lot easier to read than the if-statement alternative:
      ```lua
      type NestedCondition<A, B, C> =
          if A is B then
              if B is C then
                  string
              else
                  never
          else
              number
      ```

      Or even the headless if-statement alternative:
      ```lua
      type NestedCondition<A, B, C> =
            A is B then
                B is C
                    then string
                    else never
            else
                number
      ```

```lua
type IsValidQuery<PATH, PAGE> = 
    PATH is string
        ? PAGE is number 
            ? Page
            : string
        : string;
```

### Technical Consideration

#### Evaluation

Theoretically, anything is assignable to `never` and `unknown`, `never` is assignable to nothing. Dealing with `any` is the same, it should be assignable to anything in theory.

Considering the following:
```lua
type Helper<A, B> = A is B ? 1 : 0;
```

| A | B | X[^2] |
| - | - | - |
| `never` | `never` | `0` |
| `unknown` | `never` | `1` |
| `any` | `never` | `1` |
| `any` | `unknown` | `1` |
| `{}` | `unknown \| never` | `1` |
| `string` | `string` | `1` |
| `string` | `string` | `1` |
| `string \| number` | `string` |  `0 \| 1`[^1] |
| `{ string }` | `{ unknown }` | `1` |
| `"foo"` | `"bar"` | `0` |
| `"foo"` | `"foo"` | `1` |


[^1]: Evaluates to `0 | 1` as we apply the condition to each constituent of the union type.

      `string is string` -> true
      `number is string` -> false

      We union the results together, `true | false` || `0 | 1`.
[^2]: This can either follow this truth table or general type matching logic (for `x is y ? 1 : 0`), where `x` having at minimum **all** that `y` has will simply pass.

---

## Drawbacks

This would theoretically reserve the `is` keyword, which is currently unused and could be user-defined. If we try to circumvent this, we land on unfavorable usage of `::` where its used for condition instead of type ascription.

This isn't a whole load of complexity and will likely be easy to use and understand. (Well, as easy as type theory can be)

### Nullable Type Interference

Currently, we have `?` to represent a type that can be nil.

```lua
-- This is currently valid syntax in this proposal:

type convert<A> = A is string ? number ? : never
type convert<A> = A is string ? number? : never
type convert<A> = A is string?number?:never
type convert<A> = A is string??number?:never
```

This isn't a problem for linters as we can still perform valid lookahead, but it does make it a little more difficult to read. We **could** force ternary operators to be surrounded by spaces, so that only the following is valid;

```lua
type convert<A> = A is string ? number? : never
```

I'm going to have to oppose myself to this though, as it's not a great solution, if we do ignore this drawback we will simply ignore it

---

## Alternatives

### Generic Overloading

Generic overloading was proposed by @echnobas as a replacement to type conditionals. This would allow for the following:

```lua
type P<string> = number;
type P<number> = string; -- Same type, new return based on the generic
```

It would also allow us to use function overloading, but it would lack the necessary range in many cases.

This also opens the door to lots of inconsistencies, do we check if it's *exactly* that type or do we check if it is assignable to that type? This can also overlap with people's definitions for generics at the moment. We mulled this over and even with expanded syntax, it makes for a mess (it also isn't compatible with the Constrained Generics RFC).

### Match-Exclusive Type Conditionals

Instead of using `is`, we could use a `match`-like similar to Rust to represent the exclusive type conditional. This would allow for the following:

```lua
type convert<T> = match T is (
    string: number,
    number: string,
    never
)
```

Don't know about you, reader, but this doesn't really fit in Luau's syntax.

### Quick-Fire Considerations

**`if <Type> == <Type> then <Type> else <Type>`.**

`<Type> == <Type>` gives user the impression that they can use `==` and other logical operators to compare types, which is not the case.

Side-Note: We have also considered allowing other logical operators, but this does not make sense in the context of type conditionals. Specific `x is y ? foo : bar` is easier to implement and more powerful.

**`if <Type> :: <Type> then <Type> else <Type>`.**

`<Type> :: <Type>` takes away from the verbose syntax that Luau aims to achieve. (I kept `<Type> :: <Type> ? <Type> : <Type>` if we do settle on a symbols-based approach)

**`if <Type> is <Type> then <Type> else <Type>`.**

`if` is a bit too verbose in my opinion, and could be easily legibly be confused with `is`, there's no *real* point to do it as we're not handling blocks. I'll renounce the `if` keyword for this and present headless-if-statements if the original design is to be denied

**`<Type> := <Type> then <Type> else <Type>`.**

Pseudocode and some languages already reserve `:=` as a walrus operator for expression-based assignment or just for assignment, which is not what we're doing here.

**`<Type> :: <Type> ? <Type> : <Type>`**

The issue with this design is the removal of the logical english form of the statement; "**X is Y ?** Yep, **FOO**; Nope, **BAR**". This also gives us a bit of mixup potential between `::` and `:` used for type assignment.

**`<Type> :: <Type> ? <Type> : <Type>`**

The issue with this design is the removal of the logical english form of the statement; "**X is Y ?** Yep, **FOO**; Nope, **BAR**". This also gives us a bit of mixup potential between `::` and `:` used for type assignment.

**`if <Type>: <Type> then <Type> else <Type>`**

The issue with this design is that `<Type>: <Type>` looks like parameter type *assignment* rather than an actual check, this syntax doesn't really fit with the rest of Luau considering this in my opinion.
