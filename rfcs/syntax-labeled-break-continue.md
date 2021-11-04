# Labeled Break and Continue

## Summary

Labeled break and continue statements allow for specifying which loop is being broken out of or continued into in the case of multiple nested loops. 

## Motivation

Luau does not have a clean way to deal with exiting multiple nested loops, or advance an outermost loop while skipping execution in all the inside ones.
```Lua
while cond_1 do
    -- code
    while cond_2 do
        -- code
        break -- we can not break the 2nd loop from here
    end
end
```
This kind of operation is useful in a lot of places, although not very common. Another interesting application of it would be facilitating code generation with tooling that supports this behavior already.

## Design

The proposed implementation is to allow for vanilla Lua-like labels, with the `::name::` style, but only before loop statements. This is essentially a `goto` statement that can only go outwards and not to an arbitrary location. The behavior is not much different than WebAssembly's branching instructions, or Rust's `break 'label`/`continue 'label` syntax. A possible implementation might look like the following code:
```Lua
::my_loop:: while true do
    while true do
        break my_loop
    end
end
```

Though, this example is ambiguous on `expr :: type` syntax and for any already invalid statements that could come after `break`. A possible solution might be using another symbol, possibly a new unused one or recycling an operator.

## Drawbacks

This has the potential to introduce very tricky syntax ambiguities or may just not look right with the rest of the language. It may also increase complexity during compilation as jumps should now be able to reach other syntax levels, although would not be as chaotic as raw `goto`s.

## Alternatives

The current alternative to this is using flag variables to decide when a loop should do an action. This is really combersome though, and becomes exponentially harder to maintain as you add more loops.
```Lua
while true do
    local flag_should_break

    while true do
        flag_should_break = true
        break
    end

    if flag_should_break then
        break
    end
end
```
