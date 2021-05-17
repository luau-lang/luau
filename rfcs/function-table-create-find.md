# table.create and table.find

> Note: this RFC was adapted from an internal proposal that predates RFC process and as such doesn't follow the template precisely

**Status**: Implemented

## Design

This proposal suggests adding two new builtin table functions:

`table.create(count, value)`: Creates an array with count values, initialized to value. This can be useful to preallocate large tables - repeatedly appending an element to the table repeatedly reallocates it. count is converted to an integer using standard conversion/coercion rules (strings are converted to doubles, doubles are converted to integers using truncation). Negative counts result in the function failing. Positive counts that are too large and would cause a heap allocation error also result in function failing. When value is nil or omitted, table is preallocated without storing anything in it - this is roughly equivalent to creating a large table literal filled with `nil`, or preallocating a table by assigning a sufficiently large numeric index to a value and then erasing it by reassigning it to nil.

`table.find(table, value [, init])`: Looks for value in the array part of the table; returns index of first occurrence or nil if value is not found. Comparison is performed using standard equality (non-raw) to make sure that objects like Vector3 etc. can be found. The first nil value in the array part of the table terminates the traversal. init is an optional numeric index where the search starts and it defaults to 1; this can be useful to go through repeat occurrences.

`table.create` can not be replicated efficiently in Lua at all; `table.find` is provided as a faster and more convenient option compared to the code above.

`table.find` is roughly equivalent to the following code modulo semantical oddities with #t and performance:

```
function find(table, value, init)
    for i=init or 1, #table do
        if rawget(table, i) == value then
            return i
        end
    end
    return nil
end
``` 
