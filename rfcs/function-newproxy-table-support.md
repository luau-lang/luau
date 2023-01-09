# Support table as first argument to newproxy

## Summary

Allow `newproxy` to be called with a table or nil instead of a boolean. This table (if specified) will be used as the metatable of the userdata.

## Motivation

Since Luau is derived from Lua 5.1 it retains the `newproxy` function used to create custom userdata objects from Lua. While this function is used by a number of libraries it has its limitations.

The `newproxy` function accepts a single, boolean parameter which specifies if the newly created userdata should be initialized with a metatable or not. If the userdata is initialized with a metatable then it can be retrieved and edited by calling `getmetatable`. Unfortunately, this means that a userdata created through `newproxy` cannot share a metatable with another userdata. There are two main implications of this:
- Memory - A userdata with a metatable has the same cost as a table with a metatable.
- Usability - userdata with the same 'type' do not have the same metatable.

## Design

Allow `newproxy` to be called with a table or nil instead of a boolean.
- When it is called with a table, the newly constructed userdata will have that table assigned as its metatable.
- When it is called with nil, the newly constructed userdata will have no metatable.

Passing a boolean to `newproxy` will continue to be supported for backwards compatability but is not recommended over the proposed behavior.

## Drawbacks

`newproxy` was never documented in Lua 5.1 and was later removed in Lua 5.2 after support for the `__gc` metamethod was added to tables. If we make this change adoption of `newproxy` is likely to increase which may not be desirable for similar reasons that it was removed from Lua.

## Alternatives

### Replace newproxy with a new method

`newproxy` was never documented in Lua 5.1 and removed in Lua 5.2. Rather than improving it we could add a new method which replaces `newproxy` altogether. Since this method would be almost identical to `newproxy` and we cannot remove `newproxy` due to backwards compatability it makes sense to change the existing method rather than introduce yet another API.
