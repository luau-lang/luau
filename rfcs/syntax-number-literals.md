# Extended numeric literal syntax

> Note: this RFC was adapted from an internal proposal that predates RFC process and as such doesn't follow the template precisely

**Status**: Implemented

## Design

This proposal suggests extending Lua number syntax with:

1. Binary literals: `0b10101010101`. The prefix is either '0b' or '0B' (to match Lua's '0x' and '0X'). Followed by at least one 1 or 0.
2. Number literal separators: `1_034_123`. We will allow an arbitrary number  and arrangement of underscores in all numeric literals, including hexadecimal and binary. This helps with readability of long numbers.

Both of these features are standard in all modern languages, and can help write readable code. 
