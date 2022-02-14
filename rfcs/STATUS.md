This document tracks unimplemented RFCs.

## Deprecate getfenv/setfenv

[RFC: Deprecate getfenv/setfenv](https://github.com/Roblox/luau/blob/master/rfcs/deprecate-getfenv-setfenv.md)

**Status**: Needs implementation.

**Notes**: Implementing this RFC triggers warnings across the board in the apps ecosystem, in particular in testing libraries. Pending code changes / decisions.

## Read-only and write-only properties

[RFC: Read-only properties](https://github.com/Roblox/luau/blob/master/rfcs/property-readonly.md) |
[RFC: Write-only properties](https://github.com/Roblox/luau/blob/master/rfcs/property-writeonly.md)

**Status**: Needs implementation

## Sealed/unsealed typing changes

[RFC: Sealed table subtyping](https://github.com/Roblox/luau/blob/master/rfcs/sealed-table-subtyping.md) |
[RFC: Unsealed table literals](https://github.com/Roblox/luau/blob/master/rfcs/unsealed-table-literals.md) |
[RFC: Only strip optional properties from unsealed tables during subtyping](https://github.com/Roblox/luau/blob/master/rfcs/unsealed-table-subtyping-strips-optional-properties.md)

**Status**: Implemented but depends on new transaction log implementation that is not fully live yet.

## Default type parameters

[RFC: Default type alias type parameters](https://github.com/Roblox/luau/blob/master/rfcs/syntax-default-type-alias-type-parameters.md)

**Status**: Implemented but not fully rolled out yet.

## Singleton types

[RFC: Singleton types](https://github.com/Roblox/luau/blob/master/rfcs/syntax-singleton-types.md)

**Status**: Implemented but not fully rolled out yet.

## Nil-forgiving operator

[RFC: nil-forgiving postfix operator !](https://github.com/Roblox/luau/blob/master/rfcs/syntax-nil-forgiving-operator.md)

**Status**: Needs implementation.

**Notes**: Do we need to reevaluate the necessity given `assert` and improved refinements?

## Safe navigation operator

[RFC: Safe navigation postfix operator (?)](https://github.com/Roblox/luau/blob/master/rfcs/syntax-safe-navigation-operator.md)

**Status**: Needs implementation.

**Notes**: We have unresolved issues with interaction between this feature and Roblox instance hierarchy. This may affect the viability of this proposal.

## String interpolation

[RFC: String interpolation](https://github.com/Roblox/luau/blob/master/rfcs/syntax-string-interpolation.md)

**Status**: Needs implementation

## Generalized iteration

[RFC: Generalized iteration](https://github.com/Roblox/luau/blob/master/rfcs/generalized-iteration.md)

**Status**: Needs implementation
