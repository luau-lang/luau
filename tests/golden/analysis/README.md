# Type inference analysis goldens

These folders contain CLI-observable ports of the C++ `TypeInfer.*.test.cpp`,
`TypeFunction.test.cpp`, and `TypeFunction.user.test.cpp` suites. Folder names
are normalized to kebab-case from the source category name; for example,
`TypeInfer.intersectionTypes.test.cpp` maps to `intersection-types/`, the
uncategorized `TypeInfer.test.cpp` maps to `general/`, and the two type function
suites map to `type-functions/` and `user-defined-type-functions/`.

Each entry is an independently runnable golden test. Its adjacent
`flags-on`/`flags-off` strict and nonstrict `.output` files encode complete
combined process output, so the suite detects changed diagnostics, changed
locations, unexpected warnings, and unexpected output from successful
analysis.

Only behavior visible through `luau-analyze` and safely runnable through
`luau` belongs here. Tests remain C++-only when they depend on fixture-provided
definition files or extern classes, hidden types such as `Not<T>`, direct
`TypeId`/arena/unifier inspection, autocomplete state, custom fast-flag
combinations outside the golden matrix, old-solver behavior, or crash-only
internal invariants. This currently excludes the `definitions`, `externTypes`,
`classes`, `negations`, `provisional`, and `tryUnify` categories except where a
portable equivalent is added later.
