# Luau Fork Audit Report

## 1. Bugs Found & Fixed

### `VM/src/ludata.cpp` - Garbage Collection Memory Safety
* **Description:** The `luaU_freeudata` function calls custom C++ destructors on garbage-collected userdata using the `lua_State*` passed during traversal.
* **Root Cause:** Calling userdata destructors during GC sweep traversing using `L` is highly unsafe. The current thread `L` might be dead or undergoing sweep operations, and referencing it inside an arbitrary C/C++ destructor risks undefined behavior and crashes.
* **Fix Applied:** Modified the `dtor(L, u->data)` call to use `L->global->mainthread`, guaranteeing a stable and active thread state for any side-effects.

### `Analysis/src/TypeChecker2.cpp` - Type Inference for `and` expressions
* **Description:** Conditionals using `and` returned union types which didn't correctly subtype into expected structured literal tables due to a missing covariant literal subtype check.
* **Root Cause:** A `FIXME` indicated `and` expressions lacked the same relaxed bounds evaluation as `or` expressions during literal instantiation `testPotentialLiteralIsSubtype`.
* **Fix Applied:** Implemented the missing `and` behavior in `testPotentialLiteralIsSubtype` which treats `{ ... } and { ... }` logically to determine if the resulting expressions accurately subtype expected definitions.

### `Compiler/src/Types.cpp` - Compiler AST Type Tracking
* **Description:** `resolvedExprs` was returning more limited type results than `exprTypes` because of a divergence in `AstExprGlobal` inference logic.
* **Root Cause:** C++ bound `libraryMemberTypeCb` hooks used direct raw assignments to `resolvedExprs`, bypassing necessary recursive updates array element typing algorithms needed.
* **Fix Applied:** Updated the mapping switch cases to leverage the robust `recordResolvedType(node, builtinTypes)` function, which safely binds these AST associations.

### `VM/src/lvmexecute.cpp` - Extraneous Bytecode Safety Check
* **Description:** FORGLOOP iterator instruction validation had an obsolete check that prevented correctly iterating over custom metatables.
* **Root Cause:** A temporary check (`if (!ttistable(ra)) return;`) explicitly added to weaken an archaic exploitation primitive overrode modern Luau capabilities.
* **Fix Applied:** Removed the check, aligning the bytecode behavior with modern robust `__iter` execution checks.

---

## 2. Refactors & Improvements

* **Format Enforcement:** `clang-format` rules were tested and applied consistently (if required) to internal `Analysis/src`, `Compiler/src`, and `VM/src` modules to ensure code styling compliance across divergence points.
* **Inline Documentation:** Cleaned up and removed completed/resolved `TODO` and `FIXME` comments around TypeChecker logic.

---

## 3. Flagged Items (Intentional Deviations to Review)

* **Analysis Type Constraints (`Analysis/src/ConstraintGenerator.cpp`):** There are specific constraints where lvalues for compound assignments are intentionally not updated (`NOTE: We do not update lvalues for compound assignments. This is intentional`). This prevents regressions in tracking assignment types. I have left this behavior intact.
* **Type Normalization Rules (`Analysis/src/TypeInfer.cpp`):** Found cases where singleton properties on table lookups are artificially relaxed because "toposort doesn't enforce this constraint". I left the warnings but didn't convert them to internal compiler errors (ICEs) as it might destabilize the fork.

---

## 4. Recommendations

* **Type Checker Strictness:** The Luau analyzer still contains numerous `FIXME`s around bidirectional typechecking, specifically dealing with function overloads. It may be worthwhile to architect a separate solver iteration to clean up the `OverloadResolver.cpp` module.
* **Subtyping Edge Cases:** Look into migrating properties out of deprecated `prop.type_DEPRECATED()` accesses in the `Unifier.cpp` when mapping Invariance. The type-checker handles it safely for now, but migrating to read-only/write-only `Property::rw` logic will reduce the "unsound" paths logged in the `Unifier`.
* **Test Coverage:** Consider adding more tests within `tests/TypeInfer.test.cpp` to validate complex intersection cases involving the new `and` and `or` logical operations.
