// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Constraint.h"
#include "Luau/ControlFlowGraph.h"
#include "Luau/DenseHash.h"

#include "Luau/NotNull.h"
#include "Luau/TypeFwd.h"
#include "Luau/Scope.h"

#include <optional>

namespace Luau
{

namespace CFG
{

// TypeStateMap walks a ControlFlowGraph in SSA form and assigns a TypeId to every
// Definition. The mapping is keyed on the definition.
//
// Rules:
//   Declare (no annotation)  -> BlockedType
//   Assign                   -> BlockedTypes
//   Join                     -> represents points where control flow merges - we allocate union types here, along with a simplification constraint
//   Refine                   -> refine<inputTy, discriminantTy> (type-function instance)
//
// The walk is two-pass and lazy-allocates BlockedType placeholders for any
// Join operand whose def hasn't been visited yet (back-edge phi operands). When
// the walk later reaches that def's Declare/Assign, the existing placeholder is
// reused.
struct TypeStateMap
{
    TypeStateMap(NotNull<TypeArena> arena, NotNull<Scope> globalScope, NotNull<BuiltinTypes> builtinTypes, NotNull<ControlFlowGraph> g);
    void computeTypes();
    TypeId getRHSType(AstExpr* expr);
    TypeId getLHSType(const LValue& lv) const;
    std::optional<ConstraintV> getOptionalConstraint(TypeId ty) const;

private:
    TypeId getType(Definition* def) const;
    void handleInstruction(InstrId id);
    TypeId getDiscriminantOf(const Refine& refine);

    DenseHashMap<Definition*, TypeId> defTypes{nullptr};

    // Mapping from types to the constraints they require to be solved
    // Join instructions will require simplification constraints
    // Refine instructions will require refinement constraints
    DenseHashMap<TypeId, ConstraintV> typesRequiringConstraint{nullptr};

    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<ControlFlowGraph> g;
    NotNull<Scope> globalScope;
};

} // namespace CFG

} // namespace Luau
