// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/Constraint.h"
#include "Luau/ControlFlowGraph.h"
#include "Luau/DenseHash.h"

#include "Luau/NotNull.h"
#include "Luau/TypeFwd.h"
#include "Luau/Scope.h"

#include <vector>

namespace Luau
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
    void finalizeDomains();
    TypeId getRHSType(AstExpr* expr);
    TypeId getDomain(Symbol sym) const;
    void expandDomain(const LHS& lv, TypeId rhs);
    std::vector<ConstraintV> getConstraints(TypeId ty) const;

    std::vector<ConstraintV> flushConstraintsRequiringFinalization();


private:
    TypeId getType(SSAId def) const;
    void handleInstruction(InstructionId id);
    TypeId getDiscriminantOf(const NarrowingConstraint& constraint);
    void addConstraintForType(TypeId ty, ConstraintV&& constraint);
    void transferConstraints(TypeId from, TypeId to);


    TypeId newBlockedType(SSAId d);

    // This tracks a mapping from AstLocal/Global -> placeholder.
    DenseHashMap<Symbol, TypeId> lvalueDomain;
    // This tracks the SSA defs that participate in the domain of a given local
    DenseHashMap<TypeId, DenseHashMap<SSAId, TypeId>> domainStates;


    DenseHashMap<SSAId, TypeId> defTypes;

    DenseHashMap<SSAId, NarrowingConstraint> constraints;
    // Mapping from types to the constraints they require to be solved
    // Join instructions will require simplification constraints
    // Refine instructions will require refinement constraints

    DenseHashMap<TypeId, std::vector<ConstraintV>> typesRequiringConstraints;
    DenseHashMap<AstLocal*, std::vector<TypeId>> locals;

    NotNull<TypeArena> arena;
    NotNull<BuiltinTypes> builtinTypes;
    NotNull<ControlFlowGraph> g;
    NotNull<Scope> globalScope;
};


} // namespace Luau
