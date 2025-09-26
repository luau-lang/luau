// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/Ast.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/DenseHash.h"
#include "Luau/NotNull.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypeArena.h"
#include "Luau/Unifier2.h"

namespace Luau
{

struct IncompleteInference
{
    TypeId expectedType;
    TypeId targetType;
    const AstExpr* expr;
};

struct PushTypeResult
{
    std::vector<IncompleteInference> incompleteTypes;
};

PushTypeResult pushTypeInto(
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astTypes,
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astExpectedTypes,
    NotNull<ConstraintSolver> solver,
    NotNull<const Constraint> constraint,
    NotNull<Unifier2> unifier,
    NotNull<Subtyping> subtyping,
    TypeId expectedType,
    const AstExpr* expr
);

}; // namespace Luau