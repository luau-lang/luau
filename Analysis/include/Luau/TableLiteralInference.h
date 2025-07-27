// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/DenseHash.h"
#include "Luau/NotNull.h"
#include "Luau/TypeFwd.h"

#include <vector>

namespace Luau
{

struct TypeArena;
struct BuiltinTypes;
struct Unifier2;
struct Subtyping;
class AstExpr;

TypeId matchLiteralType(
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astTypes,
    NotNull<DenseHashMap<const AstExpr*, TypeId>> astExpectedTypes,
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeArena> arena,
    NotNull<Unifier2> unifier,
    NotNull<Subtyping> subtyping,
    TypeId expectedType,
    TypeId exprType,
    const AstExpr* expr,
    std::vector<TypeId>& toBlock
);
} // namespace Luau
