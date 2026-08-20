// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/DenseHash2.h"
#include "Luau/NotNull.h"
#include "Luau/TypeFwd.h"

#include <optional>
#include <string>
#include <vector>

namespace Luau
{

struct TypeGuard
{
    bool isTypeof;
    AstExpr* target;
    std::string type;
};

std::optional<TypeGuard> matchTypeGuard(AstExprBinary::Op op, AstExpr* left, AstExpr* right);

// Search through the expression 'expr' for typeArguments that are known to represent
// uniquely held references. Append these typeArguments to 'uniqueTypes'.
void findUniqueTypes(NotNull<DenseHashSet2<TypeId>> uniqueTypes, AstExpr* expr, NotNull<const DenseHashMap2<const AstExpr*, TypeId>> astTypes);

void findUniqueTypes(
    NotNull<DenseHashSet2<TypeId>> uniqueTypes,
    AstArray<AstExpr*> exprs,
    NotNull<const DenseHashMap2<const AstExpr*, TypeId>> astTypes
);
void findUniqueTypes(
    NotNull<DenseHashSet2<TypeId>> uniqueTypes,
    const std::vector<AstExpr*>& exprs,
    NotNull<const DenseHashMap2<const AstExpr*, TypeId>> astTypes
);

} // namespace Luau
