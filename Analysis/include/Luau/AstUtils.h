// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/DenseHash.h"
#include "Luau/NotNull.h"
#include "Luau/TypeFwd.h"

namespace Luau
{

// Search through the expression 'expr' for types that are known to represent
// uniquely held references. Append these types to 'uniqueTypes'.
void findUniqueTypes(NotNull<DenseHashSet<TypeId>> uniqueTypes, AstExpr* expr, NotNull<const DenseHashMap<const AstExpr*, TypeId>> astTypes);

void findUniqueTypes(
    NotNull<DenseHashSet<TypeId>> uniqueTypes,
    AstArray<AstExpr*> exprs,
    NotNull<const DenseHashMap<const AstExpr*, TypeId>> astTypes
);
void findUniqueTypes(
    NotNull<DenseHashSet<TypeId>> uniqueTypes,
    const std::vector<AstExpr*>& exprs,
    NotNull<const DenseHashMap<const AstExpr*, TypeId>> astTypes
);

} // namespace Luau
