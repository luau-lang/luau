// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"
#include "Luau/DenseHash.h"
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

} // namespace Luau
