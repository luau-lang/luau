// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/DenseHash.h"
#include "Luau/FileResolver.h"
#include "Luau/Location.h"

#include <string>

namespace Luau
{

class AstStat;
class AstExpr;
class AstStatBlock;
struct AstLocal;

struct RequireTraceResult
{
    DenseHashMap<const AstExpr*, ModuleName> exprs{0};
    DenseHashMap<const AstExpr*, bool> optional{0};

    std::vector<std::pair<ModuleName, Location>> requires;
};

RequireTraceResult traceRequires(FileResolver* fileResolver, AstStatBlock* root, ModuleName currentModuleName);

} // namespace Luau
