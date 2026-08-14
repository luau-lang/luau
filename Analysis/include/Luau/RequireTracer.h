// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/DenseHash2.h"
#include "Luau/FileResolver.h"
#include "Luau/Location.h"

#include <string>
#include <vector>

namespace Luau
{

class AstNode;
class AstStatBlock;

struct RequireTraceResult
{
    DenseHashMap2<const AstNode*, ModuleInfo> exprs;

    std::vector<std::pair<ModuleName, Location>> requireList;
};

RequireTraceResult traceRequires(FileResolver* fileResolver, AstStatBlock* root, const ModuleName& currentModuleName, const TypeCheckLimits& limits);

} // namespace Luau
