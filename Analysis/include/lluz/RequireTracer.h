// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lluz/DenseHash.h"
#include "lluz/FileResolver.h"
#include "lluz/Location.h"

#include <string>

namespace lluz
{

class AstStat;
class AstExpr;
class AstStatBlock;
struct AstLocal;

struct RequireTraceResult
{
    DenseHashMap<const AstExpr*, ModuleInfo> exprs{nullptr};

    std::vector<std::pair<ModuleName, Location>> requireList;
};

RequireTraceResult traceRequires(FileResolver* fileResolver, AstStatBlock* root, const ModuleName& currentModuleName);

} // namespace lluz
