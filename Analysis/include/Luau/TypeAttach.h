// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Module.h"

#include <unordered_set>

namespace Luau
{

struct TypeRehydrationOptions
{
    std::unordered_set<std::string> bannedNames;
    bool expandClassProps = false;
};

void attachTypeData(SourceModule& source, Module& result);

AstType* rehydrateAnnotation(TypeId type, Allocator* allocator, const TypeRehydrationOptions& options = {});

} // namespace Luau
