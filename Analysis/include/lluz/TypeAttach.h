// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lluz/Module.h"

#include <unordered_set>

namespace lluz
{

struct TypeRehydrationOptions
{
    std::unordered_set<std::string> bannedNames;
    bool expandClassProps = false;
};

void attachTypeData(SourceModule& source, Module& result);

AstType* rehydrateAnnotation(TypeId type, Allocator* allocator, const TypeRehydrationOptions& options = {});

} // namespace lluz
