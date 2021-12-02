// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"

#include <string>

namespace Luau
{
struct TypeVar;
using TypeId = const TypeVar*;

struct TypePackVar;
using TypePackId = const TypePackVar*;

struct ToDotOptions
{
    bool showPointers = true;        // Show pointer value in the node label
    bool duplicatePrimitives = true; // Display primitive types and 'any' as separate nodes
};

std::string toDot(TypeId ty, const ToDotOptions& opts);
std::string toDot(TypePackId tp, const ToDotOptions& opts);

std::string toDot(TypeId ty);
std::string toDot(TypePackId tp);

void dumpDot(TypeId ty);
void dumpDot(TypePackId tp);

} // namespace Luau
