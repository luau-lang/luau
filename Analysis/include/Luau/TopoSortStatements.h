// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <vector>

namespace Luau
{

template<typename T>
struct AstArray;

class AstStat;

bool containsFunctionCall(const AstStat& stat);
bool isFunction(const AstStat& stat);
void toposort(std::vector<AstStat*>& stats);

} // namespace Luau
