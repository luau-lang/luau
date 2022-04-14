// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Ast.h"

namespace Luau
{
namespace Compile
{

// cost model: 8 bytes, where first byte is the baseline cost, and the next 7 bytes are discounts for when variable #i is constant
uint64_t modelCost(AstNode* root, AstLocal* const* vars, size_t varCount);

// cost is computed as B - sum(Di * Ci), where B is baseline cost, Di is the discount for each variable and Ci is 1 when variable #i is constant
int computeCost(uint64_t model, const bool* varsConst, size_t varCount);

} // namespace Compile
} // namespace Luau
