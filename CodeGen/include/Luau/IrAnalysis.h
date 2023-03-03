// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <utility>

#include <stdint.h>

namespace Luau
{
namespace CodeGen
{

struct IrBlock;
struct IrFunction;

void updateUseCounts(IrFunction& function);

void updateLastUseLocations(IrFunction& function);

// Returns how many values are coming into the block (live in) and how many are coming out of the block (live out)
std::pair<uint32_t, uint32_t> getLiveInOutValueCount(IrFunction& function, IrBlock& block);
uint32_t getLiveInValueCount(IrFunction& function, IrBlock& block);
uint32_t getLiveOutValueCount(IrFunction& function, IrBlock& block);

} // namespace CodeGen
} // namespace Luau
