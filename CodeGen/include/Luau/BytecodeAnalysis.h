// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"

#include <vector>

#include <stdint.h>

namespace Luau
{
namespace CodeGen
{

struct IrFunction;

void buildBytecodeBlocks(IrFunction& function, const std::vector<uint8_t>& jumpTargets);
void analyzeBytecodeTypes(IrFunction& function);

} // namespace CodeGen
} // namespace Luau
