// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "ConstantFolding.h"

namespace Luau
{
namespace Compile
{

Constant foldBuiltin(int bfid, const Constant* args, size_t count);

} // namespace Compile
} // namespace Luau
