// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lobject.h"
#include <stdint.h>

namespace Luau
{
namespace CodeGen
{

static bool hasTypedParameters(Proto* proto)
{
    return proto->typeinfo && proto->numparams != 0;
}

} // namespace CodeGen
} // namespace Luau
