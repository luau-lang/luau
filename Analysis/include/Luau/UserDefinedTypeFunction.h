// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/TypeFunction.h"
#include "Luau/TypeFwd.h"

namespace Luau
{

TypeFunctionReductionResult<TypeId> userDefinedTypeFunction(
    TypeId instance,
    const std::vector<TypeId>& typeParams,
    const std::vector<TypePackId>& packParams,
    NotNull<TypeFunctionContext> ctx
);

}