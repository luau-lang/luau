// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Module.h"
#include "Luau/NotNull.h"
#include "Luau/DataFlowGraph.h"

namespace Luau
{

struct BuiltinTypes;
struct TypeFunctionRuntime;
struct UnifierSharedState;
struct TypeCheckLimits;

void checkNonStrict(
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeFunctionRuntime> typeFunctionRuntime,
    NotNull<InternalErrorReporter> ice,
    NotNull<UnifierSharedState> unifierState,
    NotNull<const DataFlowGraph> dfg,
    NotNull<TypeCheckLimits> limits,
    const SourceModule& sourceModule,
    Module* module
);


} // namespace Luau
