// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/ConstraintGraphBuilder.h"
#include "Luau/ConstraintSolver.h"
#include "Luau/DcrLogger.h"
#include "Luau/TypeArena.h"
#include "Luau/Module.h"

#include "Fixture.h"
#include "ScopedFlags.h"

namespace Luau
{

struct ConstraintGraphBuilderFixture : Fixture
{
    TypeArena arena;
    ModulePtr mainModule;
    DcrLogger logger;
    UnifierSharedState sharedState{&ice};
    Normalizer normalizer{&arena, builtinTypes, NotNull{&sharedState}};

    std::unique_ptr<DataFlowGraph> dfg;
    std::unique_ptr<ConstraintGraphBuilder> cgb;
    Scope* rootScope = nullptr;

    std::vector<NotNull<Constraint>> constraints;

    ScopedFastFlag forceTheFlag;

    ConstraintGraphBuilderFixture();

    void generateConstraints(const std::string& code);
    void solve(const std::string& code);
};

} // namespace Luau
