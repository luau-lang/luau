// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/ConstraintGraphBuilder.h"
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
    ConstraintGraphBuilder cgb;
    DcrLogger logger;

    ScopedFastFlag forceTheFlag;

    ConstraintGraphBuilderFixture();
};

}
