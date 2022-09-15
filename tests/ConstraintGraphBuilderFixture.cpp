// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "ConstraintGraphBuilderFixture.h"

namespace Luau
{

ConstraintGraphBuilderFixture::ConstraintGraphBuilderFixture()
    : Fixture()
    , mainModule(new Module)
    , cgb("MainModule", mainModule, &arena, NotNull(&moduleResolver), singletonTypes, NotNull(&ice), frontend.getGlobalScope(), &logger)
    , forceTheFlag{"DebugLuauDeferredConstraintResolution", true}
{
    BlockedTypeVar::nextIndex = 0;
    BlockedTypePack::nextIndex = 0;
}

}
