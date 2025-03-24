// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Fixture.h"

namespace Luau
{

struct ClassFixture : BuiltinsFixture
{
    explicit ClassFixture(bool prepareAutocomplete = false);

    TypeId vector2Type;
    TypeId vector2InstanceType;
};

} // namespace Luau
