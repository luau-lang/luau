// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Symbol.h"

#include "Luau/Common.h"

namespace Luau
{

std::string toString(const Symbol& name)
{
    if (name.local)
        return name.local->name.value;

    LUAU_ASSERT(name.global.value);
    return name.global.value;
}

} // namespace Luau
