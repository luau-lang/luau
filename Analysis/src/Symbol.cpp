// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Symbol.h"

#include "Luau/Common.h"

LUAU_FASTFLAG(LuauSolverV2)

namespace Luau
{

bool Symbol::operator==(const Symbol& rhs) const
{
    if (local)
        return local == rhs.local;
    else if (global.value)
        return rhs.global.value && global == rhs.global.value; // Subtlety: AstName::operator==(const char*) uses strcmp, not pointer identity.
    else
        return !rhs.local && !rhs.global.value; // Reflexivity: we already know `this` Symbol is empty, so check that rhs is.
}

std::string toString(const Symbol& name)
{
    if (name.local)
        return name.local->name.value;

    LUAU_ASSERT(name.global.value);
    return name.global.value;
}

} // namespace Luau
