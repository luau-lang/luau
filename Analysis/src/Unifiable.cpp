// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Unifiable.h"

LUAU_FASTFLAG(LuauTypeNormalization2);

namespace Luau
{
namespace Unifiable
{

static int nextIndex = 0;

Free::Free(TypeLevel level)
    : index(FFlag::LuauTypeNormalization2 ? ++nextIndex : ++DEPRECATED_nextIndex)
    , level(level)
{
}

Free::Free(Scope* scope)
    : index(FFlag::LuauTypeNormalization2 ? ++nextIndex : ++DEPRECATED_nextIndex)
    , scope(scope)
{
}

Free::Free(Scope* scope, TypeLevel level)
    : index(FFlag::LuauTypeNormalization2 ? ++nextIndex : ++DEPRECATED_nextIndex)
    , level(level)
    , scope(scope)
{
}

int Free::DEPRECATED_nextIndex = 0;

Generic::Generic()
    : index(FFlag::LuauTypeNormalization2 ? ++nextIndex : ++DEPRECATED_nextIndex)
    , name("g" + std::to_string(index))
{
}

Generic::Generic(TypeLevel level)
    : index(FFlag::LuauTypeNormalization2 ? ++nextIndex : ++DEPRECATED_nextIndex)
    , level(level)
    , name("g" + std::to_string(index))
{
}

Generic::Generic(const Name& name)
    : index(FFlag::LuauTypeNormalization2 ? ++nextIndex : ++DEPRECATED_nextIndex)
    , name(name)
    , explicitName(true)
{
}

Generic::Generic(Scope* scope)
    : index(FFlag::LuauTypeNormalization2 ? ++nextIndex : ++DEPRECATED_nextIndex)
    , scope(scope)
{
}

Generic::Generic(TypeLevel level, const Name& name)
    : index(FFlag::LuauTypeNormalization2 ? ++nextIndex : ++DEPRECATED_nextIndex)
    , level(level)
    , name(name)
    , explicitName(true)
{
}

Generic::Generic(Scope* scope, const Name& name)
    : index(FFlag::LuauTypeNormalization2 ? ++nextIndex : ++DEPRECATED_nextIndex)
    , scope(scope)
    , name(name)
    , explicitName(true)
{
}

int Generic::DEPRECATED_nextIndex = 0;

Error::Error()
    : index(++nextIndex)
{
}

int Error::nextIndex = 0;

} // namespace Unifiable
} // namespace Luau
