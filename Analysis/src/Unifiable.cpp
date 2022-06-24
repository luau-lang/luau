// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Unifiable.h"

namespace Luau
{
namespace Unifiable
{

Free::Free(TypeLevel level)
    : index(++nextIndex)
    , level(level)
{
}

Free::Free(Scope2* scope)
    : scope(scope)
{
}

int Free::nextIndex = 0;

Generic::Generic()
    : index(++nextIndex)
    , name("g" + std::to_string(index))
{
}

Generic::Generic(TypeLevel level)
    : index(++nextIndex)
    , level(level)
    , name("g" + std::to_string(index))
{
}

Generic::Generic(const Name& name)
    : index(++nextIndex)
    , name(name)
    , explicitName(true)
{
}

Generic::Generic(Scope2* scope)
    : index(++nextIndex)
    , scope(scope)
{
}

Generic::Generic(TypeLevel level, const Name& name)
    : index(++nextIndex)
    , level(level)
    , name(name)
    , explicitName(true)
{
}

Generic::Generic(Scope2* scope, const Name& name)
    : index(++nextIndex)
    , scope(scope)
    , name(name)
    , explicitName(true)
{
}

int Generic::nextIndex = 0;

Error::Error()
    : index(++nextIndex)
{
}

int Error::nextIndex = 0;

} // namespace Unifiable
} // namespace Luau
