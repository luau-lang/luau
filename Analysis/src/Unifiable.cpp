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

int Free::nextIndex = 0;

Generic::Generic()
    : index(++nextIndex)
    , name("g" + std::to_string(index))
    , explicitName(false)
{
}

Generic::Generic(TypeLevel level)
    : index(++nextIndex)
    , level(level)
    , name("g" + std::to_string(index))
    , explicitName(false)
{
}

Generic::Generic(const Name& name)
    : index(++nextIndex)
    , name(name)
    , explicitName(true)
{
}

Generic::Generic(TypeLevel level, const Name& name)
    : index(++nextIndex)
    , level(level)
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
