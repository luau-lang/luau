// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Unifiable.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypePack.h"

namespace Luau
{
namespace Unifiable
{

static int nextIndex = 0;

int freshIndex()
{
    return ++nextIndex;
}

template<typename Id>
Error<Id>::Error()
    : index(++nextIndex)
{
}

template<typename Id>
int Error<Id>::nextIndex = 0;

template struct Error<TypeId>;
template struct Error<TypePackId>;

} // namespace Unifiable
} // namespace Luau
