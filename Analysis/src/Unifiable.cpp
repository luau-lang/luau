// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Unifiable.h"

namespace Luau
{
namespace Unifiable
{

static int nextIndex = 0;

int freshIndex()
{
    return ++nextIndex;
}

Error::Error()
    : index(++nextIndex)
{
}

int Error::nextIndex = 0;

} // namespace Unifiable
} // namespace Luau
