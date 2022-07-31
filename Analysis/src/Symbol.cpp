// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lluz/Symbol.h"

#include "lluz/Common.h"

namespace lluz
{

std::string toString(const Symbol& name)
{
    if (name.local)
        return name.local->name.value;

    lluz_ASSERT(name.global.value);
    return name.global.value;
}

} // namespace lluz
