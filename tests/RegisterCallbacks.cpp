// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "RegisterCallbacks.h"

namespace Luau
{

std::unordered_set<RegisterCallback>& getRegisterCallbacks()
{
    static std::unordered_set<RegisterCallback> cbs;
    return cbs;
}

int addTestCallback(RegisterCallback cb)
{
    getRegisterCallbacks().insert(cb);
    return 0;
}

} // namespace Luau
