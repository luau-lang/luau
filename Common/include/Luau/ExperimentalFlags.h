// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string.h>

namespace Luau
{

inline bool isFlagExperimental(const char* flag)
{
    // Flags in this list are disabled by default in various command-line tools. They may have behavior that is not fully final,
    // or critical bugs that are found after the code has been submitted.
    static const char* kList[] = {
        "LuauLowerBoundsCalculation",
        nullptr, // makes sure we always have at least one entry
    };

    for (const char* item : kList)
        if (item && strcmp(item, flag) == 0)
            return true;

    return false;
}

} // namespace Luau
