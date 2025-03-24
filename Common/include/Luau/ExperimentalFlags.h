// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <string.h>

namespace Luau
{

inline bool isAnalysisFlagExperimental(const char* flag)
{
    // Flags in this list are disabled by default in various command-line tools. They may have behavior that is not fully final,
    // or critical bugs that are found after the code has been submitted. This list is intended _only_ for flags that affect
    // Luau's type checking. Flags that may change runtime behavior (e.g.: parser or VM flags) are not appropriate for this list.
    static const char* const kList[] = {
        "LuauInstantiateInSubtyping",      // requires some fixes to lua-apps code
        "LuauFixIndexerSubtypingOrdering", // requires some small fixes to lua-apps code since this fixes a false negative
        "StudioReportLuauAny2",            // takes telemetry data for usage of any types
        "LuauTableCloneClonesType3",       // requires fixes in lua-apps code, terrifyingly
        "LuauSolverV2",
        // makes sure we always have at least one entry
        nullptr,
    };

    for (const char* item : kList)
        if (item && strcmp(item, flag) == 0)
            return true;

    return false;
}

} // namespace Luau
