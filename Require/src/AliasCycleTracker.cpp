// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "AliasCycleTracker.h"

#include "Luau/DenseHash.h"
#include "Luau/StringUtils.h"

#include <optional>
#include <string>
#include <vector>

namespace Luau::Require
{

std::optional<std::string> AliasCycleTracker::add(std::string alias)
{
    if (seen.contains(alias))
        return Luau::format("detected alias cycle (%s)", getStringifiedCycle(alias).c_str());

    seen.insert(alias);
    ordered.push_back(std::move(alias));
    return std::nullopt;
}

std::string AliasCycleTracker::getStringifiedCycle(const std::string& repeated) const
{
    std::string result;
    bool inCycle = false;
    for (const std::string& item : ordered)
    {
        if (inCycle)
        {
            result += " -> ";
            result += "@" + item;
        }
        if (item == repeated)
        {
            inCycle = true;
            result += "@" + item;
        }
    }
    result += " -> " + ("@" + repeated);
    return result;
}

} // namespace Luau::Require
