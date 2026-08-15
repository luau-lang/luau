// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/DenseHash2.h"

#include <optional>
#include <string>
#include <vector>

namespace Luau::Require
{

class AliasCycleTracker
{
public:
    std::optional<std::string> add(std::string alias);

private:
    std::string getStringifiedCycle(const std::string& repeated) const;

    DenseHashSet2<std::string> seen;
    std::vector<std::string> ordered;
};

} // namespace Luau::Require
