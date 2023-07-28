// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Cancellation.h"
#include "Luau/Error.h"

#include <memory>
#include <optional>
#include <string>

namespace Luau
{

class TimeLimitError : public InternalCompilerError
{
public:
    explicit TimeLimitError(const std::string& moduleName)
        : InternalCompilerError("Typeinfer failed to complete in allotted time", moduleName)
    {
    }
};

class UserCancelError : public InternalCompilerError
{
public:
    explicit UserCancelError(const std::string& moduleName)
        : InternalCompilerError("Analysis has been cancelled by user", moduleName)
    {
    }
};

struct TypeCheckLimits
{
    std::optional<double> finishTime;
    std::optional<int> instantiationChildLimit;
    std::optional<int> unifierIterationLimit;

    std::shared_ptr<FrontendCancellationToken> cancellationToken;
};

} // namespace Luau
