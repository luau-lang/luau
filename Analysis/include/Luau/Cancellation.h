// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <atomic>

namespace Luau
{

struct FrontendCancellationToken
{
    void cancel()
    {
        cancelled.store(true);
    }

    bool requested()
    {
        return cancelled.load();
    }

    std::atomic<bool> cancelled;
};

} // namespace Luau
