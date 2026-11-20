// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/NotNull.h"

#include <vector>

namespace Luau
{
struct Scope;

// Push a scope onto the end of a stack for the lifetime of the StackPusher instance
struct StackPusher
{
    std::vector<NotNull<Scope>>* stack;
    NotNull<Scope> scope;

    explicit StackPusher(std::vector<NotNull<Scope>>& stack, Scope* scope)
        : stack(&stack)
        , scope(scope)
    {
        stack.push_back(NotNull{scope});
    }

    ~StackPusher()
    {
        if (stack)
        {
            LUAU_ASSERT(stack->back() == scope);
            stack->pop_back();
        }
    }

    StackPusher(const StackPusher&) = delete;
    StackPusher&& operator=(const StackPusher&) = delete;

    StackPusher(StackPusher&& other)
        : stack(std::exchange(other.stack, nullptr))
        , scope(other.scope)
    {
    }
};

} // namespace Luau
