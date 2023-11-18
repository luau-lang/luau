// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <memory>

namespace Luau
{

struct Scope;
using ScopePtr = std::shared_ptr<Scope>;

enum class ControlFlow
{
    None = 0b00001,
    Returns = 0b00010,
    Throws = 0b00100,
    Breaks = 0b01000,
    Continues = 0b10000,
};

inline ControlFlow operator&(ControlFlow a, ControlFlow b)
{
    return ControlFlow(int(a) & int(b));
}

inline ControlFlow operator|(ControlFlow a, ControlFlow b)
{
    return ControlFlow(int(a) | int(b));
}

inline bool matches(ControlFlow a, ControlFlow b)
{
    return (a & b) != ControlFlow(0);
}

} // namespace Luau
