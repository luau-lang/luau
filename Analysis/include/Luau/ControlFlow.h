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

    // Returns OR Throws (e.g. if predicate then return else error() end)
    MixedFunctionExit = 0b100000,

    // Breaks OR Continues (e.g. if predicate then break else continue end)
    MixedLoopExit = 0b1000000,

    // Exits a loop OR function (e.g. if prediate then continue else return end)
    MixedExit = 0b10000000,
};
// Bitmask of all control flows which exit the nearest function scope
#define FunctionExitControlFlows ControlFlow::Returns | ControlFlow::Throws | ControlFlow::MixedFunctionExit
// Bitmask of all control flows which exit the nearest loop scope
#define LoopExitControlFlows ControlFlow::Breaks | ControlFlow::Continues | ControlFlow::MixedLoopExit
// Bitmask of all control flows which exit the nearest function or loop scopes
#define ExitingControlFlows ControlFlow::Returns | ControlFlow::Throws | ControlFlow::Breaks | ControlFlow::Continues | ControlFlow::MixedFunctionExit | ControlFlow::MixedLoopExit | ControlFlow::MixedExit

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
