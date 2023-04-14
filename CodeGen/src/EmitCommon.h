// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Label.h"

namespace Luau
{
namespace CodeGen
{

constexpr unsigned kTValueSizeLog2 = 4;
constexpr unsigned kLuaNodeSizeLog2 = 5;
constexpr unsigned kLuaNodeTagMask = 0xf;
constexpr unsigned kNextBitOffset = 4;

constexpr unsigned kOffsetOfTKeyTag = 12;  // offsetof cannot be used on a bit field
constexpr unsigned kOffsetOfTKeyNext = 12; // offsetof cannot be used on a bit field
constexpr unsigned kOffsetOfInstructionC = 3;

// Leaf functions that are placed in every module to perform common instruction sequences
struct ModuleHelpers
{
    // A64/X64
    Label exitContinueVm;
    Label exitNoContinueVm;

    // X64
    Label continueCallInVm;

    // A64
    Label reentry;   // x0: closure
    Label interrupt; // x0: pc offset, x1: return address, x2: interrupt
};

} // namespace CodeGen
} // namespace Luau
