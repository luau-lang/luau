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

constexpr unsigned kOffsetOfLuaNodeTag = 12;  // offsetof cannot be used on a bit field
constexpr unsigned kOffsetOfLuaNodeNext = 12; // offsetof cannot be used on a bit field
constexpr unsigned kOffsetOfInstructionC = 3;

// Leaf functions that are placed in every module to perform common instruction sequences
struct ModuleHelpers
{
    Label exitContinueVm;
    Label exitNoContinueVm;
    Label continueCallInVm;
};

} // namespace CodeGen
} // namespace Luau
