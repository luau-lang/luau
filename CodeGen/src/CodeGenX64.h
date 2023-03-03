// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

namespace Luau
{
namespace CodeGen
{

struct NativeState;

namespace X64
{

bool initEntryFunction(NativeState& data);

} // namespace X64
} // namespace CodeGen
} // namespace Luau
