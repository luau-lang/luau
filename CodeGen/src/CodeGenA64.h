// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

namespace Luau
{
namespace CodeGen
{

struct NativeState;
struct ModuleHelpers;

namespace A64
{

class AssemblyBuilderA64;

bool initHeaderFunctions(NativeState& data);
void assembleHelpers(AssemblyBuilderA64& build, ModuleHelpers& helpers);

} // namespace A64
} // namespace CodeGen
} // namespace Luau
