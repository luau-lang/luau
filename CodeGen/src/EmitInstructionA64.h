// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

namespace Luau
{
namespace CodeGen
{

struct ModuleHelpers;

namespace A64
{

class AssemblyBuilderA64;

void emitInstReturn(AssemblyBuilderA64& build, ModuleHelpers& helpers, int ra, int n);

} // namespace A64
} // namespace CodeGen
} // namespace Luau
