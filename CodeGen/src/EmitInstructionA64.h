// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <stdint.h>

namespace Luau
{
namespace CodeGen
{

struct ModuleHelpers;

namespace A64
{

class AssemblyBuilderA64;

void emitInstReturn(AssemblyBuilderA64& build, ModuleHelpers& helpers, int ra, int n);
void emitInstCall(AssemblyBuilderA64& build, ModuleHelpers& helpers, int ra, int nparams, int nresults);
void emitInstGetImport(AssemblyBuilderA64& build, int ra, uint32_t aux);

} // namespace A64
} // namespace CodeGen
} // namespace Luau
