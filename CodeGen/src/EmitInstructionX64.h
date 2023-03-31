// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <stdint.h>

namespace Luau
{
namespace CodeGen
{

struct Label;
struct ModuleHelpers;

namespace X64
{

class AssemblyBuilderX64;
struct IrRegAllocX64;

void emitInstCall(AssemblyBuilderX64& build, ModuleHelpers& helpers, int ra, int nparams, int nresults);
void emitInstReturn(AssemblyBuilderX64& build, ModuleHelpers& helpers, int ra, int actualResults);
void emitInstSetList(IrRegAllocX64& regs, AssemblyBuilderX64& build, Label& next, int ra, int rb, int count, uint32_t index);
void emitinstForGLoop(AssemblyBuilderX64& build, int ra, int aux, Label& loopRepeat, Label& loopExit);
void emitinstForGLoopFallback(AssemblyBuilderX64& build, int ra, int aux, Label& loopRepeat);
void emitInstForGPrepXnextFallback(AssemblyBuilderX64& build, int pcpos, int ra, Label& target);
void emitInstAnd(AssemblyBuilderX64& build, int ra, int rb, int rc);
void emitInstAndK(AssemblyBuilderX64& build, int ra, int rb, int kc);
void emitInstOr(AssemblyBuilderX64& build, int ra, int rb, int rc);
void emitInstOrK(AssemblyBuilderX64& build, int ra, int rb, int kc);
void emitInstGetImportFallback(AssemblyBuilderX64& build, int ra, uint32_t aux);
void emitInstCoverage(AssemblyBuilderX64& build, int pcpos);

} // namespace X64
} // namespace CodeGen
} // namespace Luau
