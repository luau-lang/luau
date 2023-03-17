// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <stdint.h>

#include "ltm.h"

typedef uint32_t Instruction;
typedef struct lua_TValue TValue;

namespace Luau
{
namespace CodeGen
{

struct Label;
struct ModuleHelpers;

namespace X64
{

class AssemblyBuilderX64;

void emitInstCall(AssemblyBuilderX64& build, ModuleHelpers& helpers, const Instruction* pc, int pcpos);
void emitInstReturn(AssemblyBuilderX64& build, ModuleHelpers& helpers, const Instruction* pc, int pcpos);
void emitInstSetList(AssemblyBuilderX64& build, const Instruction* pc, Label& next);
void emitinstForGLoop(AssemblyBuilderX64& build, int ra, int aux, Label& loopRepeat, Label& loopExit);
void emitinstForGLoopFallback(AssemblyBuilderX64& build, int pcpos, int ra, int aux, Label& loopRepeat);
void emitInstForGPrepXnextFallback(AssemblyBuilderX64& build, int pcpos, int ra, Label& target);
void emitInstAnd(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstAndK(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstOr(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstOrK(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstGetImportFallback(AssemblyBuilderX64& build, int ra, uint32_t aux);
void emitInstCoverage(AssemblyBuilderX64& build, int pcpos);

} // namespace X64
} // namespace CodeGen
} // namespace Luau
