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

class AssemblyBuilderX64;
struct Label;
struct ModuleHelpers;

void emitInstNameCall(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, const TValue* k, Label& next, Label& fallback);
void emitInstCall(AssemblyBuilderX64& build, ModuleHelpers& helpers, const Instruction* pc, int pcpos);
void emitInstReturn(AssemblyBuilderX64& build, ModuleHelpers& helpers, const Instruction* pc, int pcpos);
void emitInstSetList(AssemblyBuilderX64& build, const Instruction* pc, Label& next);
void emitInstFastCall1(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback);
void emitInstFastCall2(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback);
void emitInstFastCall2K(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback);
void emitInstFastCall(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback);
void emitinstForGLoop(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& loopRepeat, Label& loopExit, Label& fallback);
void emitinstForGLoopFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& loopRepeat);
void emitInstForGPrepXnextFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& target);
void emitInstAnd(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstAndK(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstOr(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstOrK(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstGetImportFallback(AssemblyBuilderX64& build, int ra, uint32_t aux);
void emitInstCoverage(AssemblyBuilderX64& build, int pcpos);

} // namespace CodeGen
} // namespace Luau
