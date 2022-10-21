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
enum class Condition;
struct Label;
struct NativeState;

void emitInstLoadNil(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstLoadB(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstLoadN(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstLoadK(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstLoadKX(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstMove(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstJump(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstJumpBack(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstJumpIf(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr, bool not_);
void emitInstJumpIfEq(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr, bool not_, Label& fallback);
void emitInstJumpIfEqFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr, bool not_);
void emitInstJumpIfCond(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr, Condition cond, Label& fallback);
void emitInstJumpIfCondFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr, Condition cond);
void emitInstJumpX(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstJumpxEqNil(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstJumpxEqB(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstJumpxEqN(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos, Label* labelarr);
void emitInstJumpxEqS(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstBinary(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, TMS tm, Label& fallback);
void emitInstBinaryFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, TMS tm);
void emitInstBinaryK(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, TMS tm, Label& fallback);
void emitInstBinaryKFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, TMS tm);
void emitInstPowK(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos, Label& fallback);
void emitInstNot(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstMinus(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback);
void emitInstMinusFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstLength(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback);
void emitInstLengthFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstNewTable(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstDupTable(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstSetList(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstGetUpval(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstSetUpval(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstCloseUpvals(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstFastCall1(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstFastCall2(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstFastCall2K(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstFastCall(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstForNPrep(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstForNLoop(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstAnd(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstAndK(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstOr(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstOrK(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstGetTableN(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback);
void emitInstGetTableNFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstSetTableN(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr, Label& fallback);
void emitInstSetTableNFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstGetTable(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback);
void emitInstGetTableFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstSetTable(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr, Label& fallback);
void emitInstSetTableFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstGetImport(AssemblyBuilderX64& build, const Instruction* pc, Label& fallback);
void emitInstGetImportFallback(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstGetTableKS(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback);
void emitInstSetTableKS(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr, Label& fallback);
void emitInstGetGlobal(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label& fallback);
void emitInstSetGlobal(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr, Label& fallback);
void emitInstConcat(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);

} // namespace CodeGen
} // namespace Luau
