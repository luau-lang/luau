// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <stdint.h>

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

void emitInstLoadNil(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc);
void emitInstLoadB(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstLoadN(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc);
void emitInstLoadK(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, const TValue* k);
void emitInstMove(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc);
void emitInstJump(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstJumpBack(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstJumpIf(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, int pcpos, Label* labelarr, bool not_);
void emitInstJumpIfEq(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, int pcpos, Label* labelarr, bool not_);
void emitInstJumpIfCond(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, int pcpos, Label* labelarr, Condition cond);
void emitInstJumpX(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstJumpxEqNil(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, const TValue* k, int pcpos, Label* labelarr);
void emitInstJumpxEqB(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, const TValue* k, int pcpos, Label* labelarr);
void emitInstJumpxEqN(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, const TValue* k, int pcpos, Label* labelarr);
void emitInstJumpxEqS(AssemblyBuilderX64& build, NativeState& data, const Instruction* pc, const TValue* k, int pcpos, Label* labelarr);
void emitInstAdd(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstSub(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstMul(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstDiv(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstMod(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstPow(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstAddK(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos);
void emitInstSubK(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos);
void emitInstMulK(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos);
void emitInstDivK(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos);
void emitInstModK(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos);
void emitInstPowK(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos);
void emitInstNot(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstMinus(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstLength(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstGetUpval(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstFastCall1(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstFastCall2(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstFastCall2K(AssemblyBuilderX64& build, const Instruction* pc, const TValue* k, int pcpos, Label* labelarr);
void emitInstFastCall(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstForNPrep(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstForNLoop(AssemblyBuilderX64& build, const Instruction* pc, int pcpos, Label* labelarr);
void emitInstAnd(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstAndK(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstOr(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstOrK(AssemblyBuilderX64& build, const Instruction* pc);
void emitInstGetTableN(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstSetTableN(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstGetTable(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);
void emitInstSetTable(AssemblyBuilderX64& build, const Instruction* pc, int pcpos);

} // namespace CodeGen
} // namespace Luau
