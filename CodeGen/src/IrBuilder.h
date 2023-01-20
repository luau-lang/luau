// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/Bytecode.h"

#include "IrData.h"

#include <vector>

struct Proto;
typedef uint32_t Instruction;

namespace Luau
{
namespace CodeGen
{

struct AssemblyOptions;

struct IrBuilder
{
    void buildFunctionIr(Proto* proto);

    void rebuildBytecodeBasicBlocks(Proto* proto);
    void translateInst(LuauOpcode op, const Instruction* pc, int i);

    bool isInternalBlock(IrOp block);
    void beginBlock(IrOp block);

    IrOp constBool(bool value);
    IrOp constInt(int value);
    IrOp constUint(unsigned value);
    IrOp constDouble(double value);
    IrOp constTag(uint8_t value);
    IrOp constAny(IrConst constant);

    IrOp cond(IrCondition cond);

    IrOp inst(IrCmd cmd);
    IrOp inst(IrCmd cmd, IrOp a);
    IrOp inst(IrCmd cmd, IrOp a, IrOp b);
    IrOp inst(IrCmd cmd, IrOp a, IrOp b, IrOp c);
    IrOp inst(IrCmd cmd, IrOp a, IrOp b, IrOp c, IrOp d);
    IrOp inst(IrCmd cmd, IrOp a, IrOp b, IrOp c, IrOp d, IrOp e);

    IrOp block(IrBlockKind kind); // Requested kind can be ignored if we are in an outlined sequence
    IrOp blockAtInst(uint32_t index);

    IrOp vmReg(uint8_t index);
    IrOp vmConst(uint32_t index);
    IrOp vmUpvalue(uint8_t index);

    bool activeFastcallFallback = false;
    IrOp fastcallFallbackReturn;

    IrFunction function;

    std::vector<uint32_t> instIndexToBlock; // Block index at the bytecode instruction
};

} // namespace CodeGen
} // namespace Luau
