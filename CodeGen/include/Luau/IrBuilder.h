// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/Bytecode.h"
#include "Luau/IrData.h"

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

    void loadAndCheckTag(IrOp loc, uint8_t tag, IrOp fallback);

    // Clones all instructions into the current block
    // Source block that is cloned cannot use values coming in from a predecessor
    void clone(const IrBlock& source, bool removeCurrentTerminator);

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
    IrOp inst(IrCmd cmd, IrOp a, IrOp b, IrOp c, IrOp d, IrOp e, IrOp f);

    IrOp block(IrBlockKind kind); // Requested kind can be ignored if we are in an outlined sequence
    IrOp blockAtInst(uint32_t index);

    IrOp vmReg(uint8_t index);
    IrOp vmConst(uint32_t index);
    IrOp vmUpvalue(uint8_t index);

    bool inTerminatedBlock = false;

    bool activeFastcallFallback = false;
    IrOp fastcallFallbackReturn;

    IrFunction function;

    uint32_t activeBlockIdx = ~0u;

    std::vector<uint32_t> instIndexToBlock; // Block index at the bytecode instruction
};

} // namespace CodeGen
} // namespace Luau
