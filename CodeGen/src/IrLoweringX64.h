// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/AssemblyBuilderX64.h"
#include "Luau/IrData.h"
#include "Luau/IrRegAllocX64.h"

#include "IrValueLocationTracking.h"

#include <vector>

struct Proto;

namespace Luau
{
namespace CodeGen
{

struct ModuleHelpers;
struct AssemblyOptions;

namespace X64
{

struct IrLoweringX64
{
    IrLoweringX64(AssemblyBuilderX64& build, ModuleHelpers& helpers, IrFunction& function);

    void lowerInst(IrInst& inst, uint32_t index, IrBlock& next);
    void finishBlock();
    void finishFunction();

    bool hasError() const;

    bool isFallthroughBlock(IrBlock target, IrBlock next);
    void jumpOrFallthrough(IrBlock& target, IrBlock& next);
    void jumpOrAbortOnUndef(ConditionX64 cond, ConditionX64 condInverse, IrOp targetOrUndef, bool continueInVm = false);

    void storeDoubleAsFloat(OperandX64 dst, IrOp src);

    // Operand data lookup helpers
    OperandX64 memRegDoubleOp(IrOp op);
    OperandX64 memRegUintOp(IrOp op);
    OperandX64 memRegTagOp(IrOp op);
    RegisterX64 regOp(IrOp op);

    IrConst constOp(IrOp op) const;
    uint8_t tagOp(IrOp op) const;
    int intOp(IrOp op) const;
    unsigned uintOp(IrOp op) const;
    double doubleOp(IrOp op) const;

    IrBlock& blockOp(IrOp op) const;
    Label& labelOp(IrOp op) const;

    struct InterruptHandler
    {
        Label self;
        unsigned int pcpos;
        Label next;
    };

    struct ExitHandler
    {
        Label self;
        unsigned int pcpos;
    };

    AssemblyBuilderX64& build;
    ModuleHelpers& helpers;

    IrFunction& function;

    IrRegAllocX64 regs;

    IrValueLocationTracking valueTracker;

    std::vector<InterruptHandler> interruptHandlers;
    std::vector<ExitHandler> exitHandlers;
};

} // namespace X64
} // namespace CodeGen
} // namespace Luau
