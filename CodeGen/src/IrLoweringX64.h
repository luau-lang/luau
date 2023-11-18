// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/AssemblyBuilderX64.h"
#include "Luau/DenseHash.h"
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
struct LoweringStats;

namespace X64
{

struct IrLoweringX64
{
    IrLoweringX64(AssemblyBuilderX64& build, ModuleHelpers& helpers, IrFunction& function, LoweringStats* stats);

    void lowerInst(IrInst& inst, uint32_t index, const IrBlock& next);
    void finishBlock(const IrBlock& curr, const IrBlock& next);
    void finishFunction();

    bool hasError() const;

    bool isFallthroughBlock(const IrBlock& target, const IrBlock& next);
    void jumpOrFallthrough(IrBlock& target, const IrBlock& next);

    Label& getTargetLabel(IrOp op, Label& fresh);
    void finalizeTargetLabel(IrOp op, Label& fresh);

    void jumpOrAbortOnUndef(ConditionX64 cond, IrOp target, const IrBlock& next);
    void jumpOrAbortOnUndef(IrOp target, const IrBlock& next);

    void storeDoubleAsFloat(OperandX64 dst, IrOp src);

    // Operand data lookup helpers
    OperandX64 memRegDoubleOp(IrOp op);
    OperandX64 memRegUintOp(IrOp op);
    OperandX64 memRegTagOp(IrOp op);
    RegisterX64 regOp(IrOp op);
    OperandX64 bufferAddrOp(IrOp bufferOp, IrOp indexOp);

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
    LoweringStats* stats = nullptr;

    IrRegAllocX64 regs;

    IrValueLocationTracking valueTracker;

    std::vector<InterruptHandler> interruptHandlers;
    std::vector<ExitHandler> exitHandlers;
    DenseHashMap<uint32_t, uint32_t> exitHandlerMap;
};

} // namespace X64
} // namespace CodeGen
} // namespace Luau
