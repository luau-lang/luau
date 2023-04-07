// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/AssemblyBuilderA64.h"
#include "Luau/IrData.h"

#include "IrRegAllocA64.h"

#include <vector>

struct Proto;

namespace Luau
{
namespace CodeGen
{

struct ModuleHelpers;
struct NativeState;
struct AssemblyOptions;

namespace A64
{

struct IrLoweringA64
{
    IrLoweringA64(AssemblyBuilderA64& build, ModuleHelpers& helpers, NativeState& data, Proto* proto, IrFunction& function);

    static bool canLower(const IrFunction& function);

    void lowerInst(IrInst& inst, uint32_t index, IrBlock& next);

    bool hasError() const;

    bool isFallthroughBlock(IrBlock target, IrBlock next);
    void jumpOrFallthrough(IrBlock& target, IrBlock& next);

    // Operand data build helpers
    RegisterA64 tempDouble(IrOp op);
    RegisterA64 tempInt(IrOp op);
    AddressA64 tempAddr(IrOp op, int offset);

    // Operand data lookup helpers
    RegisterA64 regOp(IrOp op) const;

    IrConst constOp(IrOp op) const;
    uint8_t tagOp(IrOp op) const;
    bool boolOp(IrOp op) const;
    int intOp(IrOp op) const;
    unsigned uintOp(IrOp op) const;
    double doubleOp(IrOp op) const;

    IrBlock& blockOp(IrOp op) const;
    Label& labelOp(IrOp op) const;

    AssemblyBuilderA64& build;
    ModuleHelpers& helpers;
    NativeState& data;
    Proto* proto = nullptr; // Temporarily required to provide 'Instruction* pc' to old emitInst* methods

    IrFunction& function;

    IrRegAllocA64 regs;
};

} // namespace A64
} // namespace CodeGen
} // namespace Luau
