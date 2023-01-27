// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/AssemblyBuilderX64.h"

#include "IrData.h"

#include <array>
#include <initializer_list>
#include <vector>

struct Proto;

namespace Luau
{
namespace CodeGen
{

struct ModuleHelpers;
struct NativeState;
struct AssemblyOptions;

struct IrLoweringX64
{
    // Some of these arguments are only required while we re-use old direct bytecode to x64 lowering
    IrLoweringX64(AssemblyBuilderX64& build, ModuleHelpers& helpers, NativeState& data, Proto* proto, IrFunction& function);

    void lower(AssemblyOptions options);

    void lowerInst(IrInst& inst, uint32_t index, IrBlock& next);

    bool isFallthroughBlock(IrBlock target, IrBlock next);
    void jumpOrFallthrough(IrBlock& target, IrBlock& next);

    // Operand data lookup helpers
    OperandX64 memRegDoubleOp(IrOp op) const;
    OperandX64 memRegTagOp(IrOp op) const;
    RegisterX64 regOp(IrOp op) const;

    IrConst constOp(IrOp op) const;
    uint8_t tagOp(IrOp op) const;
    bool boolOp(IrOp op) const;
    int intOp(IrOp op) const;
    unsigned uintOp(IrOp op) const;
    double doubleOp(IrOp op) const;

    IrBlock& blockOp(IrOp op) const;
    Label& labelOp(IrOp op) const;

    // Unscoped register allocation
    RegisterX64 allocGprReg(SizeX64 preferredSize);
    RegisterX64 allocXmmReg();

    RegisterX64 allocGprRegOrReuse(SizeX64 preferredSize, uint32_t index, std::initializer_list<IrOp> oprefs);
    RegisterX64 allocXmmRegOrReuse(uint32_t index, std::initializer_list<IrOp> oprefs);

    void freeReg(RegisterX64 reg);
    void freeLastUseReg(IrInst& target, uint32_t index);
    void freeLastUseRegs(const IrInst& inst, uint32_t index);

    ConditionX64 getX64Condition(IrCondition cond) const;

    struct ScopedReg
    {
        ScopedReg(IrLoweringX64& owner, SizeX64 size);
        ~ScopedReg();

        ScopedReg(const ScopedReg&) = delete;
        ScopedReg& operator=(const ScopedReg&) = delete;

        void free();

        IrLoweringX64& owner;
        RegisterX64 reg;
    };

    AssemblyBuilderX64& build;
    ModuleHelpers& helpers;
    NativeState& data;
    Proto* proto = nullptr; // Temporarily required to provide 'Instruction* pc' to old emitInst* methods

    IrFunction& function;

    std::array<bool, 16> freeGprMap;
    std::array<bool, 16> freeXmmMap;
};

} // namespace CodeGen
} // namespace Luau
