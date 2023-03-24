// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "IrLoweringA64.h"

#include "Luau/CodeGen.h"
#include "Luau/DenseHash.h"
#include "Luau/IrAnalysis.h"
#include "Luau/IrDump.h"
#include "Luau/IrUtils.h"

#include "EmitCommonA64.h"
#include "EmitInstructionA64.h"
#include "NativeState.h"

#include "lstate.h"

namespace Luau
{
namespace CodeGen
{
namespace A64
{

IrLoweringA64::IrLoweringA64(AssemblyBuilderA64& build, ModuleHelpers& helpers, NativeState& data, Proto* proto, IrFunction& function)
    : build(build)
    , helpers(helpers)
    , data(data)
    , proto(proto)
    , function(function)
{
    // In order to allocate registers during lowering, we need to know where instruction results are last used
    updateLastUseLocations(function);
}

// TODO: Eventually this can go away
bool IrLoweringA64::canLower(const IrFunction& function)
{
    for (const IrInst& inst : function.instructions)
    {
        switch (inst.cmd)
        {
        case IrCmd::NOP:
        case IrCmd::SUBSTITUTE:
        case IrCmd::INTERRUPT:
        case IrCmd::LOP_RETURN:
            continue;
        default:
            return false;
        }
    }

    return true;
}

void IrLoweringA64::lowerInst(IrInst& inst, uint32_t index, IrBlock& next)
{
    switch (inst.cmd)
    {
    case IrCmd::INTERRUPT:
    {
        emitInterrupt(build, uintOp(inst.a));
        break;
    }
    case IrCmd::LOP_RETURN:
    {
        emitInstReturn(build, helpers, vmRegOp(inst.a), intOp(inst.b));
        break;
    }
    default:
        LUAU_ASSERT(!"Not supported yet");
        break;
    }

    // TODO
    // regs.freeLastUseRegs(inst, index);
}

bool IrLoweringA64::isFallthroughBlock(IrBlock target, IrBlock next)
{
    return target.start == next.start;
}

void IrLoweringA64::jumpOrFallthrough(IrBlock& target, IrBlock& next)
{
    if (!isFallthroughBlock(target, next))
        build.b(target.label);
}

RegisterA64 IrLoweringA64::regOp(IrOp op) const
{
    IrInst& inst = function.instOp(op);
    LUAU_ASSERT(inst.regA64 != noreg);
    return inst.regA64;
}

IrConst IrLoweringA64::constOp(IrOp op) const
{
    return function.constOp(op);
}

uint8_t IrLoweringA64::tagOp(IrOp op) const
{
    return function.tagOp(op);
}

bool IrLoweringA64::boolOp(IrOp op) const
{
    return function.boolOp(op);
}

int IrLoweringA64::intOp(IrOp op) const
{
    return function.intOp(op);
}

unsigned IrLoweringA64::uintOp(IrOp op) const
{
    return function.uintOp(op);
}

double IrLoweringA64::doubleOp(IrOp op) const
{
    return function.doubleOp(op);
}

IrBlock& IrLoweringA64::blockOp(IrOp op) const
{
    return function.blockOp(op);
}

Label& IrLoweringA64::labelOp(IrOp op) const
{
    return blockOp(op).label;
}

} // namespace A64
} // namespace CodeGen
} // namespace Luau
