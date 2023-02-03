// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IrAnalysis.h"

#include "Luau/IrData.h"
#include "Luau/IrUtils.h"

#include <stddef.h>

namespace Luau
{
namespace CodeGen
{

static void recordUse(IrInst& inst, size_t index)
{
    LUAU_ASSERT(inst.useCount < 0xffff);

    inst.useCount++;
    inst.lastUse = uint32_t(index);
}

void updateUseInfo(IrFunction& function)
{
    std::vector<IrInst>& instructions = function.instructions;

    for (IrInst& inst : instructions)
    {
        inst.useCount = 0;
        inst.lastUse = 0;
    }

    for (size_t i = 0; i < instructions.size(); ++i)
    {
        IrInst& inst = instructions[i];

        auto checkOp = [&instructions, i](IrOp op) {
            if (op.kind == IrOpKind::Inst)
                recordUse(instructions[op.index], i);
        };

        checkOp(inst.a);
        checkOp(inst.b);
        checkOp(inst.c);
        checkOp(inst.d);
        checkOp(inst.e);
    }
}

} // namespace CodeGen
} // namespace Luau
