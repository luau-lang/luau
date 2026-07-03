// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BytecodeGraph.h"

#pragma once

#include <algorithm>

namespace Luau
{

namespace Bytecode
{

template<typename VmConst>
inline std::vector<BcOp>& usesOf(BcFunction<VmConst>& fn, const BcOp& def)
{
    if (def.kind == BcOpKind::Inst)
        return fn.instOp(def).uses;
    else
    {
        LUAU_ASSERT(def.kind == BcOpKind::Phi);
        return fn.phiOp(def).uses;
    }
}

template<typename VmConst>
inline int countUses(BcFunction<VmConst>& fn, const BcOp& def, const BcOp& consumer)
{
    std::vector<BcOp>& uses = usesOf(fn, def);
    return static_cast<int>(std::count(uses.begin(), uses.end(), consumer));
}

template<typename VmConst>
inline bool hasUse(BcFunction<VmConst>& fn, const BcOp& def, const BcOp& consumer)
{
    return countUses(fn, def, consumer) > 0;
}

// for every operand of every inst/phi in a live block, the referenced def must have that consumer in its `uses`
template<typename VmConst>
bool verifyUseConsistency(BcFunction<VmConst>& fn)
{
    auto checkOperand = [&](const BcOp& consumer, BcOp operand)
    {
        if (operand.kind != BcOpKind::Inst && operand.kind != BcOpKind::Phi)
            return true;
        return hasUse(fn, operand, consumer);
    };

    bool result = true;
    for (BcBlock& block : fn.blocks)
    {
        if ((block.flags & BcBlockFlag::Dead) != 0)
            continue;

        for (const BcOp& phiOp : block.phis)
            for (const BcOp& operand : fn.phiOp(phiOp).ops)
                result &= checkOperand(phiOp, operand);

        for (const BcOp& instOp : block.ops)
            for (const BcOp& operand : fn.instOp(instOp).ops)
                result &= checkOperand(instOp, operand);
    }
    return result;
}

} // namespace Bytecode

} // namespace Luau
