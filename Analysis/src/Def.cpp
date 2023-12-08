// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Def.h"

#include "Luau/Common.h"

#include <algorithm>
#include <deque>

namespace Luau
{

bool containsSubscriptedDefinition(DefId def)
{
    if (auto cell = get<Cell>(def))
        return cell->subscripted;
    else if (auto phi = get<Phi>(def))
        return std::any_of(phi->operands.begin(), phi->operands.end(), containsSubscriptedDefinition);
    else
        return false;
}

DefId DefArena::freshCell(bool subscripted)
{
    return NotNull{allocator.allocate(Def{Cell{subscripted}})};
}

static void collectOperands(DefId def, std::vector<DefId>& operands)
{
    if (std::find(operands.begin(), operands.end(), def) != operands.end())
        return;
    else if (get<Cell>(def))
        operands.push_back(def);
    else if (auto phi = get<Phi>(def))
    {
        for (const Def* operand : phi->operands)
            collectOperands(NotNull{operand}, operands);
    }
}

DefId DefArena::phi(DefId a, DefId b)
{
    return phi({a, b});
}

DefId DefArena::phi(const std::vector<DefId>& defs)
{
    std::vector<DefId> operands;
    for (DefId operand : defs)
        collectOperands(operand, operands);

    // There's no need to allocate a Phi node for a singleton set.
    if (operands.size() == 1)
        return operands[0];
    else
        return NotNull{allocator.allocate(Def{Phi{std::move(operands)}})};
}

} // namespace Luau
