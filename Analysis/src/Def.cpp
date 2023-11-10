// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Def.h"
#include "Luau/Common.h"
#include "Luau/DenseHash.h"

#include <deque>

namespace Luau
{

bool containsSubscriptedDefinition(DefId def)
{
    if (auto cell = get<Cell>(def))
        return cell->subscripted;
    else if (auto phi = get<Phi>(def))
    {
        std::deque<DefId> queue(begin(phi->operands), end(phi->operands));
        DenseHashSet<const Def*> seen{nullptr};

        while (!queue.empty())
        {
            DefId next = queue.front();
            queue.pop_front();

            LUAU_ASSERT(!seen.find(next));
            if (seen.find(next))
                continue;
            seen.insert(next);

            if (auto cell_ = get<Cell>(next); cell_ && cell_->subscripted)
                return true;
            else if (auto phi_ = get<Phi>(next))
                queue.insert(queue.end(), phi_->operands.begin(), phi_->operands.end());
        }
    }
    return false;
}

DefId DefArena::freshCell(bool subscripted)
{
    return NotNull{allocator.allocate(Def{Cell{subscripted}})};
}

DefId DefArena::phi(DefId a, DefId b)
{
    if (a == b)
        return a;
    else
        return NotNull{allocator.allocate(Def{Phi{{a, b}}})};
}

} // namespace Luau
