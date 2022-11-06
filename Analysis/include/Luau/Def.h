// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/NotNull.h"
#include "Luau/TypedAllocator.h"
#include "Luau/Variant.h"

namespace Luau
{

using Def = Variant<struct Undefined, struct Phi>;

/**
 * We statically approximate a value at runtime using a symbolic value, which we call a Def.
 *
 * DataFlowGraphBuilder will allocate these defs as a stand-in for some Luau values, and bind them to places that
 * can hold a Luau value, and then observes how those defs will commute as it statically evaluate the program.
 *
 * It must also be noted that defs are a cyclic graph, so it is not safe to recursively traverse into it expecting it to terminate.
 */
using DefId = NotNull<const Def>;

/**
 * A "single-object" value.
 *
 * Leaky implementation note: sometimes "multiple-object" values, but none of which were interesting enough to warrant creating a phi node instead.
 * That can happen because there's no point in creating a phi node that points to either resultant in `if math.random() > 0.5 then 5 else "hello"`.
 * This might become of utmost importance if we wanted to do some backward reasoning, e.g. if `5` is taken, then `cond` must be `truthy`.
 */
struct Undefined
{
};

/**
 * A phi node is a union of defs.
 *
 * We need this because we're statically evaluating a program, and sometimes a place may be assigned with
 * different defs, and when that happens, we need a special data type that merges in all the defs
 * that will flow into that specific place. For example, consider this simple program:
 *
 * ```
 * x-1
 * if cond() then
 *   x-2 = 5
 * else
 *   x-3 = "hello"
 * end
 * x-4 : {x-2, x-3}
 * ```
 *
 * At x-4, we know for a fact statically that either `5` or `"hello"` can flow into the variable `x` after the branch, but
 * we cannot make any definitive decisions about which one, so we just take in both.
 */
struct Phi
{
    std::vector<DefId> operands;
};

template<typename T>
T* getMutable(DefId def)
{
    return get_if<T>(def.get());
}

template<typename T>
const T* get(DefId def)
{
    return getMutable<T>(def);
}

struct DefArena
{
    TypedAllocator<Def> allocator;

    DefId freshDef();
};

} // namespace Luau
