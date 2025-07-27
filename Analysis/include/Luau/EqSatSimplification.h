// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#pragma once

#include "Luau/TypeFwd.h"
#include "Luau/NotNull.h"

#include <memory>
#include <optional>
#include <vector>

namespace Luau
{
struct TypeArena;
}

// The EqSat stuff is pretty template heavy, so we go to some lengths to prevent
// the complexity from leaking outside its implementation sources.
namespace Luau::EqSatSimplification
{

struct Simplifier;

using SimplifierPtr = std::unique_ptr<Simplifier, void (*)(Simplifier*)>;

SimplifierPtr newSimplifier(NotNull<TypeArena> arena, NotNull<BuiltinTypes> builtinTypes);

} // namespace Luau::EqSatSimplification

namespace Luau
{

struct EqSatSimplificationResult
{
    TypeId result;

    // New type function applications that were created by the reduction phase.
    // We return these so that the ConstraintSolver can know to try to reduce
    // them.
    std::vector<TypeId> newTypeFunctions;
};

using EqSatSimplification::newSimplifier;    // NOLINT: clang-tidy thinks these are unused.  It is incorrect.
using Luau::EqSatSimplification::Simplifier; // NOLINT
using Luau::EqSatSimplification::SimplifierPtr;

std::optional<EqSatSimplificationResult> eqSatSimplify(NotNull<Simplifier> simplifier, TypeId ty);

} // namespace Luau
