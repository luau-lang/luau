// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Substitution.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeFwd.h"

namespace Luau
{

// A substitution which replaces the type parameters of a type function by arguments
struct ApplyTypeFunction : Substitution
{
    ApplyTypeFunction(TypeArena* arena)
        : Substitution(TxnLog::empty(), arena)
        , encounteredForwardedType(false)
    {
    }

    // Never set under deferred constraint resolution.
    bool encounteredForwardedType;
    std::unordered_map<TypeId, TypeId> typeArguments;
    std::unordered_map<TypePackId, TypePackId> typePackArguments;
    bool ignoreChildren(TypeId ty) override;
    bool ignoreChildren(TypePackId tp) override;
    bool isDirty(TypeId ty) override;
    bool isDirty(TypePackId tp) override;
    TypeId clean(TypeId ty) override;
    TypePackId clean(TypePackId tp) override;
};

} // namespace Luau
