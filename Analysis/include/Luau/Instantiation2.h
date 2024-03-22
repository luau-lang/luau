// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/NotNull.h"
#include "Luau/Substitution.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeFwd.h"
#include "Luau/Unifiable.h"

namespace Luau
{

struct TypeArena;
struct TypeCheckLimits;

struct Replacer : Substitution
{
    DenseHashMap<TypeId, TypeId> replacements;
    DenseHashMap<TypePackId, TypePackId> replacementPacks;

    Replacer(NotNull<TypeArena> arena, DenseHashMap<TypeId, TypeId> replacements, DenseHashMap<TypePackId, TypePackId> replacementPacks)
        : Substitution(TxnLog::empty(), arena)
        , replacements(std::move(replacements))
        , replacementPacks(std::move(replacementPacks))
    {
    }

    bool isDirty(TypeId ty) override
    {
        return replacements.find(ty) != nullptr;
    }

    bool isDirty(TypePackId tp) override
    {
        return replacementPacks.find(tp) != nullptr;
    }

    TypeId clean(TypeId ty) override
    {
        return replacements[ty];
    }

    TypePackId clean(TypePackId tp) override
    {
        return replacementPacks[tp];
    }
};

// A substitution which replaces generic functions by monomorphic functions
struct Instantiation2 : Substitution
{
    // Mapping from generic types to free types to be used in instantiation.
    DenseHashMap<TypeId, TypeId> genericSubstitutions{nullptr};
    // Mapping from generic type packs to `TypePack`s of free types to be used in instantiation.
    DenseHashMap<TypePackId, TypePackId> genericPackSubstitutions{nullptr};

    Instantiation2(TypeArena* arena, DenseHashMap<TypeId, TypeId> genericSubstitutions, DenseHashMap<TypePackId, TypePackId> genericPackSubstitutions)
        : Substitution(TxnLog::empty(), arena)
        , genericSubstitutions(std::move(genericSubstitutions))
        , genericPackSubstitutions(std::move(genericPackSubstitutions))
    {
    }

    bool ignoreChildren(TypeId ty) override;
    bool isDirty(TypeId ty) override;
    bool isDirty(TypePackId tp) override;
    TypeId clean(TypeId ty) override;
    TypePackId clean(TypePackId tp) override;
};

} // namespace Luau
