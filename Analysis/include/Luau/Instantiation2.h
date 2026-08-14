// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/NotNull.h"
#include "Luau/Substitution.h"
#include "Luau/Subtyping.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeFwd.h"
#include "Luau/Unifiable.h"

namespace Luau
{

struct TypeArena;
struct TypeCheckLimits;

struct Replacer_DEPRECATED : Substitution
{
    DenseHashMap2<TypeId, TypeId> replacements;
    DenseHashMap2<TypePackId, TypePackId> replacementPacks;

    Replacer_DEPRECATED(NotNull<TypeArena> arena, DenseHashMap2<TypeId, TypeId> replacements, DenseHashMap2<TypePackId, TypePackId> replacementPacks)
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
        TypeId res = replacements[ty];
        LUAU_ASSERT(res);
        dontTraverseInto(res);
        return res;
    }

    TypePackId clean(TypePackId tp) override
    {
        TypePackId res = replacementPacks[tp];
        LUAU_ASSERT(res);
        dontTraverseInto(res);
        return res;
    }
};

struct Replacer : Substitution
{
    NotNull<DenseHashMap2<TypeId, TypeId>> replacements;
    NotNull<DenseHashMap2<TypePackId, TypePackId>> replacementPacks;

    Replacer(
        NotNull<TypeArena> arena,
        NotNull<DenseHashMap2<TypeId, TypeId>> replacements,
        NotNull<DenseHashMap2<TypePackId, TypePackId>> replacementPacks
    );

    bool isDirty(TypeId ty) override;

    bool isDirty(TypePackId tp) override;

    TypeId clean(TypeId ty) override;

    TypePackId clean(TypePackId tp) override;

    bool ignoreChildren(TypeId ty) override;

private:
    /**
     * It is *very* easy to create the world's worst bug by using a bound type
     * as key: this is a helper function we run in debug mode to confirm this
     * isn't the case.
     */
    bool checkReplacementKeys() const;
};

// A substitution which replaces generic functions by monomorphic functions
struct Instantiation2_DEPRECATED final : Substitution
{
    // Mapping from generic types to free types to be used in instantiation.
    DenseHashMap2<TypeId, TypeId> genericSubstitutions;
    // Mapping from generic type packs to `TypePack`s of free types to be used in instantiation.
    DenseHashMap2<TypePackId, TypePackId> genericPackSubstitutions;

    // Make `NotNull` with LuauInstantiationUsesGenericPolarity
    Subtyping* subtyping = nullptr;
    Scope* scope = nullptr;

    Instantiation2_DEPRECATED(
        TypeArena* arena,
        DenseHashMap2<TypeId, TypeId> genericSubstitutions,
        DenseHashMap2<TypePackId, TypePackId> genericPackSubstitutions
    )
        : Substitution(TxnLog::empty(), arena)
        , genericSubstitutions(std::move(genericSubstitutions))
        , genericPackSubstitutions(std::move(genericPackSubstitutions))
    {
    }

    Instantiation2_DEPRECATED(
        TypeArena* arena,
        DenseHashMap2<TypeId, TypeId> genericSubstitutions,
        DenseHashMap2<TypePackId, TypePackId> genericPackSubstitutions,
        NotNull<Subtyping> subtyping,
        NotNull<Scope> scope
    )
        : Substitution(TxnLog::empty(), arena)
        , genericSubstitutions(std::move(genericSubstitutions))
        , genericPackSubstitutions(std::move(genericPackSubstitutions))
        , subtyping(subtyping)
        , scope(scope)
    {
    }

    bool ignoreChildren(TypeId ty) override;
    bool isDirty(TypeId ty) override;
    bool isDirty(TypePackId tp) override;
    TypeId clean(TypeId ty) override;
    TypePackId clean(TypePackId tp) override;
};

void resolveGenericSubstitutions(
    TypeArena* arena,
    DenseHashMap2<TypeId, TypeId>& genericSubstitutions,
    DenseHashMap2<TypePackId, TypePackId>& genericPackSubstitutions,
    NotNull<Subtyping> subtyping,
    NotNull<Scope> scope
);

// FIXME: This process needs a rename.  It's not really instantiation.  It's the
// process of substituting generics in a function type for inferred
// substitutions.
std::optional<TypeId> instantiate2(
    TypeArena* arena,
    DenseHashMap2<TypeId, TypeId> genericSubstitutions,
    DenseHashMap2<TypePackId, TypePackId> genericPackSubstitutions,
    NotNull<Subtyping> subtyping,
    NotNull<Scope> scope,
    TypeId ty
);

std::optional<TypePackId> instantiate2(
    TypeArena* arena,
    DenseHashMap2<TypeId, TypeId> genericSubstitutions,
    DenseHashMap2<TypePackId, TypePackId> genericPackSubstitutions,
    NotNull<Subtyping> subtyping,
    NotNull<Scope> scope,
    TypePackId tp
);

} // namespace Luau
