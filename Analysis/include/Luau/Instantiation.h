// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/NotNull.h"
#include "Luau/Substitution.h"
#include "Luau/TypeFwd.h"
#include "Luau/Unifiable.h"
#include "Luau/VisitType.h"

LUAU_FASTFLAG(LuauExplicitSkipBoundTypes)

namespace Luau
{

struct TxnLog;
struct TypeArena;
struct TypeCheckLimits;

// A substitution which replaces generic types in a given set by free types.
struct ReplaceGenerics : Substitution
{
    ReplaceGenerics(
        const TxnLog* log,
        TypeArena* arena,
        NotNull<BuiltinTypes> builtinTypes,
        TypeLevel level,
        Scope* scope,
        const std::vector<TypeId>& generics,
        const std::vector<TypePackId>& genericPacks
    )
        : Substitution(log, arena)
        , builtinTypes(builtinTypes)
        , level(level)
        , scope(scope)
        , generics(generics)
        , genericPacks(genericPacks)
    {
    }

    void resetState(
        const TxnLog* log,
        TypeArena* arena,
        NotNull<BuiltinTypes> builtinTypes,
        TypeLevel level,
        Scope* scope,
        const std::vector<TypeId>& generics,
        const std::vector<TypePackId>& genericPacks
    );

    NotNull<BuiltinTypes> builtinTypes;

    TypeLevel level;
    Scope* scope;
    std::vector<TypeId> generics;
    std::vector<TypePackId> genericPacks;

    bool ignoreChildren(TypeId ty) override;
    bool isDirty(TypeId ty) override;
    bool isDirty(TypePackId tp) override;
    TypeId clean(TypeId ty) override;
    TypePackId clean(TypePackId tp) override;
};

// A substitution which replaces generic functions by monomorphic functions
struct Instantiation final : Substitution
{
    Instantiation(const TxnLog* log, TypeArena* arena, NotNull<BuiltinTypes> builtinTypes, TypeLevel level, Scope* scope)
        : Substitution(log, arena)
        , builtinTypes(builtinTypes)
        , level(level)
        , scope(scope)
        , reusableReplaceGenerics(log, arena, builtinTypes, level, scope, {}, {})
    {
    }

    void resetState(const TxnLog* log, TypeArena* arena, NotNull<BuiltinTypes> builtinTypes, TypeLevel level, Scope* scope);

    NotNull<BuiltinTypes> builtinTypes;

    TypeLevel level;
    Scope* scope;

    ReplaceGenerics reusableReplaceGenerics;

    bool ignoreChildren(TypeId ty) override;
    bool isDirty(TypeId ty) override;
    bool isDirty(TypePackId tp) override;
    TypeId clean(TypeId ty) override;
    TypePackId clean(TypePackId tp) override;
};

// Used to find if a FunctionType requires generic type cleanup during instantiation
struct GenericTypeFinder : TypeOnceVisitor
{
    bool found = false;

    GenericTypeFinder()
        : TypeOnceVisitor("GenericTypeFinder", FFlag::LuauExplicitSkipBoundTypes)
    {
    }

    bool visit(TypeId ty) override
    {
        return !found;
    }

    bool visit(TypePackId ty) override
    {
        return !found;
    }

    bool visit(TypeId ty, const Luau::FunctionType& ftv) override
    {
        if (ftv.hasNoFreeOrGenericTypes)
            return false;

        if (!ftv.generics.empty() || !ftv.genericPacks.empty())
            found = true;

        return !found;
    }

    bool visit(TypeId ty, const Luau::TableType& ttv) override
    {
        if (ttv.state == Luau::TableState::Generic)
            found = true;

        return !found;
    }

    bool visit(TypeId ty, const Luau::GenericType&) override
    {
        found = true;
        return false;
    }

    bool visit(TypePackId ty, const Luau::GenericTypePack&) override
    {
        found = true;
        return false;
    }

    bool visit(TypeId ty, const Luau::ExternType&) override
    {
        // During function instantiation, extern types are not traversed even if they have generics
        return false;
    }
};

/** Attempt to instantiate a type.  Only used under local type inference.
 *
 * When given a generic function type, instantiate() will return a copy with the
 * generics replaced by fresh types.  Instantiation will return the same TypeId
 * back if the function does not have any generics.
 *
 * All higher order generics are left as-is.  For example, instantiation of
 * <X>(<Y>(Y) -> (X, Y)) -> (X, Y) is (<Y>(Y) -> ('x, Y)) -> ('x, Y)
 *
 * We substitute the generic X for the free 'x, but leave the generic Y alone.
 *
 * Instantiation fails only when processing the type causes internal recursion
 * limits to be exceeded.
 */
std::optional<TypeId> instantiate(
    NotNull<BuiltinTypes> builtinTypes,
    NotNull<TypeArena> arena,
    NotNull<TypeCheckLimits> limits,
    NotNull<Scope> scope,
    TypeId ty
);

} // namespace Luau
