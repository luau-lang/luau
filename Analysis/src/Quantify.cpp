// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Quantify.h"

#include "Luau/Scope.h"
#include "Luau/Substitution.h"
#include "Luau/TxnLog.h"
#include "Luau/VisitTypeVar.h"

LUAU_FASTFLAG(LuauAlwaysQuantify);
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);
LUAU_FASTFLAGVARIABLE(LuauQuantifyConstrained, false)

namespace Luau
{

/// @return true if outer encloses inner
static bool subsumes(Scope2* outer, Scope2* inner)
{
    while (inner)
    {
        if (inner == outer)
            return true;
        inner = inner->parent;
    }

    return false;
}

struct Quantifier final : TypeVarOnceVisitor
{
    TypeLevel level;
    std::vector<TypeId> generics;
    std::vector<TypePackId> genericPacks;
    Scope2* scope = nullptr;
    bool seenGenericType = false;
    bool seenMutableType = false;

    explicit Quantifier(TypeLevel level)
        : level(level)
    {
        LUAU_ASSERT(!FFlag::DebugLuauDeferredConstraintResolution);
    }

    explicit Quantifier(Scope2* scope)
        : scope(scope)
    {
        LUAU_ASSERT(FFlag::DebugLuauDeferredConstraintResolution);
    }

    /// @return true if outer encloses inner
    bool subsumes(Scope2* outer, Scope2* inner)
    {
        while (inner)
        {
            if (inner == outer)
                return true;
            inner = inner->parent;
        }

        return false;
    }

    bool visit(TypeId ty, const FreeTypeVar& ftv) override
    {
        seenMutableType = true;

        if (FFlag::DebugLuauDeferredConstraintResolution ? !subsumes(scope, ftv.scope) : !level.subsumes(ftv.level))
            return false;

        if (FFlag::DebugLuauDeferredConstraintResolution)
            *asMutable(ty) = GenericTypeVar{scope};
        else
            *asMutable(ty) = GenericTypeVar{level};

        generics.push_back(ty);

        return false;
    }

    bool visit(TypeId ty, const ConstrainedTypeVar&) override
    {
        if (FFlag::LuauQuantifyConstrained)
        {
            ConstrainedTypeVar* ctv = getMutable<ConstrainedTypeVar>(ty);

            seenMutableType = true;

            if (FFlag::DebugLuauDeferredConstraintResolution ? !subsumes(scope, ctv->scope) : !level.subsumes(ctv->level))
                return false;

            std::vector<TypeId> opts = std::move(ctv->parts);

            // We might transmute, so it's not safe to rely on the builtin traversal logic
            for (TypeId opt : opts)
                traverse(opt);

            if (opts.size() == 1)
                *asMutable(ty) = BoundTypeVar{opts[0]};
            else
                *asMutable(ty) = UnionTypeVar{std::move(opts)};

            return false;
        }
        else
            return true;
    }

    bool visit(TypeId ty, const TableTypeVar&) override
    {
        LUAU_ASSERT(getMutable<TableTypeVar>(ty));
        TableTypeVar& ttv = *getMutable<TableTypeVar>(ty);

        if (ttv.state == TableState::Generic)
            seenGenericType = true;

        if (ttv.state == TableState::Free)
            seenMutableType = true;

        if (!FFlag::LuauQuantifyConstrained)
        {
            if (ttv.state == TableState::Sealed || ttv.state == TableState::Generic)
                return false;
        }

        if (FFlag::DebugLuauDeferredConstraintResolution ? !subsumes(scope, ttv.scope) : !level.subsumes(ttv.level))
        {
            if (ttv.state == TableState::Unsealed)
                seenMutableType = true;
            return false;
        }

        if (ttv.state == TableState::Free)
        {
            ttv.state = TableState::Generic;
            seenGenericType = true;
        }
        else if (ttv.state == TableState::Unsealed)
            ttv.state = TableState::Sealed;

        ttv.level = level;

        return true;
    }

    bool visit(TypePackId tp, const FreeTypePack& ftp) override
    {
        seenMutableType = true;

        if (FFlag::DebugLuauDeferredConstraintResolution ? !subsumes(scope, ftp.scope) : !level.subsumes(ftp.level))
            return false;

        *asMutable(tp) = GenericTypePack{level};
        genericPacks.push_back(tp);
        return true;
    }
};

void quantify(TypeId ty, TypeLevel level)
{
    Quantifier q{level};
    q.traverse(ty);

    FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(ty);
    LUAU_ASSERT(ftv);
    if (FFlag::LuauAlwaysQuantify)
    {
        ftv->generics.insert(ftv->generics.end(), q.generics.begin(), q.generics.end());
        ftv->genericPacks.insert(ftv->genericPacks.end(), q.genericPacks.begin(), q.genericPacks.end());
    }
    else
    {
        ftv->generics = q.generics;
        ftv->genericPacks = q.genericPacks;
    }

    if (ftv->generics.empty() && ftv->genericPacks.empty() && !q.seenMutableType && !q.seenGenericType)
        ftv->hasNoGenerics = true;
}

void quantify(TypeId ty, Scope2* scope)
{
    Quantifier q{scope};
    q.traverse(ty);

    FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(ty);
    LUAU_ASSERT(ftv);
    if (FFlag::LuauAlwaysQuantify)
    {
        ftv->generics.insert(ftv->generics.end(), q.generics.begin(), q.generics.end());
        ftv->genericPacks.insert(ftv->genericPacks.end(), q.genericPacks.begin(), q.genericPacks.end());
    }
    else
    {
        ftv->generics = q.generics;
        ftv->genericPacks = q.genericPacks;
    }

    if (ftv->generics.empty() && ftv->genericPacks.empty() && !q.seenMutableType && !q.seenGenericType)
        ftv->hasNoGenerics = true;
}

struct PureQuantifier : Substitution
{
    Scope2* scope;
    std::vector<TypeId> insertedGenerics;
    std::vector<TypePackId> insertedGenericPacks;

    PureQuantifier(const TxnLog* log, TypeArena* arena, Scope2* scope)
        : Substitution(log, arena)
        , scope(scope)
    {
    }

    bool isDirty(TypeId ty) override
    {
        LUAU_ASSERT(ty == follow(ty));

        if (auto ftv = get<FreeTypeVar>(ty))
        {
            return subsumes(scope, ftv->scope);
        }
        else if (auto ttv = get<TableTypeVar>(ty))
        {
            return ttv->state == TableState::Free && subsumes(scope, ttv->scope);
        }

        return false;
    }

    bool isDirty(TypePackId tp) override
    {
        if (auto ftp = get<FreeTypePack>(tp))
        {
            return subsumes(scope, ftp->scope);
        }

        return false;
    }

    TypeId clean(TypeId ty) override
    {
        if (auto ftv = get<FreeTypeVar>(ty))
        {
            TypeId result = arena->addType(GenericTypeVar{});
            insertedGenerics.push_back(result);
            return result;
        }
        else if (auto ttv = get<TableTypeVar>(ty))
        {
            TypeId result = arena->addType(TableTypeVar{});
            TableTypeVar* resultTable = getMutable<TableTypeVar>(result);
            LUAU_ASSERT(resultTable);

            *resultTable = *ttv;
            resultTable->scope = nullptr;
            resultTable->state = TableState::Generic;

            return result;
        }

        return ty;
    }

    TypePackId clean(TypePackId tp) override
    {
        if (auto ftp = get<FreeTypePack>(tp))
        {
            TypePackId result = arena->addTypePack(TypePackVar{GenericTypePack{}});
            insertedGenericPacks.push_back(result);
            return result;
        }

        return tp;
    }

    bool ignoreChildren(TypeId ty) override
    {
        return ty->persistent;
    }
    bool ignoreChildren(TypePackId ty) override
    {
        return ty->persistent;
    }
};

TypeId quantify(TypeArena* arena, TypeId ty, Scope2* scope)
{
    PureQuantifier quantifier{TxnLog::empty(), arena, scope};
    std::optional<TypeId> result = quantifier.substitute(ty);
    LUAU_ASSERT(result);

    FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(*result);
    LUAU_ASSERT(ftv);
    ftv->generics.insert(ftv->generics.end(), quantifier.insertedGenerics.begin(), quantifier.insertedGenerics.end());
    ftv->genericPacks.insert(ftv->genericPacks.end(), quantifier.insertedGenericPacks.begin(), quantifier.insertedGenericPacks.end());

    // TODO: Set hasNoGenerics.

    return *result;
}

} // namespace Luau
