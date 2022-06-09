// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Quantify.h"

#include "Luau/VisitTypeVar.h"
#include "Luau/ConstraintGraphBuilder.h" // TODO for Scope2; move to separate header

LUAU_FASTFLAG(LuauAlwaysQuantify);
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);

namespace Luau
{

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

    void cycle(TypeId) override {}
    void cycle(TypePackId) override {}

    bool operator()(TypeId ty, const FreeTypeVar& ftv)
    {
        return visit(ty, ftv);
    }

    template<typename T>
    bool operator()(TypeId ty, const T& t)
    {
        return true;
    }

    template<typename T>
    bool operator()(TypePackId, const T&)
    {
        return true;
    }

    bool operator()(TypeId ty, const ConstrainedTypeVar&)
    {
        return true;
    }

    bool operator()(TypeId ty, const TableTypeVar& ttv)
    {
        return visit(ty, ttv);
    }

    bool operator()(TypePackId tp, const FreeTypePack& ftp)
    {
        return visit(tp, ftp);
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

    bool visit(TypeId ty, const TableTypeVar&) override
    {
        LUAU_ASSERT(getMutable<TableTypeVar>(ty));
        TableTypeVar& ttv = *getMutable<TableTypeVar>(ty);

        if (ttv.state == TableState::Generic)
            seenGenericType = true;

        if (ttv.state == TableState::Free)
            seenMutableType = true;

        if (ttv.state == TableState::Sealed || ttv.state == TableState::Generic)
            return false;
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

    ftv->generalized = true;
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

    ftv->generalized = true;
}

} // namespace Luau
