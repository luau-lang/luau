// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Quantify.h"

#include "Luau/VisitTypeVar.h"

LUAU_FASTFLAG(LuauAlwaysQuantify)

namespace Luau
{

struct Quantifier final : TypeVarOnceVisitor
{
    TypeLevel level;
    std::vector<TypeId> generics;
    std::vector<TypePackId> genericPacks;
    bool seenGenericType = false;
    bool seenMutableType = false;

    explicit Quantifier(TypeLevel level)
        : level(level)
    {
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

    bool visit(TypeId ty, const FreeTypeVar& ftv) override
    {
        seenMutableType = true;

        if (!level.subsumes(ftv.level))
            return false;

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
        if (!level.subsumes(ttv.level))
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

        if (!level.subsumes(ftp.level))
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

} // namespace Luau
