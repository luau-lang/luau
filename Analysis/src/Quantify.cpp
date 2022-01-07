// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Quantify.h"

#include "Luau/VisitTypeVar.h"

namespace Luau
{

struct Quantifier
{
    ModulePtr module;
    TypeLevel level;
    std::vector<TypeId> generics;
    std::vector<TypePackId> genericPacks;

    Quantifier(ModulePtr module, TypeLevel level)
        : module(module)
        , level(level)
    {
    }

    void cycle(TypeId) {}
    void cycle(TypePackId) {}

    bool operator()(TypeId ty, const FreeTypeVar& ftv)
    {
        if (!level.subsumes(ftv.level))
            return false;

        *asMutable(ty) = GenericTypeVar{level};
        generics.push_back(ty);

        return false;
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

    bool operator()(TypeId ty, const TableTypeVar&)
    {
        TableTypeVar& ttv = *getMutable<TableTypeVar>(ty);

        if (ttv.state == TableState::Sealed || ttv.state == TableState::Generic)
            return false;
        if (!level.subsumes(ttv.level))
            return false;

        if (ttv.state == TableState::Free)
            ttv.state = TableState::Generic;
        else if (ttv.state == TableState::Unsealed)
            ttv.state = TableState::Sealed;

        ttv.level = level;

        return true;
    }

    bool operator()(TypePackId tp, const FreeTypePack& ftp)
    {
        if (!level.subsumes(ftp.level))
            return false;

        *asMutable(tp) = GenericTypePack{level};
        genericPacks.push_back(tp);
        return true;
    }
};

void quantify(ModulePtr module, TypeId ty, TypeLevel level)
{
    Quantifier q{std::move(module), level};
    DenseHashSet<void*> seen{nullptr};
    visitTypeVarOnce(ty, q, seen);

    FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(ty);
    LUAU_ASSERT(ftv);
    ftv->generics = q.generics;
    ftv->genericPacks = q.genericPacks;
}

} // namespace Luau
