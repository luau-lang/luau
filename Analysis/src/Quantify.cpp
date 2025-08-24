// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Quantify.h"

#include "Luau/Scope.h"
#include "Luau/Substitution.h"
#include "Luau/TxnLog.h"
#include "Luau/Type.h"
#include "Luau/VisitType.h"

LUAU_FASTFLAG(LuauExplicitSkipBoundTypes)

namespace Luau
{

struct Quantifier final : TypeOnceVisitor
{
    TypeLevel level;
    std::vector<TypeId> generics;
    std::vector<TypePackId> genericPacks;
    Scope* scope = nullptr;
    bool seenGenericType = false;
    bool seenMutableType = false;

    explicit Quantifier(TypeLevel level)
        : TypeOnceVisitor("Quantifier", /* skipBoundTypes */ false)
        , level(level)
    {
    }

    /// @return true if outer encloses inner
    bool subsumes(Scope* outer, Scope* inner)
    {
        while (inner)
        {
            if (inner == outer)
                return true;
            inner = inner->parent.get();
        }

        return false;
    }

    bool visit(TypeId ty, const FreeType& ftv) override
    {
        seenMutableType = true;

        if (!level.subsumes(ftv.level))
            return false;

        *asMutable(ty) = GenericType{level};

        generics.push_back(ty);

        return false;
    }

    bool visit(TypeId ty, const TableType&) override
    {
        LUAU_ASSERT(getMutable<TableType>(ty));
        TableType& ttv = *getMutable<TableType>(ty);

        if (ttv.state == TableState::Generic)
            seenGenericType = true;

        if (ttv.state == TableState::Free)
            seenMutableType = true;

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

    FunctionType* ftv = getMutable<FunctionType>(ty);
    LUAU_ASSERT(ftv);
    ftv->generics.insert(ftv->generics.end(), q.generics.begin(), q.generics.end());
    ftv->genericPacks.insert(ftv->genericPacks.end(), q.genericPacks.begin(), q.genericPacks.end());
}

} // namespace Luau
