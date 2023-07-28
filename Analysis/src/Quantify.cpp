// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Quantify.h"

#include "Luau/Scope.h"
#include "Luau/Substitution.h"
#include "Luau/TxnLog.h"
#include "Luau/Type.h"
#include "Luau/VisitType.h"

LUAU_FASTFLAG(DebugLuauSharedSelf)

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
        : level(level)
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
    if (FFlag::DebugLuauSharedSelf)
    {
        ty = follow(ty);

        if (auto ttv = getTableType(ty); ttv && ttv->selfTy)
        {
            Quantifier selfQ{level};
            selfQ.traverse(*ttv->selfTy);

            Quantifier q{level};
            q.traverse(ty);

            for (const auto& [_, prop] : ttv->props)
            {
                auto ftv = getMutable<FunctionType>(follow(prop.type()));
                if (!ftv || !ftv->hasSelf)
                    continue;

                if (Luau::first(ftv->argTypes) == ttv->selfTy)
                {
                    ftv->generics.insert(ftv->generics.end(), selfQ.generics.begin(), selfQ.generics.end());
                    ftv->genericPacks.insert(ftv->genericPacks.end(), selfQ.genericPacks.begin(), selfQ.genericPacks.end());
                }
            }
        }
        else if (auto ftv = getMutable<FunctionType>(ty))
        {
            Quantifier q{level};
            q.traverse(ty);

            ftv->generics.insert(ftv->generics.end(), q.generics.begin(), q.generics.end());
            ftv->genericPacks.insert(ftv->genericPacks.end(), q.genericPacks.begin(), q.genericPacks.end());

            if (ftv->generics.empty() && ftv->genericPacks.empty() && !q.seenMutableType && !q.seenGenericType)
                ftv->hasNoFreeOrGenericTypes = true;
        }
    }
    else
    {
        Quantifier q{level};
        q.traverse(ty);

        FunctionType* ftv = getMutable<FunctionType>(ty);
        LUAU_ASSERT(ftv);
        ftv->generics.insert(ftv->generics.end(), q.generics.begin(), q.generics.end());
        ftv->genericPacks.insert(ftv->genericPacks.end(), q.genericPacks.begin(), q.genericPacks.end());
    }
}

struct PureQuantifier : Substitution
{
    Scope* scope;
    OrderedMap<TypeId, TypeId> insertedGenerics;
    OrderedMap<TypePackId, TypePackId> insertedGenericPacks;
    bool seenMutableType = false;
    bool seenGenericType = false;

    PureQuantifier(TypeArena* arena, Scope* scope)
        : Substitution(TxnLog::empty(), arena)
        , scope(scope)
    {
    }

    bool isDirty(TypeId ty) override
    {
        LUAU_ASSERT(ty == follow(ty));

        if (auto ftv = get<FreeType>(ty))
        {
            bool result = subsumes(scope, ftv->scope);
            seenMutableType |= result;
            return result;
        }
        else if (auto ttv = get<TableType>(ty))
        {
            if (ttv->state == TableState::Free)
                seenMutableType = true;
            else if (ttv->state == TableState::Generic)
                seenGenericType = true;

            return (ttv->state == TableState::Unsealed || ttv->state == TableState::Free) && subsumes(scope, ttv->scope);
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
        if (auto ftv = get<FreeType>(ty))
        {
            TypeId result = arena->addType(GenericType{scope});
            insertedGenerics.push(ty, result);
            return result;
        }
        else if (auto ttv = get<TableType>(ty))
        {
            TypeId result = arena->addType(TableType{});
            TableType* resultTable = getMutable<TableType>(result);
            LUAU_ASSERT(resultTable);

            *resultTable = *ttv;
            resultTable->level = TypeLevel{};
            resultTable->scope = scope;

            if (ttv->state == TableState::Free)
            {
                resultTable->state = TableState::Generic;
                insertedGenerics.push(ty, result);
            }
            else if (ttv->state == TableState::Unsealed)
                resultTable->state = TableState::Sealed;

            return result;
        }

        return ty;
    }

    TypePackId clean(TypePackId tp) override
    {
        if (auto ftp = get<FreeTypePack>(tp))
        {
            TypePackId result = arena->addTypePack(TypePackVar{GenericTypePack{scope}});
            insertedGenericPacks.push(tp, result);
            return result;
        }

        return tp;
    }

    bool ignoreChildren(TypeId ty) override
    {
        if (get<ClassType>(ty))
            return true;

        return ty->persistent;
    }
    bool ignoreChildren(TypePackId ty) override
    {
        return ty->persistent;
    }
};

std::optional<QuantifierResult> quantify(TypeArena* arena, TypeId ty, Scope* scope)
{
    PureQuantifier quantifier{arena, scope};
    std::optional<TypeId> result = quantifier.substitute(ty);
    if (!result)
        return std::nullopt;

    FunctionType* ftv = getMutable<FunctionType>(*result);
    LUAU_ASSERT(ftv);
    ftv->scope = scope;

    for (auto k : quantifier.insertedGenerics.keys)
    {
        TypeId g = quantifier.insertedGenerics.pairings[k];
        if (get<GenericType>(g))
            ftv->generics.push_back(g);
    }

    for (auto k : quantifier.insertedGenericPacks.keys)
        ftv->genericPacks.push_back(quantifier.insertedGenericPacks.pairings[k]);

    ftv->hasNoFreeOrGenericTypes = ftv->generics.empty() && ftv->genericPacks.empty() && !quantifier.seenGenericType && !quantifier.seenMutableType;

    return std::optional<QuantifierResult>({*result, std::move(quantifier.insertedGenerics), std::move(quantifier.insertedGenericPacks)});
}

} // namespace Luau
