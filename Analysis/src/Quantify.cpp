// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/Quantify.h"

#include "Luau/Scope.h"
#include "Luau/Substitution.h"
#include "Luau/TxnLog.h"
#include "Luau/TypeVar.h"
#include "Luau/VisitTypeVar.h"

LUAU_FASTFLAG(DebugLuauSharedSelf)
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);
LUAU_FASTFLAG(LuauClassTypeVarsInSubstitution)

namespace Luau
{

struct Quantifier final : TypeVarOnceVisitor
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
        LUAU_ASSERT(!FFlag::DebugLuauDeferredConstraintResolution);
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

    bool visit(TypeId ty, const FreeTypeVar& ftv) override
    {
        seenMutableType = true;

        if (!level.subsumes(ftv.level))
            return false;

        *asMutable(ty) = GenericTypeVar{level};

        generics.push_back(ty);

        return false;
    }

    bool visit(TypeId ty, const ConstrainedTypeVar&) override
    {
        ConstrainedTypeVar* ctv = getMutable<ConstrainedTypeVar>(ty);

        seenMutableType = true;

        if (!level.subsumes(ctv->level))
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

    bool visit(TypeId ty, const TableTypeVar&) override
    {
        LUAU_ASSERT(getMutable<TableTypeVar>(ty));
        TableTypeVar& ttv = *getMutable<TableTypeVar>(ty);

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
                auto ftv = getMutable<FunctionTypeVar>(follow(prop.type));
                if (!ftv || !ftv->hasSelf)
                    continue;

                if (Luau::first(ftv->argTypes) == ttv->selfTy)
                {
                    ftv->generics.insert(ftv->generics.end(), selfQ.generics.begin(), selfQ.generics.end());
                    ftv->genericPacks.insert(ftv->genericPacks.end(), selfQ.genericPacks.begin(), selfQ.genericPacks.end());
                }
            }
        }
        else if (auto ftv = getMutable<FunctionTypeVar>(ty))
        {
            Quantifier q{level};
            q.traverse(ty);

            ftv->generics.insert(ftv->generics.end(), q.generics.begin(), q.generics.end());
            ftv->genericPacks.insert(ftv->genericPacks.end(), q.genericPacks.begin(), q.genericPacks.end());

            if (ftv->generics.empty() && ftv->genericPacks.empty() && !q.seenMutableType && !q.seenGenericType)
                ftv->hasNoGenerics = true;
        }
    }
    else
    {
        Quantifier q{level};
        q.traverse(ty);

        FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(ty);
        LUAU_ASSERT(ftv);
        ftv->generics.insert(ftv->generics.end(), q.generics.begin(), q.generics.end());
        ftv->genericPacks.insert(ftv->genericPacks.end(), q.genericPacks.begin(), q.genericPacks.end());
    }
}

struct PureQuantifier : Substitution
{
    Scope* scope;
    std::vector<TypeId> insertedGenerics;
    std::vector<TypePackId> insertedGenericPacks;

    PureQuantifier(TypeArena* arena, Scope* scope)
        : Substitution(TxnLog::empty(), arena)
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
            TypeId result = arena->addType(GenericTypeVar{scope});
            insertedGenerics.push_back(result);
            return result;
        }
        else if (auto ttv = get<TableTypeVar>(ty))
        {
            TypeId result = arena->addType(TableTypeVar{});
            TableTypeVar* resultTable = getMutable<TableTypeVar>(result);
            LUAU_ASSERT(resultTable);

            *resultTable = *ttv;
            resultTable->level = TypeLevel{};
            resultTable->scope = scope;
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
        if (FFlag::LuauClassTypeVarsInSubstitution && get<ClassTypeVar>(ty))
            return true;

        return ty->persistent;
    }
    bool ignoreChildren(TypePackId ty) override
    {
        return ty->persistent;
    }
};

TypeId quantify(TypeArena* arena, TypeId ty, Scope* scope)
{
    PureQuantifier quantifier{arena, scope};
    std::optional<TypeId> result = quantifier.substitute(ty);
    LUAU_ASSERT(result);

    FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(*result);
    LUAU_ASSERT(ftv);
    ftv->scope = scope;
    ftv->generics.insert(ftv->generics.end(), quantifier.insertedGenerics.begin(), quantifier.insertedGenerics.end());
    ftv->genericPacks.insert(ftv->genericPacks.end(), quantifier.insertedGenericPacks.begin(), quantifier.insertedGenericPacks.end());
    ftv->hasNoGenerics = ftv->generics.empty() && ftv->genericPacks.empty();

    return *result;
}

} // namespace Luau
