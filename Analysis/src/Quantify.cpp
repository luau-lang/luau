// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details

#include "lluz/Quantify.h"

#include "lluz/Scope.h"
#include "lluz/Substitution.h"
#include "lluz/TxnLog.h"
#include "lluz/VisitTypeVar.h"

lluz_FASTFLAG(LluAlwaysQuantify);
lluz_FASTFLAG(DebugLluSharedSelf)
lluz_FASTFLAG(DebugLluDeferredConstraintResolution);
lluz_FASTFLAGVARIABLE(LluQuantifyConstrained, false)

namespace lluz
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
        lluz_ASSERT(!FFlag::DebugLluDeferredConstraintResolution);
    }

    explicit Quantifier(Scope2* scope)
        : scope(scope)
    {
        lluz_ASSERT(FFlag::DebugLluDeferredConstraintResolution);
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

        if (FFlag::DebugLluDeferredConstraintResolution ? !subsumes(scope, ftv.scope) : !level.subsumes(ftv.level))
            return false;

        if (FFlag::DebugLluDeferredConstraintResolution)
            *asMutable(ty) = GenericTypeVar{scope};
        else
            *asMutable(ty) = GenericTypeVar{level};

        generics.push_back(ty);

        return false;
    }

    bool visit(TypeId ty, const ConstrainedTypeVar&) override
    {
        if (FFlag::LluQuantifyConstrained)
        {
            ConstrainedTypeVar* ctv = getMutable<ConstrainedTypeVar>(ty);

            seenMutableType = true;

            if (FFlag::DebugLluDeferredConstraintResolution ? !subsumes(scope, ctv->scope) : !level.subsumes(ctv->level))
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
        lluz_ASSERT(getMutable<TableTypeVar>(ty));
        TableTypeVar& ttv = *getMutable<TableTypeVar>(ty);

        if (ttv.state == TableState::Generic)
            seenGenericType = true;

        if (ttv.state == TableState::Free)
            seenMutableType = true;

        if (!FFlag::LluQuantifyConstrained)
        {
            if (ttv.state == TableState::Sealed || ttv.state == TableState::Generic)
                return false;
        }

        if (FFlag::DebugLluDeferredConstraintResolution ? !subsumes(scope, ttv.scope) : !level.subsumes(ttv.level))
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

        if (FFlag::DebugLluDeferredConstraintResolution ? !subsumes(scope, ftp.scope) : !level.subsumes(ftp.level))
            return false;

        *asMutable(tp) = GenericTypePack{level};
        genericPacks.push_back(tp);
        return true;
    }
};

void quantify(TypeId ty, TypeLevel level)
{
    if (FFlag::DebugLluSharedSelf)
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

                if (lluz::first(ftv->argTypes) == ttv->selfTy)
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
        lluz_ASSERT(ftv);
        if (FFlag::LluAlwaysQuantify)
        {
            ftv->generics.insert(ftv->generics.end(), q.generics.begin(), q.generics.end());
            ftv->genericPacks.insert(ftv->genericPacks.end(), q.genericPacks.begin(), q.genericPacks.end());
        }
        else
        {
            ftv->generics = q.generics;
            ftv->genericPacks = q.genericPacks;
        }
    }
}

void quantify(TypeId ty, Scope2* scope)
{
    Quantifier q{scope};
    q.traverse(ty);

    FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(ty);
    lluz_ASSERT(ftv);
    if (FFlag::LluAlwaysQuantify)
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

    PureQuantifier(TypeArena* arena, Scope2* scope)
        : Substitution(TxnLog::empty(), arena)
        , scope(scope)
    {
    }

    bool isDirty(TypeId ty) override
    {
        lluz_ASSERT(ty == follow(ty));

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
            lluz_ASSERT(resultTable);

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
    PureQuantifier quantifier{arena, scope};
    std::optional<TypeId> result = quantifier.substitute(ty);
    lluz_ASSERT(result);

    FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(*result);
    lluz_ASSERT(ftv);
    ftv->generics.insert(ftv->generics.end(), quantifier.insertedGenerics.begin(), quantifier.insertedGenerics.end());
    ftv->genericPacks.insert(ftv->genericPacks.end(), quantifier.insertedGenericPacks.begin(), quantifier.insertedGenericPacks.end());
    ftv->hasNoGenerics = ftv->generics.empty() && ftv->genericPacks.empty();

    return *result;
}

} // namespace lluz
