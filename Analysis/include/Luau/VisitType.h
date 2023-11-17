// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <unordered_set>

#include "Luau/DenseHash.h"
#include "Luau/RecursionCounter.h"
#include "Luau/TypePack.h"
#include "Luau/Type.h"

LUAU_FASTINT(LuauVisitRecursionLimit)
LUAU_FASTFLAG(LuauBoundLazyTypes2)
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)
LUAU_FASTFLAG(DebugLuauReadWriteProperties)

namespace Luau
{

namespace visit_detail
{
/**
 * Apply f(tid, t, seen) if doing so would pass type checking, else apply f(tid, t)
 *
 * We do this to permit (but not require) Type visitors to accept the seen set as an argument.
 */
template<typename F, typename A, typename B, typename C>
auto apply(A tid, const B& t, C& c, F& f) -> decltype(f(tid, t, c))
{
    return f(tid, t, c);
}

template<typename A, typename B, typename C, typename F>
auto apply(A tid, const B& t, C&, F& f) -> decltype(f(tid, t))
{
    return f(tid, t);
}

inline bool hasSeen(std::unordered_set<void*>& seen, const void* tv)
{
    void* ttv = const_cast<void*>(tv);
    return !seen.insert(ttv).second;
}

inline bool hasSeen(DenseHashSet<void*>& seen, const void* tv)
{
    void* ttv = const_cast<void*>(tv);

    if (seen.contains(ttv))
        return true;

    seen.insert(ttv);
    return false;
}

inline void unsee(std::unordered_set<void*>& seen, const void* tv)
{
    void* ttv = const_cast<void*>(tv);
    seen.erase(ttv);
}

inline void unsee(DenseHashSet<void*>& seen, const void* tv)
{
    // When DenseHashSet is used for 'visitTypeOnce', where don't forget visited elements
}

} // namespace visit_detail

template<typename S>
struct GenericTypeVisitor
{
    using Set = S;

    Set seen;
    bool skipBoundTypes = false;
    int recursionCounter = 0;

    GenericTypeVisitor() = default;

    explicit GenericTypeVisitor(Set seen, bool skipBoundTypes = false)
        : seen(std::move(seen))
        , skipBoundTypes(skipBoundTypes)
    {
    }

    virtual void cycle(TypeId) {}
    virtual void cycle(TypePackId) {}

    virtual bool visit(TypeId ty)
    {
        return true;
    }
    virtual bool visit(TypeId ty, const BoundType& btv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const FreeType& ftv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const LocalType& ftv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const GenericType& gtv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const ErrorType& etv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const PrimitiveType& ptv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const FunctionType& ftv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const TableType& ttv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const MetatableType& mtv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const ClassType& ctv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const AnyType& atv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const UnknownType& utv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const NeverType& ntv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const UnionType& utv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const IntersectionType& itv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const BlockedType& btv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const PendingExpansionType& petv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const SingletonType& stv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const NegationType& ntv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const TypeFamilyInstanceType& tfit)
    {
        return visit(ty);
    }

    virtual bool visit(TypePackId tp)
    {
        return true;
    }
    virtual bool visit(TypePackId tp, const BoundTypePack& btp)
    {
        return visit(tp);
    }
    virtual bool visit(TypePackId tp, const FreeTypePack& ftp)
    {
        return visit(tp);
    }
    virtual bool visit(TypePackId tp, const GenericTypePack& gtp)
    {
        return visit(tp);
    }
    virtual bool visit(TypePackId tp, const Unifiable::Error& etp)
    {
        return visit(tp);
    }
    virtual bool visit(TypePackId tp, const TypePack& pack)
    {
        return visit(tp);
    }
    virtual bool visit(TypePackId tp, const VariadicTypePack& vtp)
    {
        return visit(tp);
    }
    virtual bool visit(TypePackId tp, const BlockedTypePack& btp)
    {
        return visit(tp);
    }
    virtual bool visit(TypePackId tp, const TypeFamilyInstanceTypePack& tfitp)
    {
        return visit(tp);
    }

    void traverse(TypeId ty)
    {
        RecursionLimiter limiter{&recursionCounter, FInt::LuauVisitRecursionLimit};

        if (visit_detail::hasSeen(seen, ty))
        {
            cycle(ty);
            return;
        }

        if (auto btv = get<BoundType>(ty))
        {
            if (skipBoundTypes)
                traverse(btv->boundTo);
            else if (visit(ty, *btv))
                traverse(btv->boundTo);
        }
        else if (auto ftv = get<FreeType>(ty))
        {
            if (FFlag::DebugLuauDeferredConstraintResolution)
            {
                if (visit(ty, *ftv))
                {
                    // TODO: Replace these if statements with assert()s when we
                    // delete FFlag::DebugLuauDeferredConstraintResolution.
                    //
                    // When the old solver is used, these pointers are always
                    // unused. When the new solver is used, they are never null.
                    if (ftv->lowerBound)
                        traverse(ftv->lowerBound);

                    if (ftv->upperBound)
                        traverse(ftv->upperBound);
                }
            }
            else
                visit(ty, *ftv);
        }
        else if (auto lt = get<LocalType>(ty))
        {
            if (visit(ty, *lt))
                traverse(lt->domain);
        }
        else if (auto gtv = get<GenericType>(ty))
            visit(ty, *gtv);
        else if (auto etv = get<ErrorType>(ty))
            visit(ty, *etv);
        else if (auto ptv = get<PrimitiveType>(ty))
            visit(ty, *ptv);
        else if (auto ftv = get<FunctionType>(ty))
        {
            if (visit(ty, *ftv))
            {
                traverse(ftv->argTypes);
                traverse(ftv->retTypes);
            }
        }
        else if (auto ttv = get<TableType>(ty))
        {
            // Some visitors want to see bound tables, that's why we traverse the original type
            if (skipBoundTypes && ttv->boundTo)
            {
                traverse(*ttv->boundTo);
            }
            else if (visit(ty, *ttv))
            {
                if (ttv->boundTo)
                {
                    traverse(*ttv->boundTo);
                }
                else
                {
                    for (auto& [_name, prop] : ttv->props)
                    {
                        if (FFlag::DebugLuauReadWriteProperties)
                        {
                            if (auto ty = prop.readType())
                                traverse(*ty);

                            if (auto ty = prop.writeType())
                                traverse(*ty);
                        }
                        else
                            traverse(prop.type());
                    }

                    if (ttv->indexer)
                    {
                        traverse(ttv->indexer->indexType);
                        traverse(ttv->indexer->indexResultType);
                    }
                }
            }
        }
        else if (auto mtv = get<MetatableType>(ty))
        {
            if (visit(ty, *mtv))
            {
                traverse(mtv->table);
                traverse(mtv->metatable);
            }
        }
        else if (auto ctv = get<ClassType>(ty))
        {
            if (visit(ty, *ctv))
            {
                for (const auto& [name, prop] : ctv->props)
                {
                    if (FFlag::DebugLuauReadWriteProperties)
                    {
                        if (auto ty = prop.readType())
                            traverse(*ty);

                        if (auto ty = prop.writeType())
                            traverse(*ty);
                    }
                    else
                        traverse(prop.type());
                }

                if (ctv->parent)
                    traverse(*ctv->parent);

                if (ctv->metatable)
                    traverse(*ctv->metatable);

                if (ctv->indexer)
                {
                    traverse(ctv->indexer->indexType);
                    traverse(ctv->indexer->indexResultType);
                }
            }
        }
        else if (auto atv = get<AnyType>(ty))
            visit(ty, *atv);
        else if (auto utv = get<UnionType>(ty))
        {
            if (visit(ty, *utv))
            {
                for (TypeId optTy : utv->options)
                    traverse(optTy);
            }
        }
        else if (auto itv = get<IntersectionType>(ty))
        {
            if (visit(ty, *itv))
            {
                for (TypeId partTy : itv->parts)
                    traverse(partTy);
            }
        }
        else if (auto ltv = get<LazyType>(ty))
        {
            if (TypeId unwrapped = ltv->unwrapped)
                traverse(unwrapped);

            // Visiting into LazyType that hasn't been unwrapped may necessarily cause infinite expansion, so we don't do that on purpose.
            // Asserting also makes no sense, because the type _will_ happen here, most likely as a property of some ClassType
            // that doesn't need to be expanded.
        }
        else if (auto stv = get<SingletonType>(ty))
            visit(ty, *stv);
        else if (auto btv = get<BlockedType>(ty))
            visit(ty, *btv);
        else if (auto utv = get<UnknownType>(ty))
            visit(ty, *utv);
        else if (auto ntv = get<NeverType>(ty))
            visit(ty, *ntv);
        else if (auto petv = get<PendingExpansionType>(ty))
        {
            if (visit(ty, *petv))
            {
                for (TypeId a : petv->typeArguments)
                    traverse(a);

                for (TypePackId a : petv->packArguments)
                    traverse(a);
            }
        }
        else if (auto ntv = get<NegationType>(ty))
        {
            if (visit(ty, *ntv))
                traverse(ntv->ty);
        }
        else if (auto tfit = get<TypeFamilyInstanceType>(ty))
        {
            if (visit(ty, *tfit))
            {
                for (TypeId p : tfit->typeArguments)
                    traverse(p);

                for (TypePackId p : tfit->packArguments)
                    traverse(p);
            }
        }
        else
            LUAU_ASSERT(!"GenericTypeVisitor::traverse(TypeId) is not exhaustive!");

        visit_detail::unsee(seen, ty);
    }

    void traverse(TypePackId tp)
    {
        if (visit_detail::hasSeen(seen, tp))
        {
            cycle(tp);
            return;
        }

        if (auto btv = get<BoundTypePack>(tp))
        {
            if (visit(tp, *btv))
                traverse(btv->boundTo);
        }

        else if (auto ftv = get<FreeTypePack>(tp))
            visit(tp, *ftv);

        else if (auto gtv = get<GenericTypePack>(tp))
            visit(tp, *gtv);

        else if (auto etv = get<Unifiable::Error>(tp))
            visit(tp, *etv);

        else if (auto pack = get<TypePack>(tp))
        {
            bool res = visit(tp, *pack);
            if (res)
            {
                for (TypeId ty : pack->head)
                    traverse(ty);

                if (pack->tail)
                    traverse(*pack->tail);
            }
        }
        else if (auto pack = get<VariadicTypePack>(tp))
        {
            bool res = visit(tp, *pack);
            if (res)
                traverse(pack->ty);
        }
        else if (auto btp = get<BlockedTypePack>(tp))
            visit(tp, *btp);
        else if (auto tfitp = get<TypeFamilyInstanceTypePack>(tp))
        {
            if (visit(tp, *tfitp))
            {
                for (TypeId t : tfitp->typeArguments)
                    traverse(t);

                for (TypePackId t : tfitp->packArguments)
                    traverse(t);
            }
        }

        else
            LUAU_ASSERT(!"GenericTypeVisitor::traverse(TypePackId) is not exhaustive!");

        visit_detail::unsee(seen, tp);
    }
};

/** Visit each type under a given type.  Skips over cycles and keeps recursion depth under control.
 *
 * The same type may be visited multiple times if there are multiple distinct paths to it.  If this is undesirable, use
 * TypeOnceVisitor.
 */
struct TypeVisitor : GenericTypeVisitor<std::unordered_set<void*>>
{
    explicit TypeVisitor(bool skipBoundTypes = false)
        : GenericTypeVisitor{{}, skipBoundTypes}
    {
    }
};

/// Visit each type under a given type.  Each type will only be checked once even if there are multiple paths to it.
struct TypeOnceVisitor : GenericTypeVisitor<DenseHashSet<void*>>
{
    explicit TypeOnceVisitor(bool skipBoundTypes = false)
        : GenericTypeVisitor{DenseHashSet<void*>{nullptr}, skipBoundTypes}
    {
    }
};

} // namespace Luau
