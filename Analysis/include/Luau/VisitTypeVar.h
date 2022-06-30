// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include <unordered_set>

#include "Luau/DenseHash.h"
#include "Luau/RecursionCounter.h"
#include "Luau/TypePack.h"
#include "Luau/TypeVar.h"

LUAU_FASTINT(LuauVisitRecursionLimit)
LUAU_FASTFLAG(LuauNormalizeFlagIsConservative)

namespace Luau
{

namespace visit_detail
{
/**
 * Apply f(tid, t, seen) if doing so would pass type checking, else apply f(tid, t)
 *
 * We do this to permit (but not require) TypeVar visitors to accept the seen set as an argument.
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
    // When DenseHashSet is used for 'visitTypeVarOnce', where don't forget visited elements
}

} // namespace visit_detail

template<typename S>
struct GenericTypeVarVisitor
{
    using Set = S;

    Set seen;
    int recursionCounter = 0;

    GenericTypeVarVisitor() = default;

    explicit GenericTypeVarVisitor(Set seen)
        : seen(std::move(seen))
    {
    }

    virtual void cycle(TypeId) {}
    virtual void cycle(TypePackId) {}

    virtual bool visit(TypeId ty)
    {
        return true;
    }
    virtual bool visit(TypeId ty, const BoundTypeVar& btv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const FreeTypeVar& ftv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const GenericTypeVar& gtv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const ErrorTypeVar& etv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const ConstrainedTypeVar& ctv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const PrimitiveTypeVar& ptv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const FunctionTypeVar& ftv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const TableTypeVar& ttv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const MetatableTypeVar& mtv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const ClassTypeVar& ctv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const AnyTypeVar& atv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const UnionTypeVar& utv)
    {
        return visit(ty);
    }
    virtual bool visit(TypeId ty, const IntersectionTypeVar& itv)
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

    void traverse(TypeId ty)
    {
        RecursionLimiter limiter{&recursionCounter, FInt::LuauVisitRecursionLimit};

        if (visit_detail::hasSeen(seen, ty))
        {
            cycle(ty);
            return;
        }

        if (auto btv = get<BoundTypeVar>(ty))
        {
            if (visit(ty, *btv))
                traverse(btv->boundTo);
        }

        else if (auto ftv = get<FreeTypeVar>(ty))
            visit(ty, *ftv);

        else if (auto gtv = get<GenericTypeVar>(ty))
            visit(ty, *gtv);

        else if (auto etv = get<ErrorTypeVar>(ty))
            visit(ty, *etv);

        else if (auto ctv = get<ConstrainedTypeVar>(ty))
        {
            if (visit(ty, *ctv))
            {
                for (TypeId part : ctv->parts)
                    traverse(part);
            }
        }

        else if (auto ptv = get<PrimitiveTypeVar>(ty))
            visit(ty, *ptv);

        else if (auto ftv = get<FunctionTypeVar>(ty))
        {
            if (visit(ty, *ftv))
            {
                traverse(ftv->argTypes);
                traverse(ftv->retTypes);
            }
        }

        else if (auto ttv = get<TableTypeVar>(ty))
        {
            // Some visitors want to see bound tables, that's why we traverse the original type
            if (visit(ty, *ttv))
            {
                if (ttv->boundTo)
                {
                    traverse(*ttv->boundTo);
                }
                else
                {
                    for (auto& [_name, prop] : ttv->props)
                        traverse(prop.type);

                    if (ttv->indexer)
                    {
                        traverse(ttv->indexer->indexType);
                        traverse(ttv->indexer->indexResultType);
                    }
                }
            }
        }

        else if (auto mtv = get<MetatableTypeVar>(ty))
        {
            if (visit(ty, *mtv))
            {
                traverse(mtv->table);
                traverse(mtv->metatable);
            }
        }

        else if (auto ctv = get<ClassTypeVar>(ty))
        {
            if (visit(ty, *ctv))
            {
                for (const auto& [name, prop] : ctv->props)
                    traverse(prop.type);

                if (ctv->parent)
                    traverse(*ctv->parent);

                if (ctv->metatable)
                    traverse(*ctv->metatable);
            }
        }

        else if (auto atv = get<AnyTypeVar>(ty))
            visit(ty, *atv);

        else if (auto utv = get<UnionTypeVar>(ty))
        {
            if (visit(ty, *utv))
            {
                for (TypeId optTy : utv->options)
                    traverse(optTy);
            }
        }

        else if (auto itv = get<IntersectionTypeVar>(ty))
        {
            if (visit(ty, *itv))
            {
                for (TypeId partTy : itv->parts)
                    traverse(partTy);
            }
        }

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

        else if (auto ftv = get<Unifiable::Free>(tp))
            visit(tp, *ftv);

        else if (auto gtv = get<Unifiable::Generic>(tp))
            visit(tp, *gtv);

        else if (auto etv = get<Unifiable::Error>(tp))
            visit(tp, *etv);

        else if (auto pack = get<TypePack>(tp))
        {
            bool res = visit(tp, *pack);
            if (!FFlag::LuauNormalizeFlagIsConservative || res)
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
            if (!FFlag::LuauNormalizeFlagIsConservative || res)
                traverse(pack->ty);
        }
        else
            LUAU_ASSERT(!"GenericTypeVarVisitor::traverse(TypePackId) is not exhaustive!");

        visit_detail::unsee(seen, tp);
    }
};

/** Visit each type under a given type.  Skips over cycles and keeps recursion depth under control.
 *
 * The same type may be visited multiple times if there are multiple distinct paths to it.  If this is undesirable, use
 * TypeVarOnceVisitor.
 */
struct TypeVarVisitor : GenericTypeVarVisitor<std::unordered_set<void*>>
{
};

/// Visit each type under a given type.  Each type will only be checked once even if there are multiple paths to it.
struct TypeVarOnceVisitor : GenericTypeVarVisitor<DenseHashSet<void*>>
{
    TypeVarOnceVisitor()
        : GenericTypeVarVisitor{DenseHashSet<void*>{nullptr}}
    {
    }
};

} // namespace Luau
