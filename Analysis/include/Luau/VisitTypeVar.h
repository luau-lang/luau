// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/DenseHash.h"
#include "Luau/TypeVar.h"
#include "Luau/TypePack.h"

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

template<typename F, typename Set>
void visit(TypePackId tp, F& f, Set& seen);

template<typename F, typename Set>
void visit(TypeId ty, F& f, Set& seen)
{
    if (visit_detail::hasSeen(seen, ty))
    {
        f.cycle(ty);
        return;
    }

    if (auto btv = get<BoundTypeVar>(ty))
    {
        if (apply(ty, *btv, seen, f))
            visit(btv->boundTo, f, seen);
    }

    else if (auto ftv = get<FreeTypeVar>(ty))
        apply(ty, *ftv, seen, f);

    else if (auto gtv = get<GenericTypeVar>(ty))
        apply(ty, *gtv, seen, f);

    else if (auto etv = get<ErrorTypeVar>(ty))
        apply(ty, *etv, seen, f);

    else if (auto ctv = get<ConstrainedTypeVar>(ty))
    {
        if (apply(ty, *ctv, seen, f))
        {
            for (TypeId part : ctv->parts)
                visit(part, f, seen);
        }
    }

    else if (auto ptv = get<PrimitiveTypeVar>(ty))
        apply(ty, *ptv, seen, f);

    else if (auto ftv = get<FunctionTypeVar>(ty))
    {
        if (apply(ty, *ftv, seen, f))
        {
            visit(ftv->argTypes, f, seen);
            visit(ftv->retType, f, seen);
        }
    }

    else if (auto ttv = get<TableTypeVar>(ty))
    {
        // Some visitors want to see bound tables, that's why we visit the original type
        if (apply(ty, *ttv, seen, f))
        {
            if (ttv->boundTo)
            {
                visit(*ttv->boundTo, f, seen);
            }
            else
            {
                for (auto& [_name, prop] : ttv->props)
                    visit(prop.type, f, seen);

                if (ttv->indexer)
                {
                    visit(ttv->indexer->indexType, f, seen);
                    visit(ttv->indexer->indexResultType, f, seen);
                }
            }
        }
    }

    else if (auto mtv = get<MetatableTypeVar>(ty))
    {
        if (apply(ty, *mtv, seen, f))
        {
            visit(mtv->table, f, seen);
            visit(mtv->metatable, f, seen);
        }
    }

    else if (auto ctv = get<ClassTypeVar>(ty))
    {
        if (apply(ty, *ctv, seen, f))
        {
            for (const auto& [name, prop] : ctv->props)
                visit(prop.type, f, seen);

            if (ctv->parent)
                visit(*ctv->parent, f, seen);

            if (ctv->metatable)
                visit(*ctv->metatable, f, seen);
        }
    }

    else if (auto atv = get<AnyTypeVar>(ty))
        apply(ty, *atv, seen, f);

    else if (auto utv = get<UnionTypeVar>(ty))
    {
        if (apply(ty, *utv, seen, f))
        {
            for (TypeId optTy : utv->options)
                visit(optTy, f, seen);
        }
    }

    else if (auto itv = get<IntersectionTypeVar>(ty))
    {
        if (apply(ty, *itv, seen, f))
        {
            for (TypeId partTy : itv->parts)
                visit(partTy, f, seen);
        }
    }

    visit_detail::unsee(seen, ty);
}

template<typename F, typename Set>
void visit(TypePackId tp, F& f, Set& seen)
{
    if (visit_detail::hasSeen(seen, tp))
    {
        f.cycle(tp);
        return;
    }

    if (auto btv = get<BoundTypePack>(tp))
    {
        if (apply(tp, *btv, seen, f))
            visit(btv->boundTo, f, seen);
    }

    else if (auto ftv = get<Unifiable::Free>(tp))
        apply(tp, *ftv, seen, f);

    else if (auto gtv = get<Unifiable::Generic>(tp))
        apply(tp, *gtv, seen, f);

    else if (auto etv = get<Unifiable::Error>(tp))
        apply(tp, *etv, seen, f);

    else if (auto pack = get<TypePack>(tp))
    {
        apply(tp, *pack, seen, f);

        for (TypeId ty : pack->head)
            visit(ty, f, seen);

        if (pack->tail)
            visit(*pack->tail, f, seen);
    }
    else if (auto pack = get<VariadicTypePack>(tp))
    {
        apply(tp, *pack, seen, f);
        visit(pack->ty, f, seen);
    }

    visit_detail::unsee(seen, tp);
}

} // namespace visit_detail

template<typename TID, typename F>
void visitTypeVar(TID ty, F& f, std::unordered_set<void*>& seen)
{
    visit_detail::visit(ty, f, seen);
}

template<typename TID, typename F>
void visitTypeVar(TID ty, F& f)
{
    std::unordered_set<void*> seen;
    visit_detail::visit(ty, f, seen);
}

template<typename TID, typename F>
void visitTypeVarOnce(TID ty, F& f, DenseHashSet<void*>& seen)
{
    seen.clear();
    visit_detail::visit(ty, f, seen);
}

} // namespace Luau
