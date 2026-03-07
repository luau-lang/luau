// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/StructuralTypeEquality.h"

#include "Luau/Type.h"
#include "Luau/TypePack.h"

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauAnalysisUsesSolverMode)

// Test Types for equivalence
// More complex than we'd like because Types can self-reference.

namespace Luau
{

bool areSeen(SeenSet& seen, const void* lhs, const void* rhs)
{
    if (lhs == rhs)
        return true;

    auto p = std::make_pair(const_cast<void*>(lhs), const_cast<void*>(rhs));
    if (seen.find(p) != seen.end())
        return true;

    seen.insert(p);
    return false;
}

bool areEqual(SeenSet& seen, const TypePackVar& lhs, const TypePackVar& rhs)
{
    TypePackId lhsId = const_cast<TypePackId>(&lhs);
    TypePackId rhsId = const_cast<TypePackId>(&rhs);
    TypePackIterator lhsIter = begin(lhsId);
    TypePackIterator rhsIter = begin(rhsId);
    TypePackIterator lhsEnd = end(lhsId);
    TypePackIterator rhsEnd = end(rhsId);
    while (lhsIter != lhsEnd && rhsIter != rhsEnd)
    {
        if (!areEqual(seen, **lhsIter, **rhsIter))
            return false;
        ++lhsIter;
        ++rhsIter;
    }

    if (lhsIter != lhsEnd || rhsIter != rhsEnd)
        return false;

    if (!lhsIter.tail() && !rhsIter.tail())
        return true;
    if (!lhsIter.tail() || !rhsIter.tail())
        return false;

    TypePackId lhsTail = *lhsIter.tail();
    TypePackId rhsTail = *rhsIter.tail();

    {
        const FreeTypePack* lf = get_if<FreeTypePack>(&lhsTail->ty);
        const FreeTypePack* rf = get_if<FreeTypePack>(&rhsTail->ty);
        if (lf && rf)
            return lf->index == rf->index;
    }

    {
        const Unifiable::Bound<TypePackId>* lb = get_if<Unifiable::Bound<TypePackId>>(&lhsTail->ty);
        const Unifiable::Bound<TypePackId>* rb = get_if<Unifiable::Bound<TypePackId>>(&rhsTail->ty);
        if (lb && rb)
            return areEqual(seen, *lb->boundTo, *rb->boundTo);
    }

    {
        const GenericTypePack* lg = get_if<GenericTypePack>(&lhsTail->ty);
        const GenericTypePack* rg = get_if<GenericTypePack>(&rhsTail->ty);
        if (lg && rg)
            return lg->index == rg->index;
    }

    {
        const VariadicTypePack* lv = get_if<VariadicTypePack>(&lhsTail->ty);
        const VariadicTypePack* rv = get_if<VariadicTypePack>(&rhsTail->ty);
        if (lv && rv)
            return areEqual(seen, *lv->ty, *rv->ty);
    }

    return false;
}

bool areEqual(SeenSet& seen, const FunctionType& lhs, const FunctionType& rhs)
{
    if (areSeen(seen, &lhs, &rhs))
        return true;

    // TODO: check generics CLI-39915

    if (!areEqual(seen, *lhs.argTypes, *rhs.argTypes))
        return false;

    if (!areEqual(seen, *lhs.retTypes, *rhs.retTypes))
        return false;

    return true;
}

bool areEqual(SeenSet& seen, const TableType& lhs, const TableType& rhs)
{
    if (areSeen(seen, &lhs, &rhs))
        return true;

    if (lhs.state != rhs.state)
        return false;

    if (lhs.props.size() != rhs.props.size())
        return false;

    if (bool(lhs.indexer) != bool(rhs.indexer))
        return false;

    if (lhs.indexer && rhs.indexer)
    {
        if (!areEqual(seen, *lhs.indexer->indexType, *rhs.indexer->indexType))
            return false;

        if (!areEqual(seen, *lhs.indexer->indexResultType, *rhs.indexer->indexResultType))
            return false;
    }

    auto l = lhs.props.begin();
    auto r = rhs.props.begin();

    while (l != lhs.props.end())
    {
        if (l->first != r->first)
            return false;

        if (FFlag::LuauAnalysisUsesSolverMode || FFlag::LuauSolverV2)
        {
            if (l->second.readTy && r->second.readTy)
            {
                if (!areEqual(seen, **l->second.readTy, **r->second.readTy))
                    return false;
            }
            else if (l->second.readTy || r->second.readTy)
                return false;

            if (l->second.writeTy && r->second.writeTy)
            {
                if (!areEqual(seen, **l->second.writeTy, **r->second.writeTy))
                    return false;
            }
            else if (l->second.writeTy || r->second.writeTy)
                return false;
        }
        else if (!areEqual(seen, *l->second.type_DEPRECATED(), *r->second.type_DEPRECATED()))
            return false;
        ++l;
        ++r;
    }

    return true;
}

static bool areEqual(SeenSet& seen, const MetatableType& lhs, const MetatableType& rhs)
{
    if (areSeen(seen, &lhs, &rhs))
        return true;

    return areEqual(seen, *lhs.table, *rhs.table) && areEqual(seen, *lhs.metatable, *rhs.metatable);
}

bool areEqual(SeenSet& seen, const Type& lhs, const Type& rhs)
{
    if (auto bound = get_if<BoundType>(&lhs.ty))
        return areEqual(seen, *bound->boundTo, rhs);

    if (auto bound = get_if<BoundType>(&rhs.ty))
        return areEqual(seen, lhs, *bound->boundTo);

    if (lhs.ty.index() != rhs.ty.index())
        return false;

    {
        const FreeType* lf = get_if<FreeType>(&lhs.ty);
        const FreeType* rf = get_if<FreeType>(&rhs.ty);
        if (lf && rf)
            return lf->index == rf->index;
    }

    {
        const GenericType* lg = get_if<GenericType>(&lhs.ty);
        const GenericType* rg = get_if<GenericType>(&rhs.ty);
        if (lg && rg)
            return lg->index == rg->index;
    }

    {
        const PrimitiveType* lp = get_if<PrimitiveType>(&lhs.ty);
        const PrimitiveType* rp = get_if<PrimitiveType>(&rhs.ty);
        if (lp && rp)
            return lp->type == rp->type;
    }

    {
        const GenericType* lg = get_if<GenericType>(&lhs.ty);
        const GenericType* rg = get_if<GenericType>(&rhs.ty);
        if (lg && rg)
            return lg->index == rg->index;
    }

    {
        const ErrorType* le = get_if<ErrorType>(&lhs.ty);
        const ErrorType* re = get_if<ErrorType>(&rhs.ty);
        if (le && re)
            return le->index == re->index;
    }

    {
        const FunctionType* lf = get_if<FunctionType>(&lhs.ty);
        const FunctionType* rf = get_if<FunctionType>(&rhs.ty);
        if (lf && rf)
            return areEqual(seen, *lf, *rf);
    }

    {
        const TableType* lt = get_if<TableType>(&lhs.ty);
        const TableType* rt = get_if<TableType>(&rhs.ty);
        if (lt && rt)
            return areEqual(seen, *lt, *rt);
    }

    {
        const MetatableType* lmt = get_if<MetatableType>(&lhs.ty);
        const MetatableType* rmt = get_if<MetatableType>(&rhs.ty);

        if (lmt && rmt)
            return areEqual(seen, *lmt, *rmt);
    }

    if (get_if<AnyType>(&lhs.ty) && get_if<AnyType>(&rhs.ty))
        return true;

    return false;
}

} // namespace Luau
