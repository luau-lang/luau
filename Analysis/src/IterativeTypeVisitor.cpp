// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IterativeTypeVisitor.h"

LUAU_FASTINT(LuauVisitRecursionLimit)

namespace Luau
{

IterativeTypeVisitor::WorkItem::WorkItem(TypeId ty, int32_t parent)
    : t(ty)
    , isType(true)
    , parent(parent)
{
}

IterativeTypeVisitor::WorkItem::WorkItem(TypePackId tp, int32_t parent)
    : t(tp)
    , isType(false)
    , parent(parent)
{
}

const TypeId* IterativeTypeVisitor::WorkItem::asType() const
{
    if (isType)
        return reinterpret_cast<const TypeId*>(&t);
    else
        return nullptr;
}

const TypePackId* IterativeTypeVisitor::WorkItem::asTypePack() const
{
    if (isType)
        return nullptr;
    else
        return reinterpret_cast<const TypePackId*>(&t);
}

bool IterativeTypeVisitor::WorkItem::operator==(TypeId ty) const
{
    auto asTy = asType();
    return asTy != nullptr && *asTy == ty;
}

bool IterativeTypeVisitor::WorkItem::operator==(TypePackId tp) const
{
    auto asTp = asTypePack();
    return asTp != nullptr && *asTp == tp;
}

IterativeTypeVisitor::IterativeTypeVisitor(std::string visitorName, bool skipBoundTypes)
    : IterativeTypeVisitor(std::move(visitorName), SeenSet{nullptr}, /*visitOnce*/ true, skipBoundTypes)
{
}

IterativeTypeVisitor::IterativeTypeVisitor(std::string visitorName, bool visitOnce, bool skipBoundTypes)
    : IterativeTypeVisitor(std::move(visitorName), SeenSet{nullptr}, visitOnce, skipBoundTypes)
{
}

IterativeTypeVisitor::IterativeTypeVisitor(std::string visitorName, SeenSet seen, bool visitOnce, bool skipBoundTypes)
    : seen(std::move(seen))
    , visitorName(std::move(visitorName))
    , skipBoundTypes(skipBoundTypes)
    , visitOnce(visitOnce)
{
    // Skip the first few doublings.  Almost all visits require less than 32 steps.
    workQueue.reserve(32);
}

void IterativeTypeVisitor::cycle(TypeId) {}
void IterativeTypeVisitor::cycle(TypePackId) {}

bool IterativeTypeVisitor::visit(TypeId ty)
{
    return true;
}

bool IterativeTypeVisitor::visit(TypeId ty, const BoundType& btv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const FreeType& ftv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const GenericType& gtv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const ErrorType& etv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const PrimitiveType& ptv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const FunctionType& ftv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const TableType& ttv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const MetatableType& mtv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const ExternType& etv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const AnyType& atv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const NoRefineType& nrt)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const UnknownType& utv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const NeverType& ntv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const UnionType& utv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const IntersectionType& itv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const BlockedType& btv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const PendingExpansionType& petv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const SingletonType& stv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const NegationType& ntv)
{
    return visit(ty);
}

bool IterativeTypeVisitor::visit(TypeId ty, const TypeFunctionInstanceType& tfit)
{
    return visit(ty);
}


bool IterativeTypeVisitor::visit(TypePackId tp)
{
    return true;
}

bool IterativeTypeVisitor::visit(TypePackId tp, const BoundTypePack& btp)
{
    return visit(tp);
}

bool IterativeTypeVisitor::visit(TypePackId tp, const FreeTypePack& ftp)
{
    return visit(tp);
}

bool IterativeTypeVisitor::visit(TypePackId tp, const GenericTypePack& gtp)
{
    return visit(tp);
}

bool IterativeTypeVisitor::visit(TypePackId tp, const ErrorTypePack& etp)
{
    return visit(tp);
}

bool IterativeTypeVisitor::visit(TypePackId tp, const TypePack& pack)
{
    return visit(tp);
}

bool IterativeTypeVisitor::visit(TypePackId tp, const VariadicTypePack& vtp)
{
    return visit(tp);
}

bool IterativeTypeVisitor::visit(TypePackId tp, const BlockedTypePack& btp)
{
    return visit(tp);
}

bool IterativeTypeVisitor::visit(TypePackId tp, const TypeFunctionInstanceTypePack& tfitp)
{
    return visit(tp);
}

void IterativeTypeVisitor::run(TypeId rootTy)
{
    parentCursor = -1;
    workCursor = 0;
    workQueue.clear();
    traverse(rootTy);
    processWorkQueue();
}

void IterativeTypeVisitor::run(TypePackId rootTp)
{
    parentCursor = -1;
    workCursor = 0;
    workQueue.clear();
    traverse(rootTp);
    processWorkQueue();
}

void IterativeTypeVisitor::traverse(TypeId ty)
{
    workQueue.emplace_back(ty, parentCursor);
}

void IterativeTypeVisitor::traverse(TypePackId tp)
{
    workQueue.emplace_back(tp, parentCursor);
}

void IterativeTypeVisitor::process(TypeId ty)
{
    // Morally, if `skipBoundTypes` is set, then whenever we encounter a bound
    // type we should "skip" ahead to the first non-bound type.
    //
    // We do this check here so that we treat all bound types as if they're
    // direct pointers to some final non-bound type. If we do the check later,
    // then we might get slightly different behavior depending on the exact
    // entry point for cyclic types.
    if (skipBoundTypes)
    {
        if (is<BoundType>(ty))
            ty = follow(ty);
        else if (const TableType* tt = get<TableType>(ty); tt && tt->boundTo)
            ty = follow(ty);
    }

    if (hasSeen(ty))
        return;

    if (auto btv = get<BoundType>(ty))
    {
        // At this point, we know that `skipBoundTypes` is false, as
        // otherwise we would have hit the above branch.
        LUAU_ASSERT(!skipBoundTypes);
        if (visit(ty, *btv))
            traverse(btv->boundTo);
    }
    else if (auto ftv = get<FreeType>(ty))
    {
        if (visit(ty, *ftv))
        {
            LUAU_ASSERT(ftv->lowerBound);
            LUAU_ASSERT(ftv->upperBound);

            traverse(ftv->lowerBound);
            traverse(ftv->upperBound);
        }
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
        LUAU_ASSERT(!skipBoundTypes || !ttv->boundTo);
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
                    if (auto ty = prop.readTy)
                        traverse(*ty);

                    // In the case that the readType and the writeType
                    // are the same pointer, just traverse once.
                    // Traversing each property twice has pretty
                    // significant performance consequences.
                    if (auto ty = prop.writeTy; ty && !prop.isShared())
                        traverse(*ty);
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
    else if (auto etv = get<ExternType>(ty))
    {
        if (visit(ty, *etv))
        {
            for (const auto& [name, prop] : etv->props)
            {
                if (auto ty = prop.readTy)
                    traverse(*ty);

                // In the case that the readType and the writeType are
                // the same pointer, just traverse once. Traversing each
                // property twice would have pretty significant
                // performance consequences.
                if (auto ty = prop.writeTy; ty && !prop.isShared())
                    traverse(*ty);
            }

            if (etv->parent)
                traverse(*etv->parent);

            if (etv->metatable)
                traverse(*etv->metatable);

            if (etv->indexer)
            {
                traverse(etv->indexer->indexType);
                traverse(etv->indexer->indexResultType);
            }
        }
    }
    else if (auto atv = get<AnyType>(ty))
        visit(ty, *atv);
    else if (auto nrt = get<NoRefineType>(ty))
        visit(ty, *nrt);
    else if (auto utv = get<UnionType>(ty))
    {
        if (visit(ty, *utv))
        {
            bool unionChanged = false;
            for (TypeId optTy : utv->options)
            {
                traverse(optTy);
                if (!get<UnionType>(follow(ty)))
                {
                    unionChanged = true;
                    break;
                }
            }

            if (unionChanged)
                traverse(ty);
        }
    }
    else if (auto itv = get<IntersectionType>(ty))
    {
        if (visit(ty, *itv))
        {
            bool intersectionChanged = false;
            for (TypeId partTy : itv->parts)
            {
                traverse(partTy);
                if (!get<IntersectionType>(follow(ty)))
                {
                    intersectionChanged = true;
                    break;
                }
            }

            if (intersectionChanged)
                traverse(ty);
        }
    }
    else if (auto ltv = get<LazyType>(ty))
    {
        if (TypeId unwrapped = ltv->unwrapped)
            traverse(unwrapped);

        // Visiting into LazyType that hasn't been unwrapped may necessarily
        // cause infinite expansion, so we don't do that on purpose. Asserting
        // also makes no sense, because the type _will_ happen here, most likely
        // as a property of some ExternType that doesn't need to be expanded.
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
    else if (auto tfit = get<TypeFunctionInstanceType>(ty))
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

    unsee(ty);
}

void IterativeTypeVisitor::process(TypePackId tp)
{
    if (hasSeen(tp))
        return;

    if (auto btv = get<BoundTypePack>(tp))
    {
        if (visit(tp, *btv))
            traverse(btv->boundTo);
    }

    else if (auto ftv = get<FreeTypePack>(tp))
        visit(tp, *ftv);

    else if (auto gtv = get<GenericTypePack>(tp))
        visit(tp, *gtv);

    else if (auto etv = get<ErrorTypePack>(tp))
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
    else if (auto tfitp = get<TypeFunctionInstanceTypePack>(tp))
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

    unsee(tp);
}

bool IterativeTypeVisitor::hasSeen(const void* tv)
{
    if (!visitOnce)
        return false;

    bool isFresh = seen.insert(tv);
    return !isFresh;
}

void IterativeTypeVisitor::unsee(const void* tv)
{
    if (!visitOnce)
        seen.erase(tv);
}

template<typename TID>
bool IterativeTypeVisitor::isCyclic(TID ty) const
{
    const IterativeTypeVisitor::WorkItem* item = &workQueue[workCursor];
    int32_t cursor = workCursor;

    while (item->parent >= 0)
    {
        LUAU_ASSERT(item->parent < cursor);
        cursor = item->parent;
        item = &workQueue[cursor];

        if (*item == ty)
            return true;
    }

    return false;
}

void IterativeTypeVisitor::processWorkQueue()
{
    while (workCursor < workQueue.size())
    {
        const WorkItem& item = workQueue[workCursor];
        parentCursor = workCursor;

        if (const TypeId* ty = item.asType())
        {
            if (isCyclic(*ty))
                cycle(*ty);
            else
                process(*ty);
        }
        else if (const TypePackId* tp = item.asTypePack())
        {
            if (isCyclic(*tp))
                cycle(*tp);
            else
                process(*tp);
        }
        else
        {
            LUAU_UNREACHABLE();
            LUAU_ASSERT(!"Unreachable");
        }

        ++workCursor;
    }
}

} // namespace Luau
