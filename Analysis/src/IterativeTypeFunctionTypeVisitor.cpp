// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/IterativeTypeFunctionTypeVisitor.h"

LUAU_FASTINT(LuauVisitRecursionLimit)

namespace Luau
{

IterativeTypeFunctionTypeVisitor::WorkItem::WorkItem(TypeFunctionTypeId ty, int32_t parent)
    : t(ty)
    , isType(true)
    , parent(parent)
{
}

IterativeTypeFunctionTypeVisitor::WorkItem::WorkItem(TypeFunctionTypePackId tp, int32_t parent)
    : t(tp)
    , isType(false)
    , parent(parent)
{
}

const TypeFunctionTypeId* IterativeTypeFunctionTypeVisitor::WorkItem::asType() const
{
    if (isType)
        return reinterpret_cast<const TypeFunctionTypeId*>(&t);
    else
        return nullptr;
}

const TypeFunctionTypePackId* IterativeTypeFunctionTypeVisitor::WorkItem::asTypePack() const
{
    if (isType)
        return nullptr;
    else
        return reinterpret_cast<const TypeFunctionTypePackId*>(&t);
}

bool IterativeTypeFunctionTypeVisitor::WorkItem::operator==(TypeFunctionTypeId ty) const
{
    auto asTy = asType();
    return asTy != nullptr && *asTy == ty;
}

bool IterativeTypeFunctionTypeVisitor::WorkItem::operator==(TypeFunctionTypePackId tp) const
{
    auto asTp = asTypePack();
    return asTp != nullptr && *asTp == tp;
}

IterativeTypeFunctionTypeVisitor::IterativeTypeFunctionTypeVisitor(std::string visitorName)
    : IterativeTypeFunctionTypeVisitor(std::move(visitorName), SeenSet{nullptr}, /*visitOnce*/ true)
{
}

IterativeTypeFunctionTypeVisitor::IterativeTypeFunctionTypeVisitor(std::string visitorName, bool visitOnce)
    : IterativeTypeFunctionTypeVisitor(std::move(visitorName), SeenSet{nullptr}, visitOnce)
{
}

IterativeTypeFunctionTypeVisitor::IterativeTypeFunctionTypeVisitor(std::string visitorName, SeenSet seen, bool visitOnce)
    : seen(std::move(seen))
    , visitorName(std::move(visitorName))
    , visitOnce(visitOnce)
{
    // Skip the first few doublings.  Almost all visits require less than 32 steps.
    workQueue.reserve(32);
}

void IterativeTypeFunctionTypeVisitor::cycle(TypeFunctionTypeId) {}
void IterativeTypeFunctionTypeVisitor::cycle(TypeFunctionTypePackId) {}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypeId ty)
{
    return true;
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypeId ty, const TypeFunctionPrimitiveType& tfpt)
{
    return visit(ty);
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypeId ty, const TypeFunctionAnyType& tfat)
{
    return visit(ty);
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypeId ty, const TypeFunctionUnknownType& tfut)
{
    return visit(ty);
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypeId ty, const TypeFunctionNeverType& tfnt)
{
    return visit(ty);
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypeId ty, const TypeFunctionSingletonType& tfst)
{
    return visit(ty);
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypeId ty, const TypeFunctionUnionType& tfut)
{
    return visit(ty);
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypeId ty, const TypeFunctionIntersectionType& tfit)
{
    return visit(ty);
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypeId ty, const TypeFunctionNegationType& tfnt)
{
    return visit(ty);
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypeId ty, const TypeFunctionFunctionType& tfft)
{
    return visit(ty);
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypeId ty, const TypeFunctionTableType& tftt)
{
    return visit(ty);
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypeId ty, const TypeFunctionExternType& tfet)
{
    return visit(ty);
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypeId ty, const TypeFunctionGenericType& tfgt)
{
    return visit(ty);
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypePackId tp)
{
    return true;
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypePackId tp, const TypeFunctionTypePack& tftp)
{
    return visit(tp);
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypePackId tp, const TypeFunctionVariadicTypePack& tfvtp)
{
    return visit(tp);
}

bool IterativeTypeFunctionTypeVisitor::visit(TypeFunctionTypePackId tp, const TypeFunctionGenericTypePack& tfgtp)
{
    return visit(tp);
}

void IterativeTypeFunctionTypeVisitor::run(TypeFunctionTypeId rootTy)
{
    parentCursor = -1;
    workCursor = 0;
    workQueue.clear();
    traverse(rootTy);
    processWorkQueue();
}

void IterativeTypeFunctionTypeVisitor::run(TypeFunctionTypePackId rootTp)
{
    parentCursor = -1;
    workCursor = 0;
    workQueue.clear();
    traverse(rootTp);
    processWorkQueue();
}

void IterativeTypeFunctionTypeVisitor::traverse(TypeFunctionTypeId ty)
{
    workQueue.emplace_back(ty, parentCursor);
}

void IterativeTypeFunctionTypeVisitor::traverse(TypeFunctionTypePackId tp)
{
    workQueue.emplace_back(tp, parentCursor);
}

void IterativeTypeFunctionTypeVisitor::process(TypeFunctionTypeId ty)
{
    if (hasSeen(ty))
        return;

    if (auto tfpt = get<TypeFunctionPrimitiveType>(ty))
        visit(ty, *tfpt);
    else if (auto tfat = get<TypeFunctionAnyType>(ty))
        visit(ty, *tfat);
    else if (auto tfut = get<TypeFunctionUnknownType>(ty))
        visit(ty, *tfut);
    else if (auto tfnt = get<TypeFunctionNeverType>(ty))
        visit(ty, *tfnt);
    else if (auto tfst = get<TypeFunctionSingletonType>(ty))
    {
        visit(ty, *tfst);
    }
    else if (auto tfut = get<TypeFunctionUnionType>(ty))
    {
        if (visit(ty, *tfut))
        {
            for (TypeFunctionTypeId component : tfut->components)
                traverse(component);
        }
    }
    else if (auto tfit = get<TypeFunctionIntersectionType>(ty))
    {
        if (visit(ty, *tfit))
        {
            for (TypeFunctionTypeId component : tfit->components)
                traverse(component);
        }
    }
    else if (auto tfnt = get<TypeFunctionNegationType>(ty))
    {
        if (visit(ty, *tfnt))
            traverse(tfnt->type);
    }
    else if (auto tfft = get<TypeFunctionFunctionType>(ty))
    {
        if (visit(ty, *tfft))
        {
            for (auto generic : tfft->generics)
                traverse(generic);

            for (auto generic : tfft->genericPacks)
                traverse(generic);

            traverse(tfft->argTypes);
            traverse(tfft->retTypes);
        }
    }
    else if (auto tftt = get<TypeFunctionTableType>(ty))
    {
        if (visit(ty, *tftt))
        {
            for (auto& [_name, prop] : tftt->props)
            {
                if (auto ty = prop.readTy)
                    traverse(*ty);

                // In the case that the readType and the writeType are the same pointer, just traverse once.
                // Traversing each property twice has pretty significant performance consequences.
                if (auto ty = prop.writeTy; ty && !prop.isShared())
                    traverse(*ty);
            }

            if (tftt->metatable)
                traverse(*tftt->metatable);

            if (tftt->indexer)
            {
                traverse(tftt->indexer->keyType);
                traverse(tftt->indexer->valueType);
            }
        }
    }
    else if (auto tfet = get<TypeFunctionExternType>(ty))
    {
        if (visit(ty, *tfet))
        {
            for (auto& [_name, prop] : tfet->props)
            {
                if (auto ty = prop.readTy)
                    traverse(*ty);

                // In the case that the readType and the writeType are the same pointer, just traverse once.
                // Traversing each property twice has pretty significant performance consequences.
                if (auto ty = prop.writeTy; ty && !prop.isShared())
                    traverse(*ty);
            }

            if (tfet->metatable)
                traverse(*tfet->metatable);

            if (tfet->readParent)
                traverse(*tfet->readParent);
            if (tfet->writeParent)
                traverse(*tfet->writeParent);

            if (tfet->indexer)
            {
                traverse(tfet->indexer->keyType);
                traverse(tfet->indexer->valueType);
            }
        }
    }
    else if (auto tfgt = get<TypeFunctionGenericType>(ty))
        visit(ty, *tfgt);
    else
        LUAU_ASSERT(!"GenericTypeFunctionTypeVisitor::traverse(TypeFunctionTypeId) is not exhaustive!");

    unsee(ty);
}

void IterativeTypeFunctionTypeVisitor::process(TypeFunctionTypePackId tp)
{
    if (hasSeen(tp))
        return;

    if (auto tftp = get<TypeFunctionTypePack>(tp))
    {
        if (visit(tp, *tftp))
        {
            for (auto ty : tftp->head)
                traverse(ty);

            if (tftp->tail)
                traverse(*tftp->tail);
        }
    }
    else if (auto tfvtp = get<TypeFunctionVariadicTypePack>(tp))
    {
        if (visit(tp, *tfvtp))
            traverse(tfvtp->type);
    }
    else if (auto tfgtv = get<TypeFunctionGenericTypePack>(tp))
        visit(tp, *tfgtv);
    else
        LUAU_ASSERT(!"GenericTypeFunctionTypeVisitor::traverse(TypeFunctionTypePackId) is not exhaustive!");

    unsee(tp);
}

bool IterativeTypeFunctionTypeVisitor::hasSeen(const void* tv)
{
    if (!visitOnce)
        return false;

    bool isFresh = seen.insert(tv);
    return !isFresh;
}

void IterativeTypeFunctionTypeVisitor::unsee(const void* tv)
{
    if (!visitOnce)
        seen.erase(tv);
}

template<typename TID>
bool IterativeTypeFunctionTypeVisitor::isCyclic(TID ty) const
{
    const IterativeTypeFunctionTypeVisitor::WorkItem* item = &workQueue[workCursor];
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

void IterativeTypeFunctionTypeVisitor::processWorkQueue()
{
    while (workCursor < workQueue.size())
    {
        const WorkItem& item = workQueue[workCursor];
        parentCursor = workCursor;

        if (const TypeFunctionTypeId* ty = item.asType())
        {
            if (isCyclic(*ty))
                cycle(*ty);
            else
                process(*ty);
        }
        else if (const TypeFunctionTypePackId* tp = item.asTypePack())
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
