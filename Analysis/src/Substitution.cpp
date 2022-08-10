// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Substitution.h"

#include "Luau/Common.h"
#include "Luau/Clone.h"
#include "Luau/TxnLog.h"

#include <algorithm>
#include <stdexcept>

LUAU_FASTFLAGVARIABLE(LuauAnyificationMustClone, false)
LUAU_FASTFLAGVARIABLE(LuauSubstitutionFixMissingFields, false)
LUAU_FASTFLAG(LuauLowerBoundsCalculation)
LUAU_FASTINTVARIABLE(LuauTarjanChildLimit, 10000)
LUAU_FASTFLAGVARIABLE(LuauClassTypeVarsInSubstitution, false)
LUAU_FASTFLAG(LuauUnknownAndNeverType)
LUAU_FASTFLAGVARIABLE(LuauSubstitutionReentrant, false)

namespace Luau
{

void Tarjan::visitChildren(TypeId ty, int index)
{
    LUAU_ASSERT(ty == log->follow(ty));

    if (ignoreChildren(ty))
        return;

    if (auto pty = log->pending(ty))
        ty = &pty->pending;

    if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(ty))
    {
        if (FFlag::LuauSubstitutionFixMissingFields)
        {
            for (TypeId generic : ftv->generics)
                visitChild(generic);
            for (TypePackId genericPack : ftv->genericPacks)
                visitChild(genericPack);
        }

        visitChild(ftv->argTypes);
        visitChild(ftv->retTypes);
    }
    else if (const TableTypeVar* ttv = get<TableTypeVar>(ty))
    {
        LUAU_ASSERT(!ttv->boundTo);
        for (const auto& [name, prop] : ttv->props)
            visitChild(prop.type);
        if (ttv->indexer)
        {
            visitChild(ttv->indexer->indexType);
            visitChild(ttv->indexer->indexResultType);
        }

        for (TypeId itp : ttv->instantiatedTypeParams)
            visitChild(itp);

        for (TypePackId itp : ttv->instantiatedTypePackParams)
            visitChild(itp);
    }
    else if (const MetatableTypeVar* mtv = get<MetatableTypeVar>(ty))
    {
        visitChild(mtv->table);
        visitChild(mtv->metatable);
    }
    else if (const UnionTypeVar* utv = get<UnionTypeVar>(ty))
    {
        for (TypeId opt : utv->options)
            visitChild(opt);
    }
    else if (const IntersectionTypeVar* itv = get<IntersectionTypeVar>(ty))
    {
        for (TypeId part : itv->parts)
            visitChild(part);
    }
    else if (const ConstrainedTypeVar* ctv = get<ConstrainedTypeVar>(ty))
    {
        for (TypeId part : ctv->parts)
            visitChild(part);
    }
    else if (const PendingExpansionTypeVar* petv = get<PendingExpansionTypeVar>(ty))
    {
        for (TypeId a : petv->typeArguments)
            visitChild(a);

        for (TypePackId a : petv->packArguments)
            visitChild(a);
    }
    else if (const ClassTypeVar* ctv = get<ClassTypeVar>(ty); FFlag::LuauClassTypeVarsInSubstitution && ctv)
    {
        for (auto [name, prop] : ctv->props)
            visitChild(prop.type);

        if (ctv->parent)
            visitChild(*ctv->parent);

        if (ctv->metatable)
            visitChild(*ctv->metatable);
    }
}

void Tarjan::visitChildren(TypePackId tp, int index)
{
    LUAU_ASSERT(tp == log->follow(tp));

    if (ignoreChildren(tp))
        return;

    if (auto ptp = log->pending(tp))
        tp = &ptp->pending;

    if (const TypePack* tpp = get<TypePack>(tp))
    {
        for (TypeId tv : tpp->head)
            visitChild(tv);
        if (tpp->tail)
            visitChild(*tpp->tail);
    }
    else if (const VariadicTypePack* vtp = get<VariadicTypePack>(tp))
    {
        visitChild(vtp->ty);
    }
}

std::pair<int, bool> Tarjan::indexify(TypeId ty)
{
    ty = log->follow(ty);

    bool fresh = !typeToIndex.contains(ty);
    int& index = typeToIndex[ty];

    if (fresh)
    {
        index = int(indexToType.size());
        indexToType.push_back(ty);
        indexToPack.push_back(nullptr);
        onStack.push_back(false);
        lowlink.push_back(index);
    }
    return {index, fresh};
}

std::pair<int, bool> Tarjan::indexify(TypePackId tp)
{
    tp = log->follow(tp);

    bool fresh = !packToIndex.contains(tp);
    int& index = packToIndex[tp];

    if (fresh)
    {
        index = int(indexToPack.size());
        indexToType.push_back(nullptr);
        indexToPack.push_back(tp);
        onStack.push_back(false);
        lowlink.push_back(index);
    }
    return {index, fresh};
}

void Tarjan::visitChild(TypeId ty)
{
    ty = log->follow(ty);

    edgesTy.push_back(ty);
    edgesTp.push_back(nullptr);
}

void Tarjan::visitChild(TypePackId tp)
{
    tp = log->follow(tp);

    edgesTy.push_back(nullptr);
    edgesTp.push_back(tp);
}

TarjanResult Tarjan::loop()
{
    // Normally Tarjan is presented recursively, but this is a hot loop, so worth optimizing
    while (!worklist.empty())
    {
        auto [index, currEdge, lastEdge] = worklist.back();

        // First visit
        if (currEdge == -1)
        {
            ++childCount;
            if (childLimit > 0 && (FFlag::LuauUnknownAndNeverType ? childLimit <= childCount : childLimit < childCount))
                return TarjanResult::TooManyChildren;

            stack.push_back(index);
            onStack[index] = true;

            currEdge = int(edgesTy.size());

            // Fill in edge list of this vertex
            if (TypeId ty = indexToType[index])
                visitChildren(ty, index);
            else if (TypePackId tp = indexToPack[index])
                visitChildren(tp, index);

            lastEdge = int(edgesTy.size());
        }

        // Visit children
        bool foundFresh = false;

        for (; currEdge < lastEdge; currEdge++)
        {
            int childIndex = -1;
            bool fresh = false;

            if (auto ty = edgesTy[currEdge])
                std::tie(childIndex, fresh) = indexify(ty);
            else if (auto tp = edgesTp[currEdge])
                std::tie(childIndex, fresh) = indexify(tp);
            else
                LUAU_ASSERT(false);

            if (fresh)
            {
                // Original recursion point, update the parent continuation point and start the new element
                worklist.back() = {index, currEdge + 1, lastEdge};
                worklist.push_back({childIndex, -1, -1});

                // We need to continue the top-level loop from the start with the new worklist element
                foundFresh = true;
                break;
            }
            else if (onStack[childIndex])
            {
                lowlink[index] = std::min(lowlink[index], childIndex);
            }

            visitEdge(childIndex, index);
        }

        if (foundFresh)
            continue;

        if (lowlink[index] == index)
        {
            visitSCC(index);
            while (!stack.empty())
            {
                int popped = stack.back();
                stack.pop_back();
                onStack[popped] = false;
                if (popped == index)
                    break;
            }
        }

        worklist.pop_back();

        // Original return from recursion into a child
        if (!worklist.empty())
        {
            auto [parentIndex, _, parentEndEdge] = worklist.back();

            // No need to keep child edges around
            edgesTy.resize(parentEndEdge);
            edgesTp.resize(parentEndEdge);

            lowlink[parentIndex] = std::min(lowlink[parentIndex], lowlink[index]);
            visitEdge(index, parentIndex);
        }
    }

    return TarjanResult::Ok;
}

TarjanResult Tarjan::visitRoot(TypeId ty)
{
    childCount = 0;
    if (childLimit == 0)
        childLimit = FInt::LuauTarjanChildLimit;

    ty = log->follow(ty);

    auto [index, fresh] = indexify(ty);
    worklist.push_back({index, -1, -1});
    return loop();
}

TarjanResult Tarjan::visitRoot(TypePackId tp)
{
    childCount = 0;
    if (childLimit == 0)
        childLimit = FInt::LuauTarjanChildLimit;

    tp = log->follow(tp);

    auto [index, fresh] = indexify(tp);
    worklist.push_back({index, -1, -1});
    return loop();
}

void FindDirty::clearTarjan()
{
    dirty.clear();

    typeToIndex.clear();
    packToIndex.clear();
    indexToType.clear();
    indexToPack.clear();

    stack.clear();
    onStack.clear();
    lowlink.clear();

    edgesTy.clear();
    edgesTp.clear();
    worklist.clear();
}

bool FindDirty::getDirty(int index)
{
    if (dirty.size() <= size_t(index))
        dirty.resize(index + 1, false);
    return dirty[index];
}

void FindDirty::setDirty(int index, bool d)
{
    if (dirty.size() <= size_t(index))
        dirty.resize(index + 1, false);
    dirty[index] = d;
}

void FindDirty::visitEdge(int index, int parentIndex)
{
    if (getDirty(index))
        setDirty(parentIndex, true);
}

void FindDirty::visitSCC(int index)
{
    bool d = getDirty(index);

    for (auto it = stack.rbegin(); !d && it != stack.rend(); it++)
    {
        if (TypeId ty = indexToType[*it])
            d = isDirty(ty);
        else if (TypePackId tp = indexToPack[*it])
            d = isDirty(tp);
        if (*it == index)
            break;
    }

    if (!d)
        return;

    for (auto it = stack.rbegin(); it != stack.rend(); it++)
    {
        setDirty(*it, true);
        if (TypeId ty = indexToType[*it])
            foundDirty(ty);
        else if (TypePackId tp = indexToPack[*it])
            foundDirty(tp);
        if (*it == index)
            return;
    }
}

TarjanResult FindDirty::findDirty(TypeId ty)
{
    return visitRoot(ty);
}

TarjanResult FindDirty::findDirty(TypePackId tp)
{
    return visitRoot(tp);
}

std::optional<TypeId> Substitution::substitute(TypeId ty)
{
    ty = log->follow(ty);

    // clear algorithm state for reentrancy
    if (FFlag::LuauSubstitutionReentrant)
        clearTarjan();

    auto result = findDirty(ty);
    if (result != TarjanResult::Ok)
        return std::nullopt;

    for (auto [oldTy, newTy] : newTypes)
    {
        if (FFlag::LuauSubstitutionReentrant)
        {
            if (!ignoreChildren(oldTy) && !replacedTypes.contains(newTy))
            {
                replaceChildren(newTy);
                replacedTypes.insert(newTy);
            }
        }
        else
        {
            if (!ignoreChildren(oldTy))
                replaceChildren(newTy);
        }
    }
    for (auto [oldTp, newTp] : newPacks)
    {
        if (FFlag::LuauSubstitutionReentrant)
        {
            if (!ignoreChildren(oldTp) && !replacedTypePacks.contains(newTp))
            {
                replaceChildren(newTp);
                replacedTypePacks.insert(newTp);
            }
        }
        else
        {
            if (!ignoreChildren(oldTp))
                replaceChildren(newTp);
        }
    }
    TypeId newTy = replace(ty);
    return newTy;
}

std::optional<TypePackId> Substitution::substitute(TypePackId tp)
{
    tp = log->follow(tp);

    // clear algorithm state for reentrancy
    if (FFlag::LuauSubstitutionReentrant)
        clearTarjan();

    auto result = findDirty(tp);
    if (result != TarjanResult::Ok)
        return std::nullopt;

    for (auto [oldTy, newTy] : newTypes)
    {
        if (FFlag::LuauSubstitutionReentrant)
        {
            if (!ignoreChildren(oldTy) && !replacedTypes.contains(newTy))
            {
                replaceChildren(newTy);
                replacedTypes.insert(newTy);
            }
        }
        else
        {
            if (!ignoreChildren(oldTy))
                replaceChildren(newTy);
        }
    }
    for (auto [oldTp, newTp] : newPacks)
    {
        if (FFlag::LuauSubstitutionReentrant)
        {
            if (!ignoreChildren(oldTp) && !replacedTypePacks.contains(newTp))
            {
                replaceChildren(newTp);
                replacedTypePacks.insert(newTp);
            }
        }
        else
        {
            if (!ignoreChildren(oldTp))
                replaceChildren(newTp);
        }
    }
    TypePackId newTp = replace(tp);
    return newTp;
}

TypeId Substitution::clone(TypeId ty)
{
    return shallowClone(ty, *arena, log);
}

TypePackId Substitution::clone(TypePackId tp)
{
    tp = log->follow(tp);

    if (auto ptp = log->pending(tp))
        tp = &ptp->pending;

    if (const TypePack* tpp = get<TypePack>(tp))
    {
        TypePack clone;
        clone.head = tpp->head;
        clone.tail = tpp->tail;
        return addTypePack(std::move(clone));
    }
    else if (const VariadicTypePack* vtp = get<VariadicTypePack>(tp))
    {
        VariadicTypePack clone;
        clone.ty = vtp->ty;
        if (FFlag::LuauSubstitutionFixMissingFields)
            clone.hidden = vtp->hidden;
        return addTypePack(std::move(clone));
    }
    else
        return tp;
}

void Substitution::foundDirty(TypeId ty)
{
    ty = log->follow(ty);

    if (FFlag::LuauSubstitutionReentrant && newTypes.contains(ty))
            return;

    if (isDirty(ty))
        newTypes[ty] = follow(clean(ty));
    else
        newTypes[ty] = follow(clone(ty));
}

void Substitution::foundDirty(TypePackId tp)
{
    tp = log->follow(tp);

    if (FFlag::LuauSubstitutionReentrant && newPacks.contains(tp))
            return;

    if (isDirty(tp))
        newPacks[tp] = follow(clean(tp));
    else
        newPacks[tp] = follow(clone(tp));
}

TypeId Substitution::replace(TypeId ty)
{
    ty = log->follow(ty);

    if (TypeId* prevTy = newTypes.find(ty))
        return *prevTy;
    else
        return ty;
}

TypePackId Substitution::replace(TypePackId tp)
{
    tp = log->follow(tp);

    if (TypePackId* prevTp = newPacks.find(tp))
        return *prevTp;
    else
        return tp;
}

void Substitution::replaceChildren(TypeId ty)
{
    if (BoundTypeVar* btv = log->getMutable<BoundTypeVar>(ty); FFlag::LuauLowerBoundsCalculation && btv)
        btv->boundTo = replace(btv->boundTo);

    LUAU_ASSERT(ty == log->follow(ty));

    if (ignoreChildren(ty))
        return;

    if (FFlag::LuauAnyificationMustClone && ty->owningArena != arena)
        return;

    if (FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(ty))
    {
        if (FFlag::LuauSubstitutionFixMissingFields)
        {
            for (TypeId& generic : ftv->generics)
                generic = replace(generic);
            for (TypePackId& genericPack : ftv->genericPacks)
                genericPack = replace(genericPack);
        }

        ftv->argTypes = replace(ftv->argTypes);
        ftv->retTypes = replace(ftv->retTypes);
    }
    else if (TableTypeVar* ttv = getMutable<TableTypeVar>(ty))
    {
        LUAU_ASSERT(!ttv->boundTo);
        for (auto& [name, prop] : ttv->props)
            prop.type = replace(prop.type);
        if (ttv->indexer)
        {
            ttv->indexer->indexType = replace(ttv->indexer->indexType);
            ttv->indexer->indexResultType = replace(ttv->indexer->indexResultType);
        }

        for (TypeId& itp : ttv->instantiatedTypeParams)
            itp = replace(itp);

        for (TypePackId& itp : ttv->instantiatedTypePackParams)
            itp = replace(itp);
    }
    else if (MetatableTypeVar* mtv = getMutable<MetatableTypeVar>(ty))
    {
        mtv->table = replace(mtv->table);
        mtv->metatable = replace(mtv->metatable);
    }
    else if (UnionTypeVar* utv = getMutable<UnionTypeVar>(ty))
    {
        for (TypeId& opt : utv->options)
            opt = replace(opt);
    }
    else if (IntersectionTypeVar* itv = getMutable<IntersectionTypeVar>(ty))
    {
        for (TypeId& part : itv->parts)
            part = replace(part);
    }
    else if (ConstrainedTypeVar* ctv = getMutable<ConstrainedTypeVar>(ty))
    {
        for (TypeId& part : ctv->parts)
            part = replace(part);
    }
    else if (PendingExpansionTypeVar* petv = getMutable<PendingExpansionTypeVar>(ty))
    {
        for (TypeId& a : petv->typeArguments)
            a = replace(a);

        for (TypePackId& a : petv->packArguments)
            a = replace(a);
    }
    else if (ClassTypeVar* ctv = getMutable<ClassTypeVar>(ty); FFlag::LuauClassTypeVarsInSubstitution && ctv)
    {
        for (auto& [name, prop] : ctv->props)
            prop.type = replace(prop.type);

        if (ctv->parent)
            ctv->parent = replace(*ctv->parent);

        if (ctv->metatable)
            ctv->metatable = replace(*ctv->metatable);
    }
}

void Substitution::replaceChildren(TypePackId tp)
{
    LUAU_ASSERT(tp == log->follow(tp));

    if (ignoreChildren(tp))
        return;

    if (FFlag::LuauAnyificationMustClone && tp->owningArena != arena)
        return;

    if (TypePack* tpp = getMutable<TypePack>(tp))
    {
        for (TypeId& tv : tpp->head)
            tv = replace(tv);
        if (tpp->tail)
            tpp->tail = replace(*tpp->tail);
    }
    else if (VariadicTypePack* vtp = getMutable<VariadicTypePack>(tp))
    {
        vtp->ty = replace(vtp->ty);
    }
}

} // namespace Luau
