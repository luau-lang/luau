// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Substitution.h"

#include "Luau/Common.h"

#include <algorithm>
#include <stdexcept>

LUAU_FASTINTVARIABLE(LuauTarjanChildLimit, 1000)

namespace Luau
{

void Tarjan::visitChildren(TypeId ty, int index)
{
    ty = follow(ty);

    if (ignoreChildren(ty))
        return;

    if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(ty))
    {
        visitChild(ftv->argTypes);
        visitChild(ftv->retType);
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
}

void Tarjan::visitChildren(TypePackId tp, int index)
{
    tp = follow(tp);

    if (ignoreChildren(tp))
        return;

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
    ty = follow(ty);

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
    tp = follow(tp);

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
    ty = follow(ty);

    edgesTy.push_back(ty);
    edgesTp.push_back(nullptr);
}

void Tarjan::visitChild(TypePackId tp)
{
    tp = follow(tp);

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
            if (FInt::LuauTarjanChildLimit > 0 && FInt::LuauTarjanChildLimit < childCount)
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

void Tarjan::clear()
{
    typeToIndex.clear();
    indexToType.clear();
    packToIndex.clear();
    indexToPack.clear();
    lowlink.clear();
    stack.clear();
    onStack.clear();

    edgesTy.clear();
    edgesTp.clear();
    worklist.clear();
}

TarjanResult Tarjan::visitRoot(TypeId ty)
{
    childCount = 0;
    ty = follow(ty);

    clear();
    auto [index, fresh] = indexify(ty);
    worklist.push_back({index, -1, -1});
    return loop();
}

TarjanResult Tarjan::visitRoot(TypePackId tp)
{
    childCount = 0;
    tp = follow(tp);

    clear();
    auto [index, fresh] = indexify(tp);
    worklist.push_back({index, -1, -1});
    return loop();
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
    dirty.clear();
    return visitRoot(ty);
}

TarjanResult FindDirty::findDirty(TypePackId tp)
{
    dirty.clear();
    return visitRoot(tp);
}

std::optional<TypeId> Substitution::substitute(TypeId ty)
{
    ty = follow(ty);
    newTypes.clear();
    newPacks.clear();

    auto result = findDirty(ty);
    if (result != TarjanResult::Ok)
        return std::nullopt;

    for (auto [oldTy, newTy] : newTypes)
        if (!ignoreChildren(oldTy))
            replaceChildren(newTy);
    for (auto [oldTp, newTp] : newPacks)
        if (!ignoreChildren(oldTp))
            replaceChildren(newTp);
    TypeId newTy = replace(ty);
    return newTy;
}

std::optional<TypePackId> Substitution::substitute(TypePackId tp)
{
    tp = follow(tp);
    newTypes.clear();
    newPacks.clear();

    auto result = findDirty(tp);
    if (result != TarjanResult::Ok)
        return std::nullopt;

    for (auto [oldTy, newTy] : newTypes)
        if (!ignoreChildren(oldTy))
            replaceChildren(newTy);
    for (auto [oldTp, newTp] : newPacks)
        if (!ignoreChildren(oldTp))
            replaceChildren(newTp);
    TypePackId newTp = replace(tp);
    return newTp;
}

TypeId Substitution::clone(TypeId ty)
{
    ty = follow(ty);

    TypeId result = ty;

    if (const FunctionTypeVar* ftv = get<FunctionTypeVar>(ty))
    {
        FunctionTypeVar clone = FunctionTypeVar{ftv->level, ftv->argTypes, ftv->retType, ftv->definition, ftv->hasSelf};
        clone.generics = ftv->generics;
        clone.genericPacks = ftv->genericPacks;
        clone.magicFunction = ftv->magicFunction;
        clone.tags = ftv->tags;
        clone.argNames = ftv->argNames;
        result = addType(std::move(clone));
    }
    else if (const TableTypeVar* ttv = get<TableTypeVar>(ty))
    {
        LUAU_ASSERT(!ttv->boundTo);
        TableTypeVar clone = TableTypeVar{ttv->props, ttv->indexer, ttv->level, ttv->state};
        clone.methodDefinitionLocations = ttv->methodDefinitionLocations;
        clone.definitionModuleName = ttv->definitionModuleName;
        clone.name = ttv->name;
        clone.syntheticName = ttv->syntheticName;
        clone.instantiatedTypeParams = ttv->instantiatedTypeParams;
        clone.instantiatedTypePackParams = ttv->instantiatedTypePackParams;
        clone.tags = ttv->tags;
        result = addType(std::move(clone));
    }
    else if (const MetatableTypeVar* mtv = get<MetatableTypeVar>(ty))
    {
        MetatableTypeVar clone = MetatableTypeVar{mtv->table, mtv->metatable};
        clone.syntheticName = mtv->syntheticName;
        result = addType(std::move(clone));
    }
    else if (const UnionTypeVar* utv = get<UnionTypeVar>(ty))
    {
        UnionTypeVar clone;
        clone.options = utv->options;
        result = addType(std::move(clone));
    }
    else if (const IntersectionTypeVar* itv = get<IntersectionTypeVar>(ty))
    {
        IntersectionTypeVar clone;
        clone.parts = itv->parts;
        result = addType(std::move(clone));
    }

    asMutable(result)->documentationSymbol = ty->documentationSymbol;
    return result;
}

TypePackId Substitution::clone(TypePackId tp)
{
    tp = follow(tp);
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
        return addTypePack(std::move(clone));
    }
    else
        return tp;
}

void Substitution::foundDirty(TypeId ty)
{
    ty = follow(ty);
    if (isDirty(ty))
        newTypes[ty] = clean(ty);
    else
        newTypes[ty] = clone(ty);
}

void Substitution::foundDirty(TypePackId tp)
{
    tp = follow(tp);
    if (isDirty(tp))
        newPacks[tp] = clean(tp);
    else
        newPacks[tp] = clone(tp);
}

TypeId Substitution::replace(TypeId ty)
{
    ty = follow(ty);
    if (TypeId* prevTy = newTypes.find(ty))
        return *prevTy;
    else
        return ty;
}

TypePackId Substitution::replace(TypePackId tp)
{
    tp = follow(tp);
    if (TypePackId* prevTp = newPacks.find(tp))
        return *prevTp;
    else
        return tp;
}

void Substitution::replaceChildren(TypeId ty)
{
    ty = follow(ty);

    if (ignoreChildren(ty))
        return;

    if (FunctionTypeVar* ftv = getMutable<FunctionTypeVar>(ty))
    {
        ftv->argTypes = replace(ftv->argTypes);
        ftv->retType = replace(ftv->retType);
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
}

void Substitution::replaceChildren(TypePackId tp)
{
    tp = follow(tp);

    if (ignoreChildren(tp))
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
