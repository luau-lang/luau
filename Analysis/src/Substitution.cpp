// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Substitution.h"

#include "Luau/Common.h"
#include "Luau/Clone.h"
#include "Luau/TxnLog.h"

#include <algorithm>
#include <stdexcept>

LUAU_FASTINTVARIABLE(LuauTarjanChildLimit, 10000)
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);
LUAU_FASTINTVARIABLE(LuauTarjanPreallocationSize, 256);
LUAU_FASTFLAG(LuauReusableSubstitutions)

namespace Luau
{

static TypeId shallowClone(TypeId ty, TypeArena& dest, const TxnLog* log, bool alwaysClone)
{
    auto go = [ty, &dest, alwaysClone](auto&& a) {
        using T = std::decay_t<decltype(a)>;

        // The pointer identities of free and local types is very important.
        // We decline to copy them.
        if constexpr (std::is_same_v<T, FreeType>)
            return ty;
        else if constexpr (std::is_same_v<T, BoundType>)
        {
            // This should never happen, but visit() cannot see it.
            LUAU_ASSERT(!"shallowClone didn't follow its argument!");
            return dest.addType(BoundType{a.boundTo});
        }
        else if constexpr (std::is_same_v<T, GenericType>)
            return dest.addType(a);
        else if constexpr (std::is_same_v<T, BlockedType>)
            return dest.addType(a);
        else if constexpr (std::is_same_v<T, PrimitiveType>)
        {
            LUAU_ASSERT(ty->persistent);
            return ty;
        }
        else if constexpr (std::is_same_v<T, PendingExpansionType>)
        {
            PendingExpansionType clone = PendingExpansionType{a.prefix, a.name, a.typeArguments, a.packArguments};
            return dest.addType(std::move(clone));
        }
        else if constexpr (std::is_same_v<T, AnyType>)
        {
            LUAU_ASSERT(ty->persistent);
            return ty;
        }
        else if constexpr (std::is_same_v<T, ErrorType>)
        {
            LUAU_ASSERT(ty->persistent);
            return ty;
        }
        else if constexpr (std::is_same_v<T, UnknownType>)
        {
            LUAU_ASSERT(ty->persistent);
            return ty;
        }
        else if constexpr (std::is_same_v<T, NeverType>)
        {
            LUAU_ASSERT(ty->persistent);
            return ty;
        }
        else if constexpr (std::is_same_v<T, LazyType>)
            return ty;
        else if constexpr (std::is_same_v<T, SingletonType>)
            return dest.addType(a);
        else if constexpr (std::is_same_v<T, FunctionType>)
        {
            FunctionType clone = FunctionType{a.level, a.scope, a.argTypes, a.retTypes, a.definition, a.hasSelf};
            clone.generics = a.generics;
            clone.genericPacks = a.genericPacks;
            clone.magicFunction = a.magicFunction;
            clone.dcrMagicFunction = a.dcrMagicFunction;
            clone.dcrMagicRefinement = a.dcrMagicRefinement;
            clone.tags = a.tags;
            clone.argNames = a.argNames;
            clone.isCheckedFunction = a.isCheckedFunction;
            return dest.addType(std::move(clone));
        }
        else if constexpr (std::is_same_v<T, TableType>)
        {
            LUAU_ASSERT(!a.boundTo);
            TableType clone = TableType{a.props, a.indexer, a.level, a.scope, a.state};
            clone.definitionModuleName = a.definitionModuleName;
            clone.definitionLocation = a.definitionLocation;
            clone.name = a.name;
            clone.syntheticName = a.syntheticName;
            clone.instantiatedTypeParams = a.instantiatedTypeParams;
            clone.instantiatedTypePackParams = a.instantiatedTypePackParams;
            clone.tags = a.tags;
            return dest.addType(std::move(clone));
        }
        else if constexpr (std::is_same_v<T, MetatableType>)
        {
            MetatableType clone = MetatableType{a.table, a.metatable};
            clone.syntheticName = a.syntheticName;
            return dest.addType(std::move(clone));
        }
        else if constexpr (std::is_same_v<T, UnionType>)
        {
            UnionType clone;
            clone.options = a.options;
            return dest.addType(std::move(clone));
        }
        else if constexpr (std::is_same_v<T, IntersectionType>)
        {
            IntersectionType clone;
            clone.parts = a.parts;
            return dest.addType(std::move(clone));
        }
        else if constexpr (std::is_same_v<T, ClassType>)
        {
            if (alwaysClone)
            {
                ClassType clone{a.name, a.props, a.parent, a.metatable, a.tags, a.userData, a.definitionModuleName, a.definitionLocation, a.indexer};
                return dest.addType(std::move(clone));
            }
            else
                return ty;
        }
        else if constexpr (std::is_same_v<T, NegationType>)
            return dest.addType(NegationType{a.ty});
        else if constexpr (std::is_same_v<T, TypeFunctionInstanceType>)
        {
            TypeFunctionInstanceType clone{a.function, a.typeArguments, a.packArguments};
            return dest.addType(std::move(clone));
        }
        else
            static_assert(always_false_v<T>, "Non-exhaustive shallowClone switch");
    };

    ty = log->follow(ty);

    if (auto pty = log->pending(ty))
        ty = &pty->pending;

    TypeId resTy = visit(go, ty->ty);
    if (resTy != ty)
        asMutable(resTy)->documentationSymbol = ty->documentationSymbol;

    return resTy;
}

Tarjan::Tarjan()
    : typeToIndex(nullptr, FFlag::LuauReusableSubstitutions ? FInt::LuauTarjanPreallocationSize : 0)
    , packToIndex(nullptr, FFlag::LuauReusableSubstitutions ? FInt::LuauTarjanPreallocationSize : 0)
{
    nodes.reserve(FInt::LuauTarjanPreallocationSize);
    stack.reserve(FInt::LuauTarjanPreallocationSize);
    edgesTy.reserve(FInt::LuauTarjanPreallocationSize);
    edgesTp.reserve(FInt::LuauTarjanPreallocationSize);
    worklist.reserve(FInt::LuauTarjanPreallocationSize);
}

void Tarjan::visitChildren(TypeId ty, int index)
{
    LUAU_ASSERT(ty == log->follow(ty));

    if (ignoreChildrenVisit(ty))
        return;

    if (auto pty = log->pending(ty))
        ty = &pty->pending;

    if (const FunctionType* ftv = get<FunctionType>(ty))
    {
        for (TypeId generic : ftv->generics)
            visitChild(generic);
        for (TypePackId genericPack : ftv->genericPacks)
            visitChild(genericPack);

        visitChild(ftv->argTypes);
        visitChild(ftv->retTypes);
    }
    else if (const TableType* ttv = get<TableType>(ty))
    {
        LUAU_ASSERT(!ttv->boundTo);
        for (const auto& [name, prop] : ttv->props)
        {
            if (FFlag::DebugLuauDeferredConstraintResolution)
            {
                visitChild(prop.readTy);
                visitChild(prop.writeTy);
            }
            else
                visitChild(prop.type());
        }

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
    else if (const MetatableType* mtv = get<MetatableType>(ty))
    {
        visitChild(mtv->table);
        visitChild(mtv->metatable);
    }
    else if (const UnionType* utv = get<UnionType>(ty))
    {
        for (TypeId opt : utv->options)
            visitChild(opt);
    }
    else if (const IntersectionType* itv = get<IntersectionType>(ty))
    {
        for (TypeId part : itv->parts)
            visitChild(part);
    }
    else if (const PendingExpansionType* petv = get<PendingExpansionType>(ty))
    {
        for (TypeId a : petv->typeArguments)
            visitChild(a);

        for (TypePackId a : petv->packArguments)
            visitChild(a);
    }
    else if (const TypeFunctionInstanceType* tfit = get<TypeFunctionInstanceType>(ty))
    {
        for (TypeId a : tfit->typeArguments)
            visitChild(a);

        for (TypePackId a : tfit->packArguments)
            visitChild(a);
    }
    else if (const ClassType* ctv = get<ClassType>(ty))
    {
        for (const auto& [name, prop] : ctv->props)
            visitChild(prop.type());

        if (ctv->parent)
            visitChild(*ctv->parent);

        if (ctv->metatable)
            visitChild(*ctv->metatable);

        if (ctv->indexer)
        {
            visitChild(ctv->indexer->indexType);
            visitChild(ctv->indexer->indexResultType);
        }
    }
    else if (const NegationType* ntv = get<NegationType>(ty))
    {
        visitChild(ntv->ty);
    }
}

void Tarjan::visitChildren(TypePackId tp, int index)
{
    LUAU_ASSERT(tp == log->follow(tp));

    if (ignoreChildrenVisit(tp))
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

    auto [index, fresh] = typeToIndex.try_insert(ty, false);

    if (fresh)
    {
        index = int(nodes.size());
        nodes.push_back({ty, nullptr, false, false, index});
    }

    return {index, fresh};
}

std::pair<int, bool> Tarjan::indexify(TypePackId tp)
{
    tp = log->follow(tp);

    auto [index, fresh] = packToIndex.try_insert(tp, false);

    if (fresh)
    {
        index = int(nodes.size());
        nodes.push_back({nullptr, tp, false, false, index});
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
            if (childLimit > 0 && childLimit <= childCount)
                return TarjanResult::TooManyChildren;

            stack.push_back(index);

            nodes[index].onStack = true;

            currEdge = int(edgesTy.size());

            // Fill in edge list of this vertex
            if (TypeId ty = nodes[index].ty)
                visitChildren(ty, index);
            else if (TypePackId tp = nodes[index].tp)
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
            else if (nodes[childIndex].onStack)
            {
                nodes[index].lowlink = std::min(nodes[index].lowlink, childIndex);
            }

            visitEdge(childIndex, index);
        }

        if (foundFresh)
            continue;

        if (nodes[index].lowlink == index)
        {
            visitSCC(index);
            while (!stack.empty())
            {
                int popped = stack.back();
                stack.pop_back();
                nodes[popped].onStack = false;
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

            nodes[parentIndex].lowlink = std::min(nodes[parentIndex].lowlink, nodes[index].lowlink);
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

void Tarjan::clearTarjan(const TxnLog* log)
{
    if (FFlag::LuauReusableSubstitutions)
    {
        typeToIndex.clear(~0u);
        packToIndex.clear(~0u);
    }
    else
    {
        typeToIndex.clear();
        packToIndex.clear();
    }

    nodes.clear();

    stack.clear();

    if (FFlag::LuauReusableSubstitutions)
    {
        childCount = 0;
        // childLimit setting stays the same

        this->log = log;
    }

    edgesTy.clear();
    edgesTp.clear();
    worklist.clear();
}

bool Tarjan::getDirty(int index)
{
    LUAU_ASSERT(size_t(index) < nodes.size());
    return nodes[index].dirty;
}

void Tarjan::setDirty(int index, bool d)
{
    LUAU_ASSERT(size_t(index) < nodes.size());
    nodes[index].dirty = d;
}

void Tarjan::visitEdge(int index, int parentIndex)
{
    if (getDirty(index))
        setDirty(parentIndex, true);
}

void Tarjan::visitSCC(int index)
{
    bool d = getDirty(index);

    for (auto it = stack.rbegin(); !d && it != stack.rend(); it++)
    {
        TarjanNode& node = nodes[*it];

        if (TypeId ty = node.ty)
            d = isDirty(ty);
        else if (TypePackId tp = node.tp)
            d = isDirty(tp);

        if (*it == index)
            break;
    }

    if (!d)
        return;

    for (auto it = stack.rbegin(); it != stack.rend(); it++)
    {
        setDirty(*it, true);

        TarjanNode& node = nodes[*it];

        if (TypeId ty = node.ty)
            foundDirty(ty);
        else if (TypePackId tp = node.tp)
            foundDirty(tp);

        if (*it == index)
            return;
    }
}

TarjanResult Tarjan::findDirty(TypeId ty)
{
    return visitRoot(ty);
}

TarjanResult Tarjan::findDirty(TypePackId tp)
{
    return visitRoot(tp);
}

Substitution::Substitution(const TxnLog* log_, TypeArena* arena)
    : arena(arena)
{
    log = log_;
    LUAU_ASSERT(log);
}

void Substitution::dontTraverseInto(TypeId ty)
{
    noTraverseTypes.insert(ty);
}

void Substitution::dontTraverseInto(TypePackId tp)
{
    noTraverseTypePacks.insert(tp);
}

std::optional<TypeId> Substitution::substitute(TypeId ty)
{
    ty = log->follow(ty);

    // clear algorithm state for reentrancy
    clearTarjan(log);

    auto result = findDirty(ty);
    if (result != TarjanResult::Ok)
        return std::nullopt;

    for (auto [oldTy, newTy] : newTypes)
    {
        if (!ignoreChildren(oldTy) && !replacedTypes.contains(newTy))
        {
            if (!noTraverseTypes.contains(newTy))
                replaceChildren(newTy);
            replacedTypes.insert(newTy);
        }
    }
    for (auto [oldTp, newTp] : newPacks)
    {
        if (!ignoreChildren(oldTp) && !replacedTypePacks.contains(newTp))
        {
            if (!noTraverseTypePacks.contains(newTp))
                replaceChildren(newTp);
            replacedTypePacks.insert(newTp);
        }
    }
    TypeId newTy = replace(ty);
    return newTy;
}

std::optional<TypePackId> Substitution::substitute(TypePackId tp)
{
    tp = log->follow(tp);

    // clear algorithm state for reentrancy
    clearTarjan(log);

    auto result = findDirty(tp);
    if (result != TarjanResult::Ok)
        return std::nullopt;

    for (auto [oldTy, newTy] : newTypes)
    {
        if (!ignoreChildren(oldTy) && !replacedTypes.contains(newTy))
        {
            if (!noTraverseTypes.contains(newTy))
                replaceChildren(newTy);
            replacedTypes.insert(newTy);
        }
    }
    for (auto [oldTp, newTp] : newPacks)
    {
        if (!ignoreChildren(oldTp) && !replacedTypePacks.contains(newTp))
        {
            if (!noTraverseTypePacks.contains(newTp))
                replaceChildren(newTp);
            replacedTypePacks.insert(newTp);
        }
    }
    TypePackId newTp = replace(tp);
    return newTp;
}

void Substitution::resetState(const TxnLog* log, TypeArena* arena)
{
    LUAU_ASSERT(FFlag::LuauReusableSubstitutions);

    clearTarjan(log);

    this->arena = arena;

    newTypes.clear();
    newPacks.clear();
    replacedTypes.clear();
    replacedTypePacks.clear();

    noTraverseTypes.clear();
    noTraverseTypePacks.clear();
}

TypeId Substitution::clone(TypeId ty)
{
    return shallowClone(ty, *arena, log, /* alwaysClone */ true);
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
        clone.hidden = vtp->hidden;
        return addTypePack(std::move(clone));
    }
    else if (const TypeFunctionInstanceTypePack* tfitp = get<TypeFunctionInstanceTypePack>(tp))
    {
        TypeFunctionInstanceTypePack clone{
            tfitp->function, std::vector<TypeId>(tfitp->typeArguments.size()), std::vector<TypePackId>(tfitp->packArguments.size())};
        clone.typeArguments.assign(tfitp->typeArguments.begin(), tfitp->typeArguments.end());
        clone.packArguments.assign(tfitp->packArguments.begin(), tfitp->packArguments.end());
        return addTypePack(std::move(clone));
    }
    else
        return addTypePack(*tp);
}

void Substitution::foundDirty(TypeId ty)
{
    ty = log->follow(ty);

    if (newTypes.contains(ty))
        return;

    if (isDirty(ty))
        newTypes[ty] = follow(clean(ty));
    else
        newTypes[ty] = follow(clone(ty));
}

void Substitution::foundDirty(TypePackId tp)
{
    tp = log->follow(tp);

    if (newPacks.contains(tp))
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
    LUAU_ASSERT(ty == log->follow(ty));

    if (ignoreChildren(ty))
        return;

    if (ty->owningArena != arena)
        return;

    if (FunctionType* ftv = getMutable<FunctionType>(ty))
    {
        for (TypeId& generic : ftv->generics)
            generic = replace(generic);
        for (TypePackId& genericPack : ftv->genericPacks)
            genericPack = replace(genericPack);

        ftv->argTypes = replace(ftv->argTypes);
        ftv->retTypes = replace(ftv->retTypes);
    }
    else if (TableType* ttv = getMutable<TableType>(ty))
    {
        LUAU_ASSERT(!ttv->boundTo);
        for (auto& [name, prop] : ttv->props)
        {
            if (FFlag::DebugLuauDeferredConstraintResolution)
            {
                if (prop.readTy)
                    prop.readTy = replace(prop.readTy);
                if (prop.writeTy)
                    prop.writeTy = replace(prop.writeTy);
            }
            else
                prop.setType(replace(prop.type()));
        }

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
    else if (MetatableType* mtv = getMutable<MetatableType>(ty))
    {
        mtv->table = replace(mtv->table);
        mtv->metatable = replace(mtv->metatable);
    }
    else if (UnionType* utv = getMutable<UnionType>(ty))
    {
        for (TypeId& opt : utv->options)
            opt = replace(opt);
    }
    else if (IntersectionType* itv = getMutable<IntersectionType>(ty))
    {
        for (TypeId& part : itv->parts)
            part = replace(part);
    }
    else if (PendingExpansionType* petv = getMutable<PendingExpansionType>(ty))
    {
        for (TypeId& a : petv->typeArguments)
            a = replace(a);

        for (TypePackId& a : petv->packArguments)
            a = replace(a);
    }
    else if (TypeFunctionInstanceType* tfit = getMutable<TypeFunctionInstanceType>(ty))
    {
        for (TypeId& a : tfit->typeArguments)
            a = replace(a);

        for (TypePackId& a : tfit->packArguments)
            a = replace(a);
    }
    else if (ClassType* ctv = getMutable<ClassType>(ty))
    {
        for (auto& [name, prop] : ctv->props)
            prop.setType(replace(prop.type()));

        if (ctv->parent)
            ctv->parent = replace(*ctv->parent);

        if (ctv->metatable)
            ctv->metatable = replace(*ctv->metatable);

        if (ctv->indexer)
        {
            ctv->indexer->indexType = replace(ctv->indexer->indexType);
            ctv->indexer->indexResultType = replace(ctv->indexer->indexResultType);
        }
    }
    else if (NegationType* ntv = getMutable<NegationType>(ty))
    {
        ntv->ty = replace(ntv->ty);
    }
}

void Substitution::replaceChildren(TypePackId tp)
{
    LUAU_ASSERT(tp == log->follow(tp));

    if (ignoreChildren(tp))
        return;

    if (tp->owningArena != arena)
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
    else if (TypeFunctionInstanceTypePack* tfitp = getMutable<TypeFunctionInstanceTypePack>(tp))
    {
        for (TypeId& t : tfitp->typeArguments)
            t = replace(t);

        for (TypePackId& t : tfitp->packArguments)
            t = replace(t);
    }
}

} // namespace Luau
