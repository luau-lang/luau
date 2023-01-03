// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ToDot.h"

#include "Luau/ToString.h"
#include "Luau/TypePack.h"
#include "Luau/Type.h"
#include "Luau/StringUtils.h"

#include <unordered_map>
#include <unordered_set>

namespace Luau
{

namespace
{

struct StateDot
{
    StateDot(ToDotOptions opts)
        : opts(opts)
    {
    }

    ToDotOptions opts;

    std::unordered_set<TypeId> seenTy;
    std::unordered_set<TypePackId> seenTp;
    std::unordered_map<TypeId, int> tyToIndex;
    std::unordered_map<TypePackId, int> tpToIndex;
    int nextIndex = 1;
    std::string result;

    bool canDuplicatePrimitive(TypeId ty);

    void visitChildren(TypeId ty, int index);
    void visitChildren(TypePackId ty, int index);

    void visitChild(TypeId ty, int parentIndex, const char* linkName = nullptr);
    void visitChild(TypePackId tp, int parentIndex, const char* linkName = nullptr);

    void startNode(int index);
    void finishNode();

    void startNodeLabel();
    void finishNodeLabel(TypeId ty);
    void finishNodeLabel(TypePackId tp);
};

bool StateDot::canDuplicatePrimitive(TypeId ty)
{
    if (get<BoundType>(ty))
        return false;

    return get<PrimitiveType>(ty) || get<AnyType>(ty);
}

void StateDot::visitChild(TypeId ty, int parentIndex, const char* linkName)
{
    if (!tyToIndex.count(ty) || (opts.duplicatePrimitives && canDuplicatePrimitive(ty)))
        tyToIndex[ty] = nextIndex++;

    int index = tyToIndex[ty];

    if (parentIndex != 0)
    {
        if (linkName)
            formatAppend(result, "n%d -> n%d [label=\"%s\"];\n", parentIndex, index, linkName);
        else
            formatAppend(result, "n%d -> n%d;\n", parentIndex, index);
    }

    if (opts.duplicatePrimitives && canDuplicatePrimitive(ty))
    {
        if (get<PrimitiveType>(ty))
            formatAppend(result, "n%d [label=\"%s\"];\n", index, toString(ty).c_str());
        else if (get<AnyType>(ty))
            formatAppend(result, "n%d [label=\"any\"];\n", index);
    }
    else
    {
        visitChildren(ty, index);
    }
}

void StateDot::visitChild(TypePackId tp, int parentIndex, const char* linkName)
{
    if (!tpToIndex.count(tp))
        tpToIndex[tp] = nextIndex++;

    if (parentIndex != 0)
    {
        if (linkName)
            formatAppend(result, "n%d -> n%d [label=\"%s\"];\n", parentIndex, tpToIndex[tp], linkName);
        else
            formatAppend(result, "n%d -> n%d;\n", parentIndex, tpToIndex[tp]);
    }

    visitChildren(tp, tpToIndex[tp]);
}

void StateDot::startNode(int index)
{
    formatAppend(result, "n%d [", index);
}

void StateDot::finishNode()
{
    formatAppend(result, "];\n");
}

void StateDot::startNodeLabel()
{
    formatAppend(result, "label=\"");
}

void StateDot::finishNodeLabel(TypeId ty)
{
    if (opts.showPointers)
        formatAppend(result, "\n0x%p", ty);
    // additional common attributes can be added here as well
    result += "\"";
}

void StateDot::finishNodeLabel(TypePackId tp)
{
    if (opts.showPointers)
        formatAppend(result, "\n0x%p", tp);
    // additional common attributes can be added here as well
    result += "\"";
}

void StateDot::visitChildren(TypeId ty, int index)
{
    if (seenTy.count(ty))
        return;
    seenTy.insert(ty);

    startNode(index);
    startNodeLabel();

    if (const BoundType* btv = get<BoundType>(ty))
    {
        formatAppend(result, "BoundType %d", index);
        finishNodeLabel(ty);
        finishNode();

        visitChild(btv->boundTo, index);
    }
    else if (const FunctionType* ftv = get<FunctionType>(ty))
    {
        formatAppend(result, "FunctionType %d", index);
        finishNodeLabel(ty);
        finishNode();

        visitChild(ftv->argTypes, index, "arg");
        visitChild(ftv->retTypes, index, "ret");
    }
    else if (const TableType* ttv = get<TableType>(ty))
    {
        if (ttv->name)
            formatAppend(result, "TableType %s", ttv->name->c_str());
        else if (ttv->syntheticName)
            formatAppend(result, "TableType %s", ttv->syntheticName->c_str());
        else
            formatAppend(result, "TableType %d", index);
        finishNodeLabel(ty);
        finishNode();

        if (ttv->boundTo)
            return visitChild(*ttv->boundTo, index, "boundTo");

        for (const auto& [name, prop] : ttv->props)
            visitChild(prop.type, index, name.c_str());
        if (ttv->indexer)
        {
            visitChild(ttv->indexer->indexType, index, "[index]");
            visitChild(ttv->indexer->indexResultType, index, "[value]");
        }
        for (TypeId itp : ttv->instantiatedTypeParams)
            visitChild(itp, index, "typeParam");

        for (TypePackId itp : ttv->instantiatedTypePackParams)
            visitChild(itp, index, "typePackParam");
    }
    else if (const MetatableType* mtv = get<MetatableType>(ty))
    {
        formatAppend(result, "MetatableType %d", index);
        finishNodeLabel(ty);
        finishNode();

        visitChild(mtv->table, index, "table");
        visitChild(mtv->metatable, index, "metatable");
    }
    else if (const UnionType* utv = get<UnionType>(ty))
    {
        formatAppend(result, "UnionType %d", index);
        finishNodeLabel(ty);
        finishNode();

        for (TypeId opt : utv->options)
            visitChild(opt, index);
    }
    else if (const IntersectionType* itv = get<IntersectionType>(ty))
    {
        formatAppend(result, "IntersectionType %d", index);
        finishNodeLabel(ty);
        finishNode();

        for (TypeId part : itv->parts)
            visitChild(part, index);
    }
    else if (const GenericType* gtv = get<GenericType>(ty))
    {
        if (gtv->explicitName)
            formatAppend(result, "GenericType %s", gtv->name.c_str());
        else
            formatAppend(result, "GenericType %d", index);
        finishNodeLabel(ty);
        finishNode();
    }
    else if (const FreeType* ftv = get<FreeType>(ty))
    {
        formatAppend(result, "FreeType %d", index);
        finishNodeLabel(ty);
        finishNode();
    }
    else if (get<AnyType>(ty))
    {
        formatAppend(result, "AnyType %d", index);
        finishNodeLabel(ty);
        finishNode();
    }
    else if (get<PrimitiveType>(ty))
    {
        formatAppend(result, "PrimitiveType %s", toString(ty).c_str());
        finishNodeLabel(ty);
        finishNode();
    }
    else if (get<ErrorType>(ty))
    {
        formatAppend(result, "ErrorType %d", index);
        finishNodeLabel(ty);
        finishNode();
    }
    else if (const ClassType* ctv = get<ClassType>(ty))
    {
        formatAppend(result, "ClassType %s", ctv->name.c_str());
        finishNodeLabel(ty);
        finishNode();

        for (const auto& [name, prop] : ctv->props)
            visitChild(prop.type, index, name.c_str());

        if (ctv->parent)
            visitChild(*ctv->parent, index, "[parent]");

        if (ctv->metatable)
            visitChild(*ctv->metatable, index, "[metatable]");
    }
    else if (const SingletonType* stv = get<SingletonType>(ty))
    {
        std::string res;

        if (const StringSingleton* ss = get<StringSingleton>(stv))
        {
            // Don't put in quotes anywhere. If it's outside of the call to escape,
            // then it's invalid syntax. If it's inside, then escaping is super noisy.
            res = "string: " + escape(ss->value);
        }
        else if (const BooleanSingleton* bs = get<BooleanSingleton>(stv))
        {
            res = "boolean: ";
            res += bs->value ? "true" : "false";
        }
        else
            LUAU_ASSERT(!"unknown singleton type");

        formatAppend(result, "SingletonType %s", res.c_str());
        finishNodeLabel(ty);
        finishNode();
    }
    else
    {
        LUAU_ASSERT(!"unknown type kind");
        finishNodeLabel(ty);
        finishNode();
    }
}

void StateDot::visitChildren(TypePackId tp, int index)
{
    if (seenTp.count(tp))
        return;
    seenTp.insert(tp);

    startNode(index);
    startNodeLabel();

    if (const BoundTypePack* btp = get<BoundTypePack>(tp))
    {
        formatAppend(result, "BoundTypePack %d", index);
        finishNodeLabel(tp);
        finishNode();

        visitChild(btp->boundTo, index);
    }
    else if (const TypePack* tpp = get<TypePack>(tp))
    {
        formatAppend(result, "TypePack %d", index);
        finishNodeLabel(tp);
        finishNode();

        for (TypeId tv : tpp->head)
            visitChild(tv, index);
        if (tpp->tail)
            visitChild(*tpp->tail, index, "tail");
    }
    else if (const VariadicTypePack* vtp = get<VariadicTypePack>(tp))
    {
        formatAppend(result, "VariadicTypePack %s%d", vtp->hidden ? "hidden " : "", index);
        finishNodeLabel(tp);
        finishNode();

        visitChild(vtp->ty, index);
    }
    else if (const FreeTypePack* ftp = get<FreeTypePack>(tp))
    {
        formatAppend(result, "FreeTypePack %d", index);
        finishNodeLabel(tp);
        finishNode();
    }
    else if (const GenericTypePack* gtp = get<GenericTypePack>(tp))
    {
        if (gtp->explicitName)
            formatAppend(result, "GenericTypePack %s", gtp->name.c_str());
        else
            formatAppend(result, "GenericTypePack %d", index);
        finishNodeLabel(tp);
        finishNode();
    }
    else if (get<Unifiable::Error>(tp))
    {
        formatAppend(result, "ErrorTypePack %d", index);
        finishNodeLabel(tp);
        finishNode();
    }
    else
    {
        LUAU_ASSERT(!"unknown type pack kind");
        finishNodeLabel(tp);
        finishNode();
    }
}

} // namespace

std::string toDot(TypeId ty, const ToDotOptions& opts)
{
    StateDot state{opts};

    state.result = "digraph graphname {\n";
    state.visitChild(ty, 0);
    state.result += "}";

    return state.result;
}

std::string toDot(TypePackId tp, const ToDotOptions& opts)
{
    StateDot state{opts};

    state.result = "digraph graphname {\n";
    state.visitChild(tp, 0);
    state.result += "}";

    return state.result;
}

std::string toDot(TypeId ty)
{
    return toDot(ty, {});
}

std::string toDot(TypePackId tp)
{
    return toDot(tp, {});
}

void dumpDot(TypeId ty)
{
    printf("%s\n", toDot(ty).c_str());
}

void dumpDot(TypePackId tp)
{
    printf("%s\n", toDot(tp).c_str());
}

} // namespace Luau
