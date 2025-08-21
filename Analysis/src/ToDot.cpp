// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/ToDot.h"

#include "Luau/ToString.h"
#include "Luau/TypePack.h"
#include "Luau/Type.h"
#include "Luau/TypeFunction.h"
#include "Luau/StringUtils.h"

#include <unordered_map>
#include <unordered_set>

LUAU_FASTFLAG(LuauSolverV2)

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

    return get<PrimitiveType>(ty) || get<AnyType>(ty) || get<UnknownType>(ty) || get<NeverType>(ty);
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
        else if (get<UnknownType>(ty))
            formatAppend(result, "n%d [label=\"unknown\"];\n", index);
        else if (get<NeverType>(ty))
            formatAppend(result, "n%d [label=\"never\"];\n", index);
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

    auto go = [&](auto&& t)
    {
        using T = std::decay_t<decltype(t)>;

        if constexpr (std::is_same_v<T, BoundType>)
        {
            formatAppend(result, "BoundType %d", index);
            finishNodeLabel(ty);
            finishNode();

            visitChild(t.boundTo, index);
        }
        else if constexpr (std::is_same_v<T, BlockedType>)
        {
            formatAppend(result, "BlockedType %d", index);
            finishNodeLabel(ty);
            finishNode();
        }
        else if constexpr (std::is_same_v<T, FunctionType>)
        {
            formatAppend(result, "FunctionType %d", index);
            finishNodeLabel(ty);
            finishNode();

            visitChild(t.argTypes, index, "arg");
            visitChild(t.retTypes, index, "ret");
        }
        else if constexpr (std::is_same_v<T, TableType>)
        {
            if (t.name)
                formatAppend(result, "TableType %s", t.name->c_str());
            else if (t.syntheticName)
                formatAppend(result, "TableType %s", t.syntheticName->c_str());
            else
                formatAppend(result, "TableType %d", index);
            finishNodeLabel(ty);
            finishNode();

            if (t.boundTo)
                return visitChild(*t.boundTo, index, "boundTo");

            for (const auto& [name, prop] : t.props)
            {
                if (prop.isShared())
                    visitChild(*prop.readTy, index, name.c_str());
                else
                {
                    if (prop.readTy)
                    {
                        std::string readName = "read " + name;
                        visitChild(*prop.readTy, index, readName.c_str());
                    }

                    if (prop.writeTy)
                    {
                        std::string writeName = "write " + name;
                        visitChild(*prop.writeTy, index, writeName.c_str());
                    }
                }
            }
            if (t.indexer)
            {
                visitChild(t.indexer->indexType, index, "[index]");
                visitChild(t.indexer->indexResultType, index, "[value]");
            }
            for (TypeId itp : t.instantiatedTypeParams)
                visitChild(itp, index, "typeParam");

            for (TypePackId itp : t.instantiatedTypePackParams)
                visitChild(itp, index, "typePackParam");
        }
        else if constexpr (std::is_same_v<T, MetatableType>)
        {
            formatAppend(result, "MetatableType %d", index);
            finishNodeLabel(ty);
            finishNode();

            visitChild(t.table, index, "table");
            visitChild(t.metatable, index, "metatable");
        }
        else if constexpr (std::is_same_v<T, UnionType>)
        {
            formatAppend(result, "UnionType %d", index);
            finishNodeLabel(ty);
            finishNode();

            for (TypeId opt : t.options)
                visitChild(opt, index);
        }
        else if constexpr (std::is_same_v<T, IntersectionType>)
        {
            formatAppend(result, "IntersectionType %d", index);
            finishNodeLabel(ty);
            finishNode();

            for (TypeId part : t.parts)
                visitChild(part, index);
        }
        else if constexpr (std::is_same_v<T, LazyType>)
        {
            formatAppend(result, "LazyType %d", index);
            finishNodeLabel(ty);
            finishNode();
        }
        else if constexpr (std::is_same_v<T, PendingExpansionType>)
        {
            formatAppend(result, "PendingExpansionType %d", index);
            finishNodeLabel(ty);
            finishNode();
        }
        else if constexpr (std::is_same_v<T, GenericType>)
        {
            if (t.explicitName)
                formatAppend(result, "GenericType %s", t.name.c_str());
            else
                formatAppend(result, "GenericType %d", index);
            finishNodeLabel(ty);
            finishNode();
        }
        else if constexpr (std::is_same_v<T, FreeType>)
        {
            formatAppend(result, "FreeType %d", index);
            finishNodeLabel(ty);
            finishNode();

            if (FFlag::LuauSolverV2)
            {
                if (!get<NeverType>(t.lowerBound))
                    visitChild(t.lowerBound, index, "[lowerBound]");

                if (!get<UnknownType>(t.upperBound))
                    visitChild(t.upperBound, index, "[upperBound]");
            }
        }
        else if constexpr (std::is_same_v<T, AnyType>)
        {
            formatAppend(result, "AnyType %d", index);
            finishNodeLabel(ty);
            finishNode();
        }
        else if constexpr (std::is_same_v<T, NoRefineType>)
        {
            formatAppend(result, "NoRefineType %d", index);
            finishNodeLabel(ty);
            finishNode();
        }
        else if constexpr (std::is_same_v<T, UnknownType>)
        {
            formatAppend(result, "UnknownType %d", index);
            finishNodeLabel(ty);
            finishNode();
        }
        else if constexpr (std::is_same_v<T, NeverType>)
        {
            formatAppend(result, "NeverType %d", index);
            finishNodeLabel(ty);
            finishNode();
        }
        else if constexpr (std::is_same_v<T, PrimitiveType>)
        {
            formatAppend(result, "PrimitiveType %s", toString(ty).c_str());
            finishNodeLabel(ty);
            finishNode();
        }
        else if constexpr (std::is_same_v<T, ErrorType>)
        {
            formatAppend(result, "ErrorType %d", index);
            finishNodeLabel(ty);
            finishNode();
        }
        else if constexpr (std::is_same_v<T, ExternType>)
        {
            formatAppend(result, "ExternType %s", t.name.c_str());
            finishNodeLabel(ty);
            finishNode();

            for (const auto& [name, prop] : t.props)
            {
                if (prop.isShared())
                    visitChild(*prop.readTy, index, name.c_str());
                else
                {
                    if (prop.readTy)
                    {
                        std::string readName = "read " + name;
                        visitChild(*prop.readTy, index, readName.c_str());
                    }

                    if (prop.writeTy)
                    {
                        std::string writeName = "write " + name;
                        visitChild(*prop.writeTy, index, writeName.c_str());
                    }
                }
            }

            if (t.parent)
                visitChild(*t.parent, index, "[parent]");

            if (t.metatable)
                visitChild(*t.metatable, index, "[metatable]");

            if (t.indexer)
            {
                visitChild(t.indexer->indexType, index, "[index]");
                visitChild(t.indexer->indexResultType, index, "[value]");
            }
        }
        else if constexpr (std::is_same_v<T, SingletonType>)
        {
            std::string res;

            if (const StringSingleton* ss = get<StringSingleton>(&t))
            {
                // Don't put in quotes anywhere. If it's outside of the call to escape,
                // then it's invalid syntax. If it's inside, then escaping is super noisy.
                res = "string: " + escape(ss->value);
            }
            else if (const BooleanSingleton* bs = get<BooleanSingleton>(&t))
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
        else if constexpr (std::is_same_v<T, NegationType>)
        {
            formatAppend(result, "NegationType %d", index);
            finishNodeLabel(ty);
            finishNode();

            visitChild(t.ty, index, "[negated]");
        }
        else if constexpr (std::is_same_v<T, TypeFunctionInstanceType>)
        {
            formatAppend(result, "TypeFunctionInstanceType %s %d", t.function->name.c_str(), index);
            finishNodeLabel(ty);
            finishNode();

            for (TypeId tyParam : t.typeArguments)
                visitChild(tyParam, index);

            for (TypePackId tpParam : t.packArguments)
                visitChild(tpParam, index);
        }
        else
            static_assert(always_false_v<T>, "unknown type kind");
    };

    visit(go, ty->ty);
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
    else if (get<ErrorTypePack>(tp))
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
