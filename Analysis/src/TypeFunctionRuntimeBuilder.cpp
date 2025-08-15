// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypeFunctionRuntimeBuilder.h"

#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/DenseHash.h"
#include "Luau/StringUtils.h"
#include "Luau/Type.h"
#include "Luau/TypeArena.h"
#include "Luau/TypeFwd.h"
#include "Luau/TypeFunctionRuntime.h"
#include "Luau/TypePack.h"
#include "Luau/TypeOrPack.h"
#include "Luau/ToString.h"

#include <optional>

// used to control the recursion limit of any operations done by user-defined type functions
// currently, controls serialization, deserialization, and `type.copy`
LUAU_DYNAMIC_FASTINTVARIABLE(LuauTypeFunctionSerdeIterationLimit, 100'000);
LUAU_FASTFLAG(LuauEmplaceNotPushBack)

namespace Luau
{

// Forked version of Clone.cpp
class TypeFunctionSerializer
{
    using SeenTypes = DenseHashMap<TypeId, TypeFunctionTypeId>;
    using SeenTypePacks = DenseHashMap<TypePackId, TypeFunctionTypePackId>;

    TypeFunctionRuntimeBuilderState* state = nullptr;
    NotNull<TypeFunctionRuntime> typeFunctionRuntime;

    // A queue of TypeFunctionTypeIds that have been serialized, but whose interior types hasn't
    // been updated to point to itself. Once all of its interior types
    // has been updated, it gets removed from the queue.

    // queue.back() should always return two of same type in their respective sides
    // For example `auto [first, second] = queue.back()`: if first is PrimitiveType,
    // second must be TypeFunctionPrimitiveType; else there should be an error
    std::vector<std::tuple<TypeOrPack, TypeFunctionKind>> queue;

    SeenTypes types;     // Mapping of TypeIds that have been shallow serialized to TypeFunctionTypeIds
    SeenTypePacks packs; // Mapping of TypePackIds that have been shallow serialized to TypeFunctionTypePackIds

    int steps = 0;

public:
    explicit TypeFunctionSerializer(TypeFunctionRuntimeBuilderState* state)
        : state(state)
        , typeFunctionRuntime(state->ctx->typeFunctionRuntime)
        , queue({})
        , types({})
        , packs({})
    {
    }

    TypeFunctionTypeId serialize(TypeId ty)
    {
        shallowSerialize(ty);
        run();

        if (hasExceededIterationLimit() || state->errors.size() != 0)
            return nullptr;

        return find(ty).value_or(nullptr);
    }

    TypeFunctionTypePackId serialize(TypePackId tp)
    {
        shallowSerialize(tp);
        run();

        if (hasExceededIterationLimit() || state->errors.size() != 0)
            return nullptr;

        return find(tp).value_or(nullptr);
    }

private:
    bool hasExceededIterationLimit() const
    {
        if (DFInt::LuauTypeFunctionSerdeIterationLimit == 0)
            return false;

        return steps + queue.size() >= size_t(DFInt::LuauTypeFunctionSerdeIterationLimit);
    }

    void run()
    {
        while (!queue.empty())
        {
            ++steps;

            if (hasExceededIterationLimit() || state->errors.size() != 0)
                break;

            auto [ty, tfti] = queue.back();
            queue.pop_back();

            serializeChildren(ty, tfti);
        }
    }

    std::optional<TypeFunctionTypeId> find(TypeId ty) const
    {
        if (auto result = types.find(ty))
            return *result;

        return std::nullopt;
    }

    std::optional<TypeFunctionTypePackId> find(TypePackId tp) const
    {
        if (auto result = packs.find(tp))
            return *result;

        return std::nullopt;
    }

    std::optional<TypeFunctionKind> find(TypeOrPack kind) const
    {
        if (auto ty = get<TypeId>(kind))
            return find(*ty);
        else if (auto tp = get<TypePackId>(kind))
            return find(*tp);
        else
        {
            LUAU_ASSERT(!"Unknown kind found at TypeFunctionRuntimeSerializer");
            return std::nullopt;
        }
    }

    TypeFunctionTypeId shallowSerialize(TypeId ty)
    {
        ty = follow(ty);

        if (auto it = find(ty))
            return *it;

        // Create a shallow serialization
        TypeFunctionTypeId target = {};
        if (auto p = get<PrimitiveType>(ty))
        {
            switch (p->type)
            {
            case PrimitiveType::NilType:
                target = typeFunctionRuntime->typeArena.allocate(TypeFunctionPrimitiveType(TypeFunctionPrimitiveType::NilType));
                break;
            case PrimitiveType::Boolean:
                target = typeFunctionRuntime->typeArena.allocate(TypeFunctionPrimitiveType(TypeFunctionPrimitiveType::Boolean));
                break;
            case PrimitiveType::Number:
                target = typeFunctionRuntime->typeArena.allocate(TypeFunctionPrimitiveType(TypeFunctionPrimitiveType::Number));
                break;
            case PrimitiveType::String:
                target = typeFunctionRuntime->typeArena.allocate(TypeFunctionPrimitiveType(TypeFunctionPrimitiveType::String));
                break;
            case PrimitiveType::Thread:
                target = typeFunctionRuntime->typeArena.allocate(TypeFunctionPrimitiveType(TypeFunctionPrimitiveType::Thread));
                break;
            case PrimitiveType::Buffer:
                target = typeFunctionRuntime->typeArena.allocate(TypeFunctionPrimitiveType(TypeFunctionPrimitiveType::Buffer));
                break;
            case PrimitiveType::Function:
            case PrimitiveType::Table:
            default:
            {
                std::string error = format("Argument of primitive type %s is not currently serializable by type functions", toString(ty).c_str());
                state->errors.push_back(error);
            }
            }
        }
        else if (auto u = get<UnknownType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionUnknownType{});
        else if (auto a = get<NeverType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionNeverType{});
        else if (auto a = get<AnyType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionAnyType{});
        else if (auto s = get<SingletonType>(ty))
        {
            if (auto bs = get<BooleanSingleton>(s))
                target = typeFunctionRuntime->typeArena.allocate(TypeFunctionSingletonType{TypeFunctionBooleanSingleton{bs->value}});
            else if (auto ss = get<StringSingleton>(s))
                target = typeFunctionRuntime->typeArena.allocate(TypeFunctionSingletonType{TypeFunctionStringSingleton{ss->value}});
            else
            {
                std::string error = format("Argument of singleton type %s is not currently serializable by type functions", toString(ty).c_str());
                state->errors.push_back(error);
            }
        }
        else if (auto u = get<UnionType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionUnionType{{}});
        else if (auto i = get<IntersectionType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionIntersectionType{{}});
        else if (auto n = get<NegationType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionNegationType{{}});
        else if (auto t = get<TableType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionTableType{{}, std::nullopt, std::nullopt});
        else if (auto m = get<MetatableType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionTableType{{}, std::nullopt, std::nullopt});
        else if (auto f = get<FunctionType>(ty))
        {
            TypeFunctionTypePackId emptyTypePack = typeFunctionRuntime->typePackArena.allocate(TypeFunctionTypePack{});
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionFunctionType{{}, {}, emptyTypePack, emptyTypePack});
        }
        else if (auto c = get<ExternType>(ty))
        {
            // Since there aren't any new class types being created in type functions, we will deserialize by using a direct reference to the original
            // class
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionExternType{{}, std::nullopt, std::nullopt, std::nullopt, std::nullopt, ty});
        }
        else if (auto g = get<GenericType>(ty))
        {
            Name name = g->name;

            if (!g->explicitName)
                name = format("g%d", g->index);

            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionGenericType{g->explicitName, false, std::move(name)});
        }
        else
        {
            std::string error = format("Argument of type %s is not currently serializable by type functions", toString(ty).c_str());
            state->errors.push_back(error);
        }

        types[ty] = target;
        queue.emplace_back(ty, target);
        return target;
    }

    TypeFunctionTypePackId shallowSerialize(TypePackId tp)
    {
        tp = follow(tp);

        if (auto it = find(tp))
            return *it;

        // Create a shallow serialization
        TypeFunctionTypePackId target = {};
        if (auto tPack = get<TypePack>(tp))
            target = typeFunctionRuntime->typePackArena.allocate(TypeFunctionTypePack{{}});
        else if (auto vPack = get<VariadicTypePack>(tp))
            target = typeFunctionRuntime->typePackArena.allocate(TypeFunctionVariadicTypePack{});
        else if (auto gPack = get<GenericTypePack>(tp))
        {
            Name name = gPack->name;

            if (!gPack->explicitName)
                name = format("g%d", gPack->index);

            target = typeFunctionRuntime->typePackArena.allocate(TypeFunctionGenericTypePack{gPack->explicitName, std::move(name)});
        }
        else
        {
            std::string error = format("Argument of type pack %s is not currently serializable by type functions", toString(tp).c_str());
            state->errors.push_back(error);
        }

        packs[tp] = target;
        queue.emplace_back(tp, target);
        return target;
    }

    void serializeChildren(const TypeId ty, TypeFunctionTypeId tfti)
    {
        if (auto [p1, p2] = std::tuple{get<PrimitiveType>(ty), getMutable<TypeFunctionPrimitiveType>(tfti)}; p1 && p2)
            serializeChildren(p1, p2);
        else if (auto [u1, u2] = std::tuple{get<UnknownType>(ty), getMutable<TypeFunctionUnknownType>(tfti)}; u1 && u2)
            serializeChildren(u1, u2);
        else if (auto [n1, n2] = std::tuple{get<NeverType>(ty), getMutable<TypeFunctionNeverType>(tfti)}; n1 && n2)
            serializeChildren(n1, n2);
        else if (auto [a1, a2] = std::tuple{get<AnyType>(ty), getMutable<TypeFunctionAnyType>(tfti)}; a1 && a2)
            serializeChildren(a1, a2);
        else if (auto [s1, s2] = std::tuple{get<SingletonType>(ty), getMutable<TypeFunctionSingletonType>(tfti)}; s1 && s2)
            serializeChildren(s1, s2);
        else if (auto [u1, u2] = std::tuple{get<UnionType>(ty), getMutable<TypeFunctionUnionType>(tfti)}; u1 && u2)
            serializeChildren(u1, u2);
        else if (auto [i1, i2] = std::tuple{get<IntersectionType>(ty), getMutable<TypeFunctionIntersectionType>(tfti)}; i1 && i2)
            serializeChildren(i1, i2);
        else if (auto [n1, n2] = std::tuple{get<NegationType>(ty), getMutable<TypeFunctionNegationType>(tfti)}; n1 && n2)
            serializeChildren(n1, n2);
        else if (auto [t1, t2] = std::tuple{get<TableType>(ty), getMutable<TypeFunctionTableType>(tfti)}; t1 && t2)
            serializeChildren(t1, t2);
        else if (auto [m1, m2] = std::tuple{get<MetatableType>(ty), getMutable<TypeFunctionTableType>(tfti)}; m1 && m2)
            serializeChildren(m1, m2);
        else if (auto [f1, f2] = std::tuple{get<FunctionType>(ty), getMutable<TypeFunctionFunctionType>(tfti)}; f1 && f2)
            serializeChildren(f1, f2);
        else if (auto [c1, c2] = std::tuple{get<ExternType>(ty), getMutable<TypeFunctionExternType>(tfti)}; c1 && c2)
            serializeChildren(c1, c2);
        else if (auto [g1, g2] = std::tuple{get<GenericType>(ty), getMutable<TypeFunctionGenericType>(tfti)}; g1 && g2)
            serializeChildren(g1, g2);
        else
        { // Either this or ty and tfti do not represent the same type
            std::string error = format("Argument of type %s is not currently serializable by type functions", toString(ty).c_str());
            state->errors.push_back(error);
        }
    }

    void serializeChildren(const TypePackId tp, TypeFunctionTypePackId tftp)
    {
        if (auto [tPack1, tPack2] = std::tuple{get<TypePack>(tp), getMutable<TypeFunctionTypePack>(tftp)}; tPack1 && tPack2)
            serializeChildren(tPack1, tPack2);
        else if (auto [vPack1, vPack2] = std::tuple{get<VariadicTypePack>(tp), getMutable<TypeFunctionVariadicTypePack>(tftp)}; vPack1 && vPack2)
            serializeChildren(vPack1, vPack2);
        else if (auto [gPack1, gPack2] = std::tuple{get<GenericTypePack>(tp), getMutable<TypeFunctionGenericTypePack>(tftp)}; gPack1 && gPack2)
            serializeChildren(gPack1, gPack2);
        else
        { // Either this or ty and tfti do not represent the same type
            std::string error = format("Argument of type pack %s is not currently serializable by type functions", toString(tp).c_str());
            state->errors.push_back(error);
        }
    }

    void serializeChildren(TypeOrPack kind, TypeFunctionKind tfkind)
    {
        if (auto [ty, tfty] = std::tuple{get<TypeId>(kind), get<TypeFunctionTypeId>(tfkind)}; ty && tfty)
            serializeChildren(*ty, *tfty);
        else if (auto [tp, tftp] = std::tuple{get<TypePackId>(kind), get<TypeFunctionTypePackId>(tfkind)}; tp && tftp)
            serializeChildren(*tp, *tftp);
        else
            state->ctx->ice->ice("Serializing user defined type function arguments: kind and tfkind do not represent the same type");
    }

    void serializeChildren(const PrimitiveType* p1, TypeFunctionPrimitiveType* p2)
    {
        // noop.
    }

    void serializeChildren(const UnknownType* u1, TypeFunctionUnknownType* u2)
    {
        // noop.
    }

    void serializeChildren(const NeverType* n1, TypeFunctionNeverType* n2)
    {
        // noop.
    }

    void serializeChildren(const AnyType* a1, TypeFunctionAnyType* a2)
    {
        // noop.
    }

    void serializeChildren(const SingletonType* s1, TypeFunctionSingletonType* s2)
    {
        // noop.
    }

    void serializeChildren(const UnionType* u1, TypeFunctionUnionType* u2)
    {
        for (const TypeId& ty : u1->options)
            u2->components.push_back(shallowSerialize(ty));
    }

    void serializeChildren(const IntersectionType* i1, TypeFunctionIntersectionType* i2)
    {
        for (const TypeId& ty : i1->parts)
            i2->components.push_back(shallowSerialize(ty));
    }

    void serializeChildren(const NegationType* n1, TypeFunctionNegationType* n2)
    {
        n2->type = shallowSerialize(n1->ty);
    }

    void serializeChildren(const TableType* t1, TypeFunctionTableType* t2)
    {
        for (const auto& [k, p] : t1->props)
        {
            std::optional<TypeFunctionTypeId> readTy = std::nullopt;
            if (p.readTy)
                readTy = shallowSerialize(*p.readTy);

            std::optional<TypeFunctionTypeId> writeTy = std::nullopt;
            if (p.writeTy)
                writeTy = shallowSerialize(*p.writeTy);

            t2->props[k] = TypeFunctionProperty{readTy, writeTy};
        }

        if (t1->indexer)
            t2->indexer = TypeFunctionTableIndexer(shallowSerialize(t1->indexer->indexType), shallowSerialize(t1->indexer->indexResultType));
    }

    void serializeChildren(const MetatableType* m1, TypeFunctionTableType* m2)
    {
        // Serialize main part of the metatable immediately
        if (auto tableTy = get<TableType>(follow(m1->table)))
            serializeChildren(tableTy, m2);

        m2->metatable = shallowSerialize(m1->metatable);
    }

    void serializeChildren(const FunctionType* f1, TypeFunctionFunctionType* f2)
    {
        f2->generics.reserve(f1->generics.size());
        for (auto ty : f1->generics)
            f2->generics.push_back(shallowSerialize(ty));

        f2->genericPacks.reserve(f1->genericPacks.size());
        for (auto tp : f1->genericPacks)
            f2->genericPacks.push_back(shallowSerialize(tp));

        f2->argTypes = shallowSerialize(f1->argTypes);
        f2->retTypes = shallowSerialize(f1->retTypes);
    }

    void serializeChildren(const ExternType* c1, TypeFunctionExternType* c2)
    {
        for (const auto& [k, p] : c1->props)
        {
            std::optional<TypeFunctionTypeId> readTy = std::nullopt;
            if (p.readTy)
                readTy = shallowSerialize(*p.readTy);

            std::optional<TypeFunctionTypeId> writeTy = std::nullopt;
            if (p.writeTy)
                writeTy = shallowSerialize(*p.writeTy);

            c2->props[k] = TypeFunctionProperty{readTy, writeTy};
        }

        if (c1->indexer)
            c2->indexer = TypeFunctionTableIndexer(shallowSerialize(c1->indexer->indexType), shallowSerialize(c1->indexer->indexResultType));

        if (c1->metatable)
            c2->metatable = shallowSerialize(*c1->metatable);

        if (c1->parent)
        {
            TypeFunctionTypeId parent = shallowSerialize(*c1->parent);

            // we don't yet have read/write parents in the type inference engine.
            c2->readParent = parent;
            c2->writeParent = parent;
        }
    }

    void serializeChildren(const GenericType* g1, TypeFunctionGenericType* g2)
    {
        // noop.
    }

    void serializeChildren(const TypePack* t1, TypeFunctionTypePack* t2)
    {
        for (const TypeId& ty : t1->head)
            t2->head.push_back(shallowSerialize(ty));

        if (t1->tail.has_value())
            t2->tail = shallowSerialize(*t1->tail);
    }

    void serializeChildren(const VariadicTypePack* v1, TypeFunctionVariadicTypePack* v2)
    {
        v2->type = shallowSerialize(v1->ty);
    }

    void serializeChildren(const GenericTypePack* v1, TypeFunctionGenericTypePack* v2)
    {
        // noop.
    }
};

template<typename T>
struct SerializedGeneric
{
    bool isNamed = false;
    std::string name;
    T type = nullptr;

    explicit SerializedGeneric(std::string name)
        : name(std::move(name))
    {
    }

    SerializedGeneric(bool isNamed, std::string name, T type)
        : isNamed(isNamed)
        , name(std::move(name))
        , type(std::move(type))
    {
    }
};

struct SerializedFunctionScope
{
    size_t oldQueueSize = 0;
    TypeFunctionFunctionType* function = nullptr;

    explicit SerializedFunctionScope(size_t oldQueueSize)
        : oldQueueSize(oldQueueSize)
    {
    }

    SerializedFunctionScope(size_t oldQueueSize, TypeFunctionFunctionType* function)
        : oldQueueSize(oldQueueSize)
        , function(function)
    {
    }
};

// Complete inverse of TypeFunctionSerializer
class TypeFunctionDeserializer
{
    using SeenTypes = DenseHashMap<TypeFunctionTypeId, TypeId>;
    using SeenTypePacks = DenseHashMap<TypeFunctionTypePackId, TypePackId>;

    TypeFunctionRuntimeBuilderState* state = nullptr;
    NotNull<TypeFunctionRuntime> typeFunctionRuntime;

    // A queue of TypeIds that have been deserialized, but whose interior types hasn't
    // been updated to point to itself. Once all of its interior types
    // has been updated, it gets removed from the queue.

    // queue.back() should always return two of same type in their respective sides
    // For example `auto [first, second] = queue.back()`: if first is TypeFunctionPrimitiveType,
    // second must be PrimitiveType; else there should be an error
    std::vector<std::tuple<TypeFunctionKind, TypeOrPack>> queue;

    // Generic types and packs currently in scope
    // Generics are resolved by name even if runtime generic type pointers are different
    // Multiple names mapping to the same generic can be in scope for nested generic functions
    std::vector<SerializedGeneric<TypeId>> genericTypes;
    std::vector<SerializedGeneric<TypePackId>> genericPacks;

    // To track when generics go out of scope, we have a list of queue positions at which a specific function has introduced generics
    std::vector<SerializedFunctionScope> functionScopes;

    SeenTypes types;     // Mapping of TypeFunctionTypeIds that have been shallow deserialized to TypeIds
    SeenTypePacks packs; // Mapping of TypeFunctionTypePackIds that have been shallow deserialized to TypePackIds

    int steps = 0;

public:
    explicit TypeFunctionDeserializer(TypeFunctionRuntimeBuilderState* state)
        : state(state)
        , typeFunctionRuntime(state->ctx->typeFunctionRuntime)
        , queue({})
        , types({})
        , packs({})
    {
    }

    TypeId deserialize(TypeFunctionTypeId ty)
    {
        shallowDeserialize(ty);
        run();

        if (hasExceededIterationLimit() || state->errors.size() != 0)
        {
            TypeId error = state->ctx->builtins->errorType;
            types[ty] = error;
            return error;
        }

        return find(ty).value_or(state->ctx->builtins->errorType);
    }

    TypePackId deserialize(TypeFunctionTypePackId tp)
    {
        shallowDeserialize(tp);
        run();

        if (hasExceededIterationLimit() || state->errors.size() != 0)
        {
            TypePackId error = state->ctx->builtins->errorTypePack;
            packs[tp] = error;
            return error;
        }

        return find(tp).value_or(state->ctx->builtins->errorTypePack);
    }

private:
    bool hasExceededIterationLimit() const
    {
        if (DFInt::LuauTypeFunctionSerdeIterationLimit == 0)
            return false;

        return steps + queue.size() >= size_t(DFInt::LuauTypeFunctionSerdeIterationLimit);
    }

    void run()
    {
        while (!queue.empty())
        {
            ++steps;

            if (hasExceededIterationLimit() || state->errors.size() != 0)
                break;

            auto [tfti, ty] = queue.back();
            queue.pop_back();

            deserializeChildren(tfti, ty);

            // If we have completed working on all children of a function, remove the generic parameters from scope
            if (!functionScopes.empty() && queue.size() == functionScopes.back().oldQueueSize && state->errors.empty())
            {
                closeFunctionScope(functionScopes.back().function);
                functionScopes.pop_back();
            }
        }
    }

    std::optional<TypeId> find(TypeFunctionTypeId ty) const
    {
        if (auto result = types.find(ty))
            return *result;

        return std::nullopt;
    }

    std::optional<TypePackId> find(TypeFunctionTypePackId tp) const
    {
        if (auto result = packs.find(tp))
            return *result;

        return std::nullopt;
    }

    std::optional<TypeOrPack> find(TypeFunctionKind kind) const
    {
        if (auto ty = get<TypeFunctionTypeId>(kind))
            return find(*ty);
        else if (auto tp = get<TypeFunctionTypePackId>(kind))
            return find(*tp);
        else
        {
            LUAU_ASSERT(!"Unknown kind found at TypeFunctionDeserializer");
            return std::nullopt;
        }
    }

    void closeFunctionScope(TypeFunctionFunctionType* f)
    {
        if (!f->generics.empty())
        {
            LUAU_ASSERT(genericTypes.size() >= f->generics.size());
            genericTypes.erase(genericTypes.begin() + int(genericTypes.size() - f->generics.size()), genericTypes.end());
        }

        if (!f->genericPacks.empty())
        {
            LUAU_ASSERT(genericPacks.size() >= f->genericPacks.size());
            genericPacks.erase(genericPacks.begin() + int(genericPacks.size() - f->genericPacks.size()), genericPacks.end());
        }
    }

    TypeId shallowDeserialize(TypeFunctionTypeId ty)
    {
        if (auto it = find(ty))
            return *it;

        // Create a shallow deserialization
        TypeId target = {};
        if (auto p = get<TypeFunctionPrimitiveType>(ty))
        {
            switch (p->type)
            {
            case TypeFunctionPrimitiveType::Type::NilType:
                target = state->ctx->builtins->nilType;
                break;
            case TypeFunctionPrimitiveType::Type::Boolean:
                target = state->ctx->builtins->booleanType;
                break;
            case TypeFunctionPrimitiveType::Type::Number:
                target = state->ctx->builtins->numberType;
                break;
            case TypeFunctionPrimitiveType::Type::String:
                target = state->ctx->builtins->stringType;
                break;
            case TypeFunctionPrimitiveType::Type::Thread:
                target = state->ctx->builtins->threadType;
                break;
            case TypeFunctionPrimitiveType::Type::Buffer:
                target = state->ctx->builtins->bufferType;
                break;
            default:
                state->ctx->ice->ice("Deserializing user defined type function arguments: mysterious type is being deserialized");
            }
        }
        else if (auto u = get<TypeFunctionUnknownType>(ty))
            target = state->ctx->builtins->unknownType;
        else if (auto n = get<TypeFunctionNeverType>(ty))
            target = state->ctx->builtins->neverType;
        else if (auto a = get<TypeFunctionAnyType>(ty))
            target = state->ctx->builtins->anyType;
        else if (auto s = get<TypeFunctionSingletonType>(ty))
        {
            if (auto bs = get<TypeFunctionBooleanSingleton>(s))
                target = state->ctx->arena->addType(SingletonType{BooleanSingleton{bs->value}});
            else if (auto ss = get<TypeFunctionStringSingleton>(s))
                target = state->ctx->arena->addType(SingletonType{StringSingleton{ss->value}});
            else
                state->ctx->ice->ice("Deserializing user defined type function arguments: mysterious type is being deserialized");
        }
        else if (auto u = get<TypeFunctionUnionType>(ty))
            target = state->ctx->arena->addTV(Type(UnionType{{}}));
        else if (auto i = get<TypeFunctionIntersectionType>(ty))
            target = state->ctx->arena->addTV(Type(IntersectionType{{}}));
        else if (auto n = get<TypeFunctionNegationType>(ty))
            target = state->ctx->arena->addType(NegationType{state->ctx->builtins->unknownType});
        else if (auto t = get<TypeFunctionTableType>(ty); t && !t->metatable.has_value())
            target = state->ctx->arena->addType(TableType{TableType::Props{}, std::nullopt, TypeLevel{}, TableState::Sealed});
        else if (auto m = get<TypeFunctionTableType>(ty); m && m->metatable.has_value())
        {
            TypeId emptyTable = state->ctx->arena->addType(TableType{TableType::Props{}, std::nullopt, TypeLevel{}, TableState::Sealed});
            target = state->ctx->arena->addType(MetatableType{emptyTable, emptyTable});
        }
        else if (auto f = get<TypeFunctionFunctionType>(ty))
        {
            TypePackId emptyTypePack = state->ctx->arena->addTypePack(TypePack{});
            target = state->ctx->arena->addType(FunctionType{emptyTypePack, emptyTypePack, {}, false});
        }
        else if (auto c = get<TypeFunctionExternType>(ty))
        {
            target = c->externTy;
        }
        else if (auto g = get<TypeFunctionGenericType>(ty))
        {
            if (g->isPack)
            {
                state->errors.push_back(format("Generic type pack '%s...' cannot be placed in a type position", g->name.c_str()));
                return nullptr;
            }
            else
            {
                auto it = std::find_if(
                    genericTypes.rbegin(),
                    genericTypes.rend(),
                    [&](const SerializedGeneric<TypeId>& el)
                    {
                        return g->isNamed == el.isNamed && g->name == el.name;
                    }
                );

                if (it == genericTypes.rend())
                {
                    state->errors.push_back(format("Generic type '%s' is not in a scope of the active generic function", g->name.c_str()));
                    return nullptr;
                }

                target = it->type;
            }
        }
        else
            state->ctx->ice->ice("Deserializing user defined type function arguments: mysterious type is being deserialized");

        types[ty] = target;
        queue.emplace_back(ty, target);
        return target;
    }

    TypePackId shallowDeserialize(TypeFunctionTypePackId tp)
    {
        if (auto it = find(tp))
            return *it;

        // Create a shallow deserialization
        TypePackId target = {};
        if (auto tPack = get<TypeFunctionTypePack>(tp))
        {
            target = state->ctx->arena->addTypePack(TypePack{});
        }
        else if (auto vPack = get<TypeFunctionVariadicTypePack>(tp))
        {
            target = state->ctx->arena->addTypePack(VariadicTypePack{});
        }
        else if (auto gPack = get<TypeFunctionGenericTypePack>(tp))
        {
            auto it = std::find_if(
                genericPacks.rbegin(),
                genericPacks.rend(),
                [&](const SerializedGeneric<TypePackId>& el)
                {
                    return gPack->isNamed == el.isNamed && gPack->name == el.name;
                }
            );

            if (it == genericPacks.rend())
            {
                state->errors.push_back(format("Generic type pack '%s...' is not in a scope of the active generic function", gPack->name.c_str()));
                return nullptr;
            }

            target = it->type;
        }
        else
        {
            state->ctx->ice->ice("Deserializing user defined type function arguments: mysterious type is being deserialized");
        }

        packs[tp] = target;
        queue.emplace_back(tp, target);
        return target;
    }

    void deserializeChildren(TypeFunctionTypeId tfti, TypeId ty)
    {
        if (auto [p1, p2] = std::tuple{getMutable<PrimitiveType>(ty), getMutable<TypeFunctionPrimitiveType>(tfti)}; p1 && p2)
            deserializeChildren(p2, p1);
        else if (auto [u1, u2] = std::tuple{getMutable<UnknownType>(ty), getMutable<TypeFunctionUnknownType>(tfti)}; u1 && u2)
            deserializeChildren(u2, u1);
        else if (auto [n1, n2] = std::tuple{getMutable<NeverType>(ty), getMutable<TypeFunctionNeverType>(tfti)}; n1 && n2)
            deserializeChildren(n2, n1);
        else if (auto [a1, a2] = std::tuple{getMutable<AnyType>(ty), getMutable<TypeFunctionAnyType>(tfti)}; a1 && a2)
            deserializeChildren(a2, a1);
        else if (auto [s1, s2] = std::tuple{getMutable<SingletonType>(ty), getMutable<TypeFunctionSingletonType>(tfti)}; s1 && s2)
            deserializeChildren(s2, s1);
        else if (auto [u1, u2] = std::tuple{getMutable<UnionType>(ty), getMutable<TypeFunctionUnionType>(tfti)}; u1 && u2)
            deserializeChildren(u2, u1);
        else if (auto [i1, i2] = std::tuple{getMutable<IntersectionType>(ty), getMutable<TypeFunctionIntersectionType>(tfti)}; i1 && i2)
            deserializeChildren(i2, i1);
        else if (auto [n1, n2] = std::tuple{getMutable<NegationType>(ty), getMutable<TypeFunctionNegationType>(tfti)}; n1 && n2)
            deserializeChildren(n2, n1);
        else if (auto [t1, t2] = std::tuple{getMutable<TableType>(ty), getMutable<TypeFunctionTableType>(tfti)};
                 t1 && t2 && !t2->metatable.has_value())
            deserializeChildren(t2, t1);
        else if (auto [m1, m2] = std::tuple{getMutable<MetatableType>(ty), getMutable<TypeFunctionTableType>(tfti)};
                 m1 && m2 && m2->metatable.has_value())
            deserializeChildren(m2, m1);
        else if (auto [f1, f2] = std::tuple{getMutable<FunctionType>(ty), getMutable<TypeFunctionFunctionType>(tfti)}; f1 && f2)
            deserializeChildren(f2, f1);
        else if (auto [c1, c2] = std::tuple{getMutable<ExternType>(ty), getMutable<TypeFunctionExternType>(tfti)}; c1 && c2)
            deserializeChildren(c2, c1);
        else if (auto [g1, g2] = std::tuple{getMutable<GenericType>(ty), getMutable<TypeFunctionGenericType>(tfti)}; g1 && g2)
            deserializeChildren(g2, g1);
        else
            state->ctx->ice->ice("Deserializing user defined type function arguments: mysterious type is being deserialized");
    }

    void deserializeChildren(TypeFunctionTypePackId tftp, TypePackId tp)
    {
        if (auto [tPack1, tPack2] = std::tuple{getMutable<TypePack>(tp), getMutable<TypeFunctionTypePack>(tftp)}; tPack1 && tPack2)
            deserializeChildren(tPack2, tPack1);
        else if (auto [vPack1, vPack2] = std::tuple{getMutable<VariadicTypePack>(tp), getMutable<TypeFunctionVariadicTypePack>(tftp)};
                 vPack1 && vPack2)
            deserializeChildren(vPack2, vPack1);
        else if (auto [gPack1, gPack2] = std::tuple{getMutable<GenericTypePack>(tp), getMutable<TypeFunctionGenericTypePack>(tftp)}; gPack1 && gPack2)
            deserializeChildren(gPack2, gPack1);
        else
            state->ctx->ice->ice("Deserializing user defined type function arguments: mysterious type is being deserialized");
    }

    void deserializeChildren(TypeFunctionKind tfkind, TypeOrPack kind)
    {
        if (auto [ty, tfty] = std::tuple{get<TypeId>(kind), get<TypeFunctionTypeId>(tfkind)}; ty && tfty)
            deserializeChildren(*tfty, *ty);
        else if (auto [tp, tftp] = std::tuple{get<TypePackId>(kind), get<TypeFunctionTypePackId>(tfkind)}; tp && tftp)
            deserializeChildren(*tftp, *tp);
        else
            state->ctx->ice->ice("Deserializing user defined type function arguments: tfkind and kind do not represent the same type");
    }

    void deserializeChildren(TypeFunctionPrimitiveType* p2, PrimitiveType* p1)
    {
        // noop.
    }

    void deserializeChildren(TypeFunctionUnknownType* u2, UnknownType* u1)
    {
        // noop.
    }

    void deserializeChildren(TypeFunctionNeverType* n2, NeverType* n1)
    {
        // noop.
    }

    void deserializeChildren(TypeFunctionAnyType* a2, AnyType* a1)
    {
        // noop.
    }

    void deserializeChildren(TypeFunctionSingletonType* s2, SingletonType* s1)
    {
        // noop.
    }

    void deserializeChildren(TypeFunctionUnionType* u2, UnionType* u1)
    {
        for (TypeFunctionTypeId& ty : u2->components)
            u1->options.push_back(shallowDeserialize(ty));
    }

    void deserializeChildren(TypeFunctionIntersectionType* i2, IntersectionType* i1)
    {
        for (TypeFunctionTypeId& ty : i2->components)
            i1->parts.push_back(shallowDeserialize(ty));
    }

    void deserializeChildren(TypeFunctionNegationType* n2, NegationType* n1)
    {
        n1->ty = shallowDeserialize(n2->type);
    }

    void deserializeChildren(TypeFunctionTableType* t2, TableType* t1)
    {
        for (const auto& [k, p] : t2->props)
        {
            if (p.readTy && p.writeTy)
                t1->props[k] = Property::rw(shallowDeserialize(*p.readTy), shallowDeserialize(*p.writeTy));
            else if (p.readTy)
                t1->props[k] = Property::readonly(shallowDeserialize(*p.readTy));
            else if (p.writeTy)
                t1->props[k] = Property::writeonly(shallowDeserialize(*p.writeTy));
        }

        if (t2->indexer.has_value())
            t1->indexer = TableIndexer(shallowDeserialize(t2->indexer->keyType), shallowDeserialize(t2->indexer->valueType));
    }

    void deserializeChildren(TypeFunctionTableType* m2, MetatableType* m1)
    {
        TypeFunctionTypeId temp = typeFunctionRuntime->typeArena.allocate(TypeFunctionTableType{m2->props, m2->indexer});
        m1->table = shallowDeserialize(temp);

        if (m2->metatable.has_value())
            m1->metatable = shallowDeserialize(*m2->metatable);
    }

    void deserializeChildren(TypeFunctionFunctionType* f2, FunctionType* f1)
    {
        if (FFlag::LuauEmplaceNotPushBack)
            functionScopes.emplace_back(queue.size(), f2);
        else
            functionScopes.push_back({queue.size(), f2});

        std::set<std::pair<bool, std::string>> genericNames;

        // Introduce generic function parameters into scope
        for (auto ty : f2->generics)
        {
            auto gty = get<TypeFunctionGenericType>(ty);
            LUAU_ASSERT(gty && !gty->isPack);

            std::pair<bool, std::string> nameKey = std::make_pair(gty->isNamed, gty->name);

            // Duplicates are not allowed
            if (genericNames.find(nameKey) != genericNames.end())
            {
                state->errors.push_back(format("Duplicate type parameter '%s'", gty->name.c_str()));
                return;
            }

            genericNames.insert(nameKey);

            TypeId mapping = state->ctx->arena->addTV(Type(gty->isNamed ? GenericType{state->ctx->scope.get(), gty->name} : GenericType{}));
            if (FFlag::LuauEmplaceNotPushBack)
                genericTypes.emplace_back(gty->isNamed, gty->name, mapping);
            else
                genericTypes.push_back({gty->isNamed, gty->name, mapping});
        }

        for (auto tp : f2->genericPacks)
        {
            auto gtp = get<TypeFunctionGenericTypePack>(tp);
            LUAU_ASSERT(gtp);

            std::pair<bool, std::string> nameKey = std::make_pair(gtp->isNamed, gtp->name);

            // Duplicates are not allowed
            if (genericNames.find(nameKey) != genericNames.end())
            {
                state->errors.push_back(format("Duplicate type parameter '%s'", gtp->name.c_str()));
                return;
            }

            genericNames.insert(nameKey);

            TypePackId mapping =
                state->ctx->arena->addTypePack(TypePackVar(gtp->isNamed ? GenericTypePack{state->ctx->scope.get(), gtp->name} : GenericTypePack{}));
            if (FFlag::LuauEmplaceNotPushBack)
                genericPacks.emplace_back(gtp->isNamed, gtp->name, mapping);
            else
                genericPacks.push_back({gtp->isNamed, gtp->name, mapping});
        }

        f1->generics.reserve(f2->generics.size());
        for (auto ty : f2->generics)
            f1->generics.push_back(shallowDeserialize(ty));

        f1->genericPacks.reserve(f2->genericPacks.size());
        for (auto tp : f2->genericPacks)
            f1->genericPacks.push_back(shallowDeserialize(tp));

        if (f2->argTypes)
            f1->argTypes = shallowDeserialize(f2->argTypes);

        if (f2->retTypes)
            f1->retTypes = shallowDeserialize(f2->retTypes);
    }

    void deserializeChildren(TypeFunctionExternType* c2, ExternType* c1)
    {
        // noop.
    }

    void deserializeChildren(TypeFunctionGenericType* g2, GenericType* g1)
    {
        // noop.
    }

    void deserializeChildren(TypeFunctionTypePack* t2, TypePack* t1)
    {
        for (TypeFunctionTypeId& ty : t2->head)
            t1->head.push_back(shallowDeserialize(ty));

        if (t2->tail.has_value())
            t1->tail = shallowDeserialize(*t2->tail);
    }

    void deserializeChildren(TypeFunctionVariadicTypePack* v2, VariadicTypePack* v1)
    {
        v1->ty = shallowDeserialize(v2->type);
    }

    void deserializeChildren(TypeFunctionGenericTypePack* v2, GenericTypePack* v1)
    {
        // noop.
    }
};

TypeFunctionTypeId serialize(TypeId ty, TypeFunctionRuntimeBuilderState* state)
{
    return TypeFunctionSerializer(state).serialize(ty);
}

TypeId deserialize(TypeFunctionTypeId ty, TypeFunctionRuntimeBuilderState* state)
{
    return TypeFunctionDeserializer(state).deserialize(ty);
}

} // namespace Luau
