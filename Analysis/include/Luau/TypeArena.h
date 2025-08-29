// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Polarity.h"
#include "Luau/TypedAllocator.h"
#include "Luau/Type.h"
#include "Luau/TypePack.h"

#include <vector>

namespace Luau
{
struct Module;

struct TypeArena
{
    TypedAllocator<Type> types;
    TypedAllocator<TypePackVar> typePacks;

    // Owning module, if any
    Module* owningModule = nullptr;

    bool collectSingletonStats = false;
    size_t boolSingletonsMinted = 0;
    size_t strSingletonsMinted = 0;
    DenseHashSet<std::optional<std::string>> uniqueStrSingletonsMinted{std::nullopt};

    void clear();

    template<typename T>
    TypeId addType(T tv)
    {
        if constexpr (std::is_same_v<T, UnionType>)
            LUAU_ASSERT(tv.options.size() >= 2);

        if constexpr (std::is_same_v<T, SingletonType>)
        {
            if (collectSingletonStats)
                recordSingletonStats(NotNull{&tv});
        }

        return addTV(Type(std::move(tv)));
    }

    TypeId addTV(Type&& tv);

    TypeId freshType(NotNull<BuiltinTypes> builtins, TypeLevel level);
    TypeId freshType(NotNull<BuiltinTypes> builtins, Scope* scope);
    TypeId freshType(NotNull<BuiltinTypes> builtins, Scope* scope, TypeLevel level);

    TypePackId freshTypePack(Scope* scope, Polarity polarity = Polarity::Unknown);

    TypePackId addTypePack(std::initializer_list<TypeId> types);
    TypePackId addTypePack(std::vector<TypeId> types, std::optional<TypePackId> tail = {});
    TypePackId addTypePack(TypePack pack);
    TypePackId addTypePack(TypePackVar pack);

    template<typename T>
    TypePackId addTypePack(T tp)
    {
        return addTypePack(TypePackVar(std::move(tp)));
    }

    TypeId addTypeFunction(const TypeFunction& function, std::initializer_list<TypeId> types);
    TypeId addTypeFunction(const TypeFunction& function, std::vector<TypeId> typeArguments, std::vector<TypePackId> packArguments = {});
    TypePackId addTypePackFunction(const TypePackFunction& function, std::initializer_list<TypeId> types);
    TypePackId addTypePackFunction(const TypePackFunction& function, std::vector<TypeId> typeArguments, std::vector<TypePackId> packArguments = {});

    void recordSingletonStats(NotNull<SingletonType> singleton);
};

void freeze(TypeArena& arena);
void unfreeze(TypeArena& arena);

} // namespace Luau
