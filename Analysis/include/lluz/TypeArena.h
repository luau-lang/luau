// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lluz/TypedAllocator.h"
#include "lluz/TypeVar.h"
#include "lluz/TypePack.h"

#include <vector>

namespace lluz
{

struct TypeArena
{
    TypedAllocator<TypeVar> typeVars;
    TypedAllocator<TypePackVar> typePacks;

    void clear();

    template<typename T>
    TypeId addType(T tv)
    {
        if constexpr (std::is_same_v<T, UnionTypeVar>)
            lluz_ASSERT(tv.options.size() >= 2);

        return addTV(TypeVar(std::move(tv)));
    }

    TypeId addTV(TypeVar&& tv);

    TypeId freshType(TypeLevel level);

    TypePackId addTypePack(std::initializer_list<TypeId> types);
    TypePackId addTypePack(std::vector<TypeId> types);
    TypePackId addTypePack(TypePack pack);
    TypePackId addTypePack(TypePackVar pack);

    template<typename T>
    TypePackId addTypePack(T tp)
    {
        return addTypePack(TypePackVar(std::move(tp)));
    }
};

void freeze(TypeArena& arena);
void unfreeze(TypeArena& arena);

} // namespace lluz
