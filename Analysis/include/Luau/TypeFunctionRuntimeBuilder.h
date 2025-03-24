// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Type.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypeFunctionRuntime.h"

namespace Luau
{

using Kind = Variant<TypeId, TypePackId>;

template<typename T>
const T* get(const Kind& kind)
{
    return get_if<T>(&kind);
}

using TypeFunctionKind = Variant<TypeFunctionTypeId, TypeFunctionTypePackId>;

template<typename T>
const T* get(const TypeFunctionKind& tfkind)
{
    return get_if<T>(&tfkind);
}

struct TypeFunctionRuntimeBuilderState
{
    NotNull<TypeFunctionContext> ctx;

    // List of errors that occur during serialization/deserialization
    // At every iteration of serialization/deserialization, if this list.size() != 0, we halt the process
    std::vector<std::string> errors{};

    TypeFunctionRuntimeBuilderState(NotNull<TypeFunctionContext> ctx)
        : ctx(ctx)
    {
    }
};

TypeFunctionTypeId serialize(TypeId ty, TypeFunctionRuntimeBuilderState* state);
TypeId deserialize(TypeFunctionTypeId ty, TypeFunctionRuntimeBuilderState* state);

} // namespace Luau
