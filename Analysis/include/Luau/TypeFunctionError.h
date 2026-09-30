// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Location.h"
#include "Luau/TypeFwd.h"
#include "Luau/Variant.h"

#include <string>

namespace Luau
{

// The type function serializer attempted to serialize an unsupported type.
struct UnsupportedType
{
    TypeId type;

    bool operator==(const UnsupportedType& rhs) const;
};

// The type function serializer attempted to serialize an unsupported type pack.
struct UnsupportedTypePack
{
    TypePackId pack;

    bool operator==(const UnsupportedTypePack& rhs) const;
};

// An error produced by the runtime during type function evaluation.
struct RuntimeError
{
    std::string message;

    bool operator==(const RuntimeError& rhs) const;
};

// The type function body failed to compile.
struct FailedToCompile
{
    std::string functionName;
    std::string compileError;

    bool operator==(const FailedToCompile& rhs) const;
};

// The type function was not found in the global scope after registration.
struct TypeFunctionMissing
{
    std::string functionName;

    bool operator==(const TypeFunctionMissing& rhs) const;
};

using TypeFunctionErrorData = Variant<UnsupportedType, UnsupportedTypePack, RuntimeError, FailedToCompile, TypeFunctionMissing>;

struct TypeFunctionError
{
    Location location;
    ModuleName moduleName;
    TypeFunctionErrorData data;

    static int minCode();
    int code() const;

    TypeFunctionError() = default;

    TypeFunctionError(const Location& location, const ModuleName moduleName, TypeFunctionErrorData data)
        : location(location)
        , moduleName(moduleName)
        , data(std::move(data))
    {
    }

    TypeFunctionError(const Location& location, const TypeFunctionErrorData& data)
        : TypeFunctionError(location, {}, data)
    {
    }

    bool operator==(const TypeFunctionError& rhs) const;
};

template<typename T>
const T* get(const TypeFunctionError& e)
{
    return get_if<T>(&e.data);
}

template<typename T>
T* get(TypeFunctionError& e)
{
    return get_if<T>(&e.data);
}

std::string toString(const TypeFunctionError& error);

} // namespace Luau
