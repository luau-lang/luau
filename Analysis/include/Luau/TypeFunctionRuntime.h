// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/Variant.h"

#include <optional>
#include <string>
#include <map>
#include <vector>

using lua_State = struct lua_State;

namespace Luau
{

void* typeFunctionAlloc(void* ud, void* ptr, size_t osize, size_t nsize);

// Replica of types from Type.h
struct TypeFunctionType;
using TypeFunctionTypeId = const TypeFunctionType*;

struct TypeFunctionTypePackVar;
using TypeFunctionTypePackId = const TypeFunctionTypePackVar*;

struct TypeFunctionPrimitiveType
{
    enum Type
    {
        NilType,
        Boolean,
        Number,
        String,
        Thread,
        Buffer,
    };

    Type type;

    TypeFunctionPrimitiveType(Type type)
        : type(type)
    {
    }
};

struct TypeFunctionBooleanSingleton
{
    bool value = false;
};

struct TypeFunctionStringSingleton
{
    std::string value;
};

using TypeFunctionSingletonVariant = Variant<TypeFunctionBooleanSingleton, TypeFunctionStringSingleton>;

struct TypeFunctionSingletonType
{
    TypeFunctionSingletonVariant variant;

    explicit TypeFunctionSingletonType(TypeFunctionSingletonVariant variant)
        : variant(std::move(variant))
    {
    }
};

template<typename T>
const T* get(const TypeFunctionSingletonType* tv)
{
    LUAU_ASSERT(tv);

    return tv ? get_if<T>(&tv->variant) : nullptr;
}

template<typename T>
T* getMutable(const TypeFunctionSingletonType* tv)
{
    LUAU_ASSERT(tv);

    return tv ? get_if<T>(&const_cast<TypeFunctionSingletonType*>(tv)->variant) : nullptr;
}

struct TypeFunctionUnionType
{
    std::vector<TypeFunctionTypeId> components;
};

struct TypeFunctionIntersectionType
{
    std::vector<TypeFunctionTypeId> components;
};

struct TypeFunctionAnyType
{
};

struct TypeFunctionUnknownType
{
};

struct TypeFunctionNeverType
{
};

struct TypeFunctionNegationType
{
    TypeFunctionTypeId type;
};

struct TypeFunctionTypePack
{
    std::vector<TypeFunctionTypeId> head;
    std::optional<TypeFunctionTypePackId> tail;
};

struct TypeFunctionVariadicTypePack
{
    TypeFunctionTypeId type;
};

using TypeFunctionTypePackVariant = Variant<TypeFunctionTypePack, TypeFunctionVariadicTypePack>;

struct TypeFunctionTypePackVar
{
    TypeFunctionTypePackVariant type;

    TypeFunctionTypePackVar(TypeFunctionTypePackVariant type)
        : type(std::move(type))
    {
    }

    bool operator==(const TypeFunctionTypePackVar& rhs) const;
};

struct TypeFunctionFunctionType
{
    TypeFunctionTypePackId argTypes;
    TypeFunctionTypePackId retTypes;
};

template<typename T>
const T* get(TypeFunctionTypePackId tv)
{
    LUAU_ASSERT(tv);

    return tv ? get_if<T>(&tv->type) : nullptr;
}

template<typename T>
T* getMutable(TypeFunctionTypePackId tv)
{
    LUAU_ASSERT(tv);

    return tv ? get_if<T>(&const_cast<TypeFunctionTypePackVar*>(tv)->type) : nullptr;
}

struct TypeFunctionTableIndexer
{
    TypeFunctionTableIndexer(TypeFunctionTypeId keyType, TypeFunctionTypeId valueType)
        : keyType(keyType)
        , valueType(valueType)
    {
    }

    TypeFunctionTypeId keyType;
    TypeFunctionTypeId valueType;
};

struct TypeFunctionProperty
{
    static TypeFunctionProperty readonly(TypeFunctionTypeId ty);
    static TypeFunctionProperty writeonly(TypeFunctionTypeId ty);
    static TypeFunctionProperty rw(TypeFunctionTypeId ty);                             // Shared read-write type.
    static TypeFunctionProperty rw(TypeFunctionTypeId read, TypeFunctionTypeId write); // Separate read-write type.

    bool isReadOnly() const;
    bool isWriteOnly() const;

    std::optional<TypeFunctionTypeId> readTy;
    std::optional<TypeFunctionTypeId> writeTy;
};

struct TypeFunctionTableType
{
    using Name = std::string;
    using Props = std::map<Name, TypeFunctionProperty>;

    Props props;

    std::optional<TypeFunctionTableIndexer> indexer;

    // Should always be a TypeFunctionTableType
    std::optional<TypeFunctionTypeId> metatable;
};

struct TypeFunctionClassType
{
    using Name = std::string;
    using Props = std::map<Name, TypeFunctionProperty>;

    Props props;

    std::optional<TypeFunctionTableIndexer> indexer;

    std::optional<TypeFunctionTypeId> metatable; // metaclass?

    std::optional<TypeFunctionTypeId> parent;

    std::string name;
};

using TypeFunctionTypeVariant = Luau::Variant<
    TypeFunctionPrimitiveType,
    TypeFunctionAnyType,
    TypeFunctionUnknownType,
    TypeFunctionNeverType,
    TypeFunctionSingletonType,
    TypeFunctionUnionType,
    TypeFunctionIntersectionType,
    TypeFunctionNegationType,
    TypeFunctionFunctionType,
    TypeFunctionTableType,
    TypeFunctionClassType>;

struct TypeFunctionType
{
    TypeFunctionTypeVariant type;

    TypeFunctionType(TypeFunctionTypeVariant type)
        : type(std::move(type))
    {
    }

    bool operator==(const TypeFunctionType& rhs) const;
};

template<typename T>
const T* get(TypeFunctionTypeId tv)
{
    LUAU_ASSERT(tv);

    return tv ? Luau::get_if<T>(&tv->type) : nullptr;
}

template<typename T>
T* getMutable(TypeFunctionTypeId tv)
{
    LUAU_ASSERT(tv);

    return tv ? Luau::get_if<T>(&const_cast<TypeFunctionType*>(tv)->type) : nullptr;
}

std::optional<std::string> checkResultForError(lua_State* L, const char* typeFunctionName, int luaResult);

TypeFunctionType* allocateTypeFunctionType(lua_State* L, TypeFunctionTypeVariant type);
TypeFunctionTypePackVar* allocateTypeFunctionTypePack(lua_State* L, TypeFunctionTypePackVariant type);

void allocTypeUserData(lua_State* L, TypeFunctionTypeVariant type);

bool isTypeUserData(lua_State* L, int idx);
TypeFunctionTypeId getTypeUserData(lua_State* L, int idx);
std::optional<TypeFunctionTypeId> optionalTypeUserData(lua_State* L, int idx);

void registerTypesLibrary(lua_State* L);
void registerTypeUserData(lua_State* L);

void setTypeFunctionEnvironment(lua_State* L);

void resetTypeFunctionState(lua_State* L);

} // namespace Luau
