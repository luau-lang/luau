// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/Scope.h"
#include "Luau/TypeFunctionRuntimeBuilder.h"
#include "Luau/Type.h"
#include "Luau/Variant.h"

#include <optional>
#include <string>
#include <map>
#include <vector>

using lua_State = struct lua_State;

namespace Luau
{

struct InternalErrorReporter;
struct TypeCheckLimits;
struct TypeFunctionRuntimeBuilderState;

struct LuauTempThreadPopper
{
    explicit LuauTempThreadPopper(lua_State* L);
    ~LuauTempThreadPopper();

    lua_State* L = nullptr;
};

using StateRef = std::unique_ptr<lua_State, void (*)(lua_State*)>;

void* typeFunctionAlloc(void* ud, void* ptr, size_t osize, size_t nsize);

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

struct TypeFunctionGenericTypePack
{
    bool isNamed = false;

    std::string name;
};

using TypeFunctionTypePackVariant = Variant<TypeFunctionTypePack, TypeFunctionVariadicTypePack, TypeFunctionGenericTypePack>;

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
    std::vector<TypeFunctionTypeId> generics;
    std::vector<TypeFunctionTypePackId> genericPacks;

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

struct TypeFunctionExternType
{
    using Name = std::string;
    using Props = std::map<Name, TypeFunctionProperty>;

    Props props;

    std::optional<TypeFunctionTableIndexer> indexer;

    std::optional<TypeFunctionTypeId> metatable; // metaclass?

    std::optional<TypeFunctionTypeId> readParent;
    std::optional<TypeFunctionTypeId> writeParent;

    TypeId externTy;
};

struct TypeFunctionGenericType
{
    bool isNamed = false;
    bool isPack = false;

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
    TypeFunctionExternType,
    TypeFunctionGenericType>;

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

struct TypeFunctionRuntime
{
    TypeFunctionRuntime(NotNull<InternalErrorReporter> ice, NotNull<TypeCheckLimits> limits);
    ~TypeFunctionRuntime();

    // Return value is an error message if registration failed
    std::optional<std::string> registerFunction(AstStatTypeFunction* function);

    // For user-defined type functions, we store all generated types and packs for the duration of the typecheck
    TypedAllocator<TypeFunctionType> typeArena;
    TypedAllocator<TypeFunctionTypePackVar> typePackArena;

    NotNull<InternalErrorReporter> ice;
    NotNull<TypeCheckLimits> limits;

    StateRef state;

    // Set of functions which have their environment table initialized
    DenseHashSet<AstStatTypeFunction*> initialized{nullptr};

    // Evaluation of type functions should only be performed in the absence of parse errors in the source module
    bool allowEvaluation = true;

    // Root scope in which the type function operates in, set up by ConstraintGenerator
    ScopePtr rootScope;

    // Output created by 'print' function
    std::vector<std::string> messages;

    // Type builder, valid for the duration of a single evaluation
    TypeFunctionRuntimeBuilderState* runtimeBuilder = nullptr;

private:
    void prepareState();
};

std::optional<std::string> checkResultForError(lua_State* L, const char* typeFunctionName, int luaResult);

TypeFunctionRuntime* getTypeFunctionRuntime(lua_State* L);

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
