// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypeFunctionRuntime.h"

#include "Luau/DenseHash.h"
#include "Luau/StringUtils.h"
#include "Luau/TypeFunction.h"

#include "lua.h"
#include "lualib.h"

#include <optional>
#include <set>
#include <vector>

LUAU_DYNAMIC_FASTINT(LuauTypeFunctionSerdeIterationLimit)
LUAU_FASTFLAGVARIABLE(LuauUserTypeFunFixRegister)
LUAU_FASTFLAGVARIABLE(LuauUserTypeFunFixNoReadWrite)
LUAU_FASTFLAGVARIABLE(LuauUserTypeFunThreadBuffer)

namespace Luau
{

constexpr int kTypeUserdataTag = 42;

void* typeFunctionAlloc(void* ud, void* ptr, size_t osize, size_t nsize)
{
    if (nsize == 0)
    {
        ::operator delete(ptr);
        return nullptr;
    }
    else if (osize == 0)
    {
        return ::operator new(nsize);
    }
    else
    {
        void* data = ::operator new(nsize);
        memcpy(data, ptr, nsize < osize ? nsize : osize);

        ::operator delete(ptr);

        return data;
    }
}

std::optional<std::string> checkResultForError(lua_State* L, const char* typeFunctionName, int luaResult)
{
    switch (luaResult)
    {
    case LUA_OK:
        return std::nullopt;
    case LUA_YIELD:
    case LUA_BREAK:
        return format("'%s' type function errored: unexpected yield or break", typeFunctionName);
    default:
        if (!lua_gettop(L))
            return format("'%s' type function errored unexpectedly", typeFunctionName);

        if (lua_isstring(L, -1))
            return format("'%s' type function errored at runtime: %s", typeFunctionName, lua_tostring(L, -1));

        return format("'%s' type function errored at runtime: raised an error of type %s", typeFunctionName, lua_typename(L, -1));
    }
}

static TypeFunctionRuntime* getTypeFunctionRuntime(lua_State* L)
{
    return static_cast<TypeFunctionRuntime*>(lua_getthreaddata(lua_mainthread(L)));
}

TypeFunctionType* allocateTypeFunctionType(lua_State* L, TypeFunctionTypeVariant type)
{
    auto ctx = getTypeFunctionRuntime(L);
    return ctx->typeArena.allocate(std::move(type));
}

TypeFunctionTypePackVar* allocateTypeFunctionTypePack(lua_State* L, TypeFunctionTypePackVariant type)
{
    auto ctx = getTypeFunctionRuntime(L);
    return ctx->typePackArena.allocate(std::move(type));
}

// Pushes a new type userdata onto the stack
void allocTypeUserData(lua_State* L, TypeFunctionTypeVariant type)
{
    // allocate a new type userdata
    TypeFunctionTypeId* ptr = static_cast<TypeFunctionTypeId*>(lua_newuserdatatagged(L, sizeof(TypeFunctionTypeId), kTypeUserdataTag));
    *ptr = allocateTypeFunctionType(L, std::move(type));

    // set the new userdata's metatable to type metatable
    luaL_getmetatable(L, "type");
    lua_setmetatable(L, -2);
}

void deallocTypeUserData(lua_State* L, void* data)
{
    // only non-owning pointers into an arena is stored
}

bool isTypeUserData(lua_State* L, int idx)
{
    if (!lua_isuserdata(L, idx))
        return false;

    return lua_touserdatatagged(L, idx, kTypeUserdataTag) != nullptr;
}

TypeFunctionTypeId getTypeUserData(lua_State* L, int idx)
{
    if (auto typ = static_cast<TypeFunctionTypeId*>(lua_touserdatatagged(L, idx, kTypeUserdataTag)))
        return *typ;

    luaL_typeerrorL(L, idx, "type");
}

std::optional<TypeFunctionTypeId> optionalTypeUserData(lua_State* L, int idx)
{
    if (lua_isnoneornil(L, idx))
        return std::nullopt;
    else
        return getTypeUserData(L, idx);
}

// returns a string tag of TypeFunctionTypeId
static std::string getTag(lua_State* L, TypeFunctionTypeId ty)
{
    if (auto n = get<TypeFunctionPrimitiveType>(ty); n && n->type == TypeFunctionPrimitiveType::Type::NilType)
        return "nil";
    else if (auto b = get<TypeFunctionPrimitiveType>(ty); b && b->type == TypeFunctionPrimitiveType::Type::Boolean)
        return "boolean";
    else if (auto n = get<TypeFunctionPrimitiveType>(ty); n && n->type == TypeFunctionPrimitiveType::Type::Number)
        return "number";
    else if (auto s = get<TypeFunctionPrimitiveType>(ty); s && s->type == TypeFunctionPrimitiveType::Type::String)
        return "string";
    else if (auto s = get<TypeFunctionPrimitiveType>(ty);
             FFlag::LuauUserTypeFunThreadBuffer && s && s->type == TypeFunctionPrimitiveType::Type::Thread)
        return "thread";
    else if (auto s = get<TypeFunctionPrimitiveType>(ty);
             FFlag::LuauUserTypeFunThreadBuffer && s && s->type == TypeFunctionPrimitiveType::Type::Buffer)
        return "buffer";
    else if (get<TypeFunctionUnknownType>(ty))
        return "unknown";
    else if (get<TypeFunctionNeverType>(ty))
        return "never";
    else if (get<TypeFunctionAnyType>(ty))
        return "any";
    else if (auto s = get<TypeFunctionSingletonType>(ty))
        return "singleton";
    else if (get<TypeFunctionNegationType>(ty))
        return "negation";
    else if (get<TypeFunctionUnionType>(ty))
        return "union";
    else if (get<TypeFunctionIntersectionType>(ty))
        return "intersection";
    else if (get<TypeFunctionTableType>(ty))
        return "table";
    else if (get<TypeFunctionFunctionType>(ty))
        return "function";
    else if (get<TypeFunctionClassType>(ty))
        return "class";

    LUAU_UNREACHABLE();
    luaL_error(L, "VM encountered unexpected type variant when determining tag");
}

// Luau: `type.unknown`
// Returns the type instance representing unknown
static int createUnknown(lua_State* L)
{
    allocTypeUserData(L, TypeFunctionUnknownType{});

    return 1;
}

// Luau: `type.never`
// Returns the type instance representing never
static int createNever(lua_State* L)
{
    allocTypeUserData(L, TypeFunctionNeverType{});

    return 1;
}

// Luau: `type.any`
// Returns the type instance representing any
static int createAny(lua_State* L)
{
    allocTypeUserData(L, TypeFunctionAnyType{});

    return 1;
}

// Luau: `type.boolean`
// Returns the type instance representing boolean
static int createBoolean(lua_State* L)
{
    allocTypeUserData(L, TypeFunctionPrimitiveType{TypeFunctionPrimitiveType::Boolean});

    return 1;
}

// Luau: `type.number`
// Returns the type instance representing number
static int createNumber(lua_State* L)
{
    allocTypeUserData(L, TypeFunctionPrimitiveType{TypeFunctionPrimitiveType::Number});

    return 1;
}

// Luau: `type.string`
// Returns the type instance representing string
static int createString(lua_State* L)
{
    allocTypeUserData(L, TypeFunctionPrimitiveType{TypeFunctionPrimitiveType::String});

    return 1;
}

// Luau: `type.thread`
static int createThread(lua_State* L)
{
    allocTypeUserData(L, TypeFunctionPrimitiveType{TypeFunctionPrimitiveType::Thread});

    return 1;
}

// Luau: `type.buffer`
static int createBuffer(lua_State* L)
{
    allocTypeUserData(L, TypeFunctionPrimitiveType{TypeFunctionPrimitiveType::Buffer});

    return 1;
}

// Luau: `type.singleton(value: string | boolean | nil) -> type`
// Returns the type instance representing string or boolean singleton or nil
static int createSingleton(lua_State* L)
{
    if (lua_isboolean(L, 1)) // Create boolean singleton
    {
        bool value = luaL_checkboolean(L, 1);
        allocTypeUserData(L, TypeFunctionSingletonType{TypeFunctionBooleanSingleton{value}});

        return 1;
    }

    // n.b. we cannot use lua_isstring here because lua committed the cardinal sin of calling a number a string
    if (lua_type(L, 1) == LUA_TSTRING) // Create string singleton
    {
        const char* value = luaL_checkstring(L, 1);
        allocTypeUserData(L, TypeFunctionSingletonType{TypeFunctionStringSingleton{value}});

        return 1;
    }

    if (lua_isnil(L, 1))
    {
        allocTypeUserData(L, TypeFunctionPrimitiveType(TypeFunctionPrimitiveType::NilType));

        return 1;
    }

    luaL_error(L, "types.singleton: can't create singleton from `%s` type", lua_typename(L, 1));
}

// Luau: `self:value() -> type`
// Returns the value of a singleton
static int getSingletonValue(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "type.value: expected 1 argument, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    if (auto tfpt = get<TypeFunctionPrimitiveType>(self))
    {
        if (tfpt->type != TypeFunctionPrimitiveType::NilType)
            luaL_error(L, "type.value: expected self to be a singleton, but got %s instead", getTag(L, self).c_str());

        lua_pushnil(L);
        return 1;
    }

    auto tfst = get<TypeFunctionSingletonType>(self);
    if (!tfst)
        luaL_error(L, "type.value: expected self to be a singleton, but got %s instead", getTag(L, self).c_str());

    if (auto tfbst = get<TypeFunctionBooleanSingleton>(tfst))
    {
        lua_pushboolean(L, tfbst->value);
        return 1;
    }

    if (auto tfsst = get<TypeFunctionStringSingleton>(tfst))
    {
        lua_pushlstring(L, tfsst->value.c_str(), tfsst->value.length());
        return 1;
    }

    luaL_error(L, "type.value: can't call `value` method on `%s` type", getTag(L, self).c_str());
}

// Luau: `types.unionof(...: type) -> type`
// Returns the type instance representing union
static int createUnion(lua_State* L)
{
    // get the number of arguments for union
    int argSize = lua_gettop(L);
    if (argSize < 2)
        luaL_error(L, "types.unionof: expected at least 2 types to union, but got %d", argSize);

    std::vector<TypeFunctionTypeId> components;
    components.reserve(argSize);

    for (int i = 1; i <= argSize; i++)
        components.push_back(getTypeUserData(L, i));

    allocTypeUserData(L, TypeFunctionUnionType{components});

    return 1;
}

// Luau: `types.intersectionof(...: type) -> type`
// Returns the type instance representing intersection
static int createIntersection(lua_State* L)
{
    // get the number of arguments for intersection
    int argSize = lua_gettop(L);
    if (argSize < 2)
        luaL_error(L, "types.intersectionof: expected at least 2 types to intersection, but got %d", argSize);

    std::vector<TypeFunctionTypeId> components;
    components.reserve(argSize);

    for (int i = 1; i <= argSize; i++)
        components.push_back(getTypeUserData(L, i));

    allocTypeUserData(L, TypeFunctionIntersectionType{components});

    return 1;
}

// Luau: `self:components() -> {type}`
// Returns the components of union or intersection
static int getComponents(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "type.components: expected 1 argument, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tfut = get<TypeFunctionUnionType>(self);
    if (tfut)
    {
        int argSize = int(tfut->components.size());

        lua_createtable(L, argSize, 0);
        for (int i = 0; i < argSize; i++)
        {
            TypeFunctionTypeId component = tfut->components[i];
            allocTypeUserData(L, component->type);
            lua_rawseti(L, -2, i + 1); // Luau is 1-indexed while C++ is 0-indexed
        }

        return 1;
    }

    auto tfit = get<TypeFunctionIntersectionType>(self);
    if (tfit)
    {
        int argSize = int(tfit->components.size());

        lua_createtable(L, argSize, 0);
        for (int i = 0; i < argSize; i++)
        {
            TypeFunctionTypeId component = tfit->components[i];
            allocTypeUserData(L, component->type);
            lua_rawseti(L, -2, i + 1); // Luau is 1-indexed while C++ is 0-indexed
        }

        return 1;
    }

    luaL_error(L, "type.components: cannot call components of `%s` type", getTag(L, self).c_str());
}

// Luau: `types.negationof(arg: type) -> type`
// Returns the type instance representing negation
static int createNegation(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "types.negationof: expected 1 argument, but got %d", argumentCount);

    TypeFunctionTypeId arg = getTypeUserData(L, 1);
    if (get<TypeFunctionTableType>(arg) || get<TypeFunctionFunctionType>(arg))
        luaL_error(L, "types.negationof: cannot perform negation on `%s` type", getTag(L, arg).c_str());

    allocTypeUserData(L, TypeFunctionNegationType{arg});

    return 1;
}

// Luau: `self:inner() -> type`
// Returns the type instance being negated
static int getNegatedValue(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "type.inner: expected 1 argument, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    if (auto tfnt = get<TypeFunctionNegationType>(self); !tfnt)
        allocTypeUserData(L, tfnt->type->type);
    else
        luaL_error(L, "type.inner: cannot call inner method on non-negation type: `%s` type", getTag(L, self).c_str());

    return 1;
}

// Luau: `types.newtable(props: {[type]: type | { read: type, write: type }}?, indexer: {index: type, readresult: type, writeresult: type}?,
// metatable: type?) -> type` Returns the type instance representing table
static int createTable(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount > 3)
        luaL_error(L, "types.newtable: expected 0-3 arguments, but got %d", argumentCount);

    // Parse prop
    TypeFunctionTableType::Props props{};
    if (lua_istable(L, 1))
    {
        lua_pushnil(L);
        while (lua_next(L, 1) != 0)
        {
            TypeFunctionTypeId key = getTypeUserData(L, -2);

            auto tfst = get<TypeFunctionSingletonType>(key);
            if (!tfst)
                luaL_error(L, "types.newtable: expected to be given a singleton type, but got %s instead", getTag(L, key).c_str());

            auto tfsst = get<TypeFunctionStringSingleton>(tfst);
            if (!tfsst)
                luaL_error(L, "types.newtable: expected to be given a string singleton type, but got %s instead", getTag(L, key).c_str());

            if (lua_istable(L, -1))
            {
                lua_getfield(L, -1, "read");
                std::optional<TypeFunctionTypeId> readTy;
                if (!lua_isnil(L, -1))
                    readTy = getTypeUserData(L, -1);
                lua_pop(L, 1);

                lua_getfield(L, -1, "write");
                std::optional<TypeFunctionTypeId> writeTy;
                if (!lua_isnil(L, -1))
                    writeTy = getTypeUserData(L, -1);
                lua_pop(L, 1);

                props[tfsst->value] = TypeFunctionProperty{readTy, writeTy};
            }
            else
            {
                TypeFunctionTypeId value = getTypeUserData(L, -1);
                props[tfsst->value] = TypeFunctionProperty::rw(value);
            }

            lua_pop(L, 1);
        }
    }
    else if (!lua_isnoneornil(L, 1))
        luaL_typeerrorL(L, 1, "table");

    // Parse indexer
    std::optional<TypeFunctionTableIndexer> indexer;
    if (lua_istable(L, 2))
    {
        // Parse keyType and valueType
        lua_getfield(L, 2, "index");
        TypeFunctionTypeId keyType = getTypeUserData(L, -1);
        lua_pop(L, 1);

        lua_getfield(L, 2, "readresult");
        TypeFunctionTypeId valueType = getTypeUserData(L, -1);
        lua_pop(L, 1);

        indexer = TypeFunctionTableIndexer(keyType, valueType);
    }
    else if (!lua_isnoneornil(L, 2))
        luaL_typeerrorL(L, 2, "table");

    // Parse metatable
    std::optional<TypeFunctionTypeId> metatable = optionalTypeUserData(L, 3);
    if (metatable && !get<TypeFunctionTableType>(*metatable))
        luaL_error(L, "types.newtable: expected to be given a table type as a metatable, but got %s instead", getTag(L, *metatable).c_str());

    allocTypeUserData(L, TypeFunctionTableType{props, indexer, metatable});
    return 1;
}

// Luau: `self:setproperty(key: type, value: type?)`
// Sets the properties of a table
static int setTableProp(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount < 2 || argumentCount > 3)
        luaL_error(L, "type.setproperty: expected 2-3 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tftt = getMutable<TypeFunctionTableType>(self);
    if (!tftt)
        luaL_error(L, "type.setproperty: expected self to be a table, but got %s instead", getTag(L, self).c_str());

    TypeFunctionTypeId key = getTypeUserData(L, 2);
    auto tfst = get<TypeFunctionSingletonType>(key);
    if (!tfst)
        luaL_error(L, "type.setproperty: expected to be given a singleton type, but got %s instead", getTag(L, key).c_str());

    auto tfsst = get<TypeFunctionStringSingleton>(tfst);
    if (!tfsst)
        luaL_error(L, "type.setproperty: expected to be given a string singleton type, but got %s instead", getTag(L, key).c_str());

    if (argumentCount == 2 || lua_isnil(L, 3))
    {
        tftt->props.erase(tfsst->value);
        return 0;
    }

    TypeFunctionTypeId value = getTypeUserData(L, 3);
    tftt->props[tfsst->value] = TypeFunctionProperty::rw(value, value);

    return 0;
}

// Luau: `self:setreadproperty(key: type, value: type?)`
// Sets the properties of a table
static int setReadTableProp(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount < 2 || argumentCount > 3)
        luaL_error(L, "type.setreadproperty: expected 2-3 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tftt = getMutable<TypeFunctionTableType>(self);
    if (!tftt)
        luaL_error(L, "type.setreadproperty: expected self to be a table, but got %s instead", getTag(L, self).c_str());

    TypeFunctionTypeId key = getTypeUserData(L, 2);
    auto tfst = get<TypeFunctionSingletonType>(key);
    if (!tfst)
        luaL_error(L, "type.setreadproperty: expected to be given a singleton type, but got %s instead", getTag(L, key).c_str());

    auto tfsst = get<TypeFunctionStringSingleton>(tfst);
    if (!tfsst)
        luaL_error(L, "type.setreadproperty: expected to be given a string singleton type, but got %s instead", getTag(L, key).c_str());

    auto iter = tftt->props.find(tfsst->value);

    if (argumentCount == 2 || lua_isnil(L, 3))
    {
        // if it's read-only, remove it altogether
        if (iter != tftt->props.end() && iter->second.isReadOnly())
            tftt->props.erase(tfsst->value);
        // but if it's not, just null out the read type.
        else if (iter != tftt->props.end())
            iter->second.readTy = std::nullopt;

        return 0;
    }

    TypeFunctionTypeId value = getTypeUserData(L, 3);
    if (iter == tftt->props.end())
        tftt->props[tfsst->value] = TypeFunctionProperty::readonly(value);
    else
        iter->second.readTy = value;

    return 0;
}

// Luau: `self:setwriteproperty(key: type, value: type?)`
// Sets the properties of a table
static int setWriteTableProp(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount < 2 || argumentCount > 3)
        luaL_error(L, "type.setwriteproperty: expected 2-3 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tftt = getMutable<TypeFunctionTableType>(self);
    if (!tftt)
        luaL_error(L, "type.setwriteproperty: expected self to be a table, but got %s instead", getTag(L, self).c_str());

    TypeFunctionTypeId key = getTypeUserData(L, 2);
    auto tfst = get<TypeFunctionSingletonType>(key);
    if (!tfst)
        luaL_error(L, "type.setwriteproperty: expected to be given a singleton type, but got %s instead", getTag(L, key).c_str());

    auto tfsst = get<TypeFunctionStringSingleton>(tfst);
    if (!tfsst)
        luaL_error(L, "type.setwriteproperty: expected to be given a string singleton type, but got %s instead", getTag(L, key).c_str());

    auto iter = tftt->props.find(tfsst->value);

    if (argumentCount == 2 || lua_isnil(L, 3))
    {
        // if it's write-only, remove it altogether
        if (iter != tftt->props.end() && iter->second.isWriteOnly())
            tftt->props.erase(tfsst->value);
        // but if it's not, just null out the write type.
        else if (iter != tftt->props.end())
            iter->second.writeTy = std::nullopt;

        return 0;
    }

    TypeFunctionTypeId value = getTypeUserData(L, 3);
    if (iter == tftt->props.end())
        tftt->props[tfsst->value] = TypeFunctionProperty::writeonly(value);
    else
        iter->second.writeTy = value;

    return 0;
}

// Luau: `self:readproperty(key: type) -> type`
// Returns the property of a table associated with the key
static int readTableProp(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 2)
        luaL_error(L, "type.readproperty: expected 2 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tftt = get<TypeFunctionTableType>(self);
    if (!tftt)
        luaL_error(L, "type.readproperty: expected self to be either a table, but got %s instead", getTag(L, self).c_str());

    TypeFunctionTypeId key = getTypeUserData(L, 2);
    auto tfst = get<TypeFunctionSingletonType>(key);
    if (!tfst)
        luaL_error(L, "type.readproperty: expected to be given a singleton type, but got %s instead", getTag(L, key).c_str());

    auto tfsst = get<TypeFunctionStringSingleton>(tfst);
    if (!tfsst)
        luaL_error(L, "type.readproperty: expected to be given a string singleton type, but got %s instead", getTag(L, key).c_str());

    // Check if key is a valid prop
    if (tftt->props.find(tfsst->value) == tftt->props.end())
    {
        lua_pushnil(L);
        return 1;
    }

    auto prop = tftt->props.at(tfsst->value);
    if (prop.readTy)
        allocTypeUserData(L, (*prop.readTy)->type);
    else if (FFlag::LuauUserTypeFunFixNoReadWrite)
        lua_pushnil(L);
    else
        luaL_error(L, "type.readproperty: property %s is write-only, and therefore does not have a read type.", tfsst->value.c_str());

    return 1;
}
//
// Luau: `self:writeproperty(key: type) -> type`
// Returns the property of a table associated with the key
static int writeTableProp(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 2)
        luaL_error(L, "type.writeproperty: expected 2 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tftt = get<TypeFunctionTableType>(self);
    if (!tftt)
        luaL_error(L, "type.writeproperty: expected self to be either a table, but got %s instead", getTag(L, self).c_str());

    TypeFunctionTypeId key = getTypeUserData(L, 2);
    auto tfst = get<TypeFunctionSingletonType>(key);
    if (!tfst)
        luaL_error(L, "type.writeproperty: expected to be given a singleton type, but got %s instead", getTag(L, key).c_str());

    auto tfsst = get<TypeFunctionStringSingleton>(tfst);
    if (!tfsst)
        luaL_error(L, "type.writeproperty: expected to be given a string singleton type, but got %s instead", getTag(L, key).c_str());

    // Check if key is a valid prop
    if (tftt->props.find(tfsst->value) == tftt->props.end())
    {
        lua_pushnil(L);
        return 1;
    }

    auto prop = tftt->props.at(tfsst->value);
    if (prop.writeTy)
        allocTypeUserData(L, (*prop.writeTy)->type);
    else if (FFlag::LuauUserTypeFunFixNoReadWrite)
        lua_pushnil(L);
    else
        luaL_error(L, "type.writeproperty: property %s is read-only, and therefore does not have a write type.", tfsst->value.c_str());

    return 1;
}

// Luau: `self:setindexer(key: type, value: type)`
// Sets the indexer of the table, if the key type is `never`, the indexer is removed
static int setTableIndexer(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 3)
        luaL_error(L, "type.setindexer: expected 3 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tftt = getMutable<TypeFunctionTableType>(self);
    if (!tftt)
        luaL_error(L, "type.setindexer: expected self to be either a table, but got %s instead", getTag(L, self).c_str());

    TypeFunctionTypeId key = getTypeUserData(L, 2);
    TypeFunctionTypeId value = getTypeUserData(L, 3);

    if (auto tfnt = get<TypeFunctionNeverType>(key))
    {
        tftt->indexer = std::nullopt;
        return 0;
    }

    tftt->indexer = TypeFunctionTableIndexer{key, value};
    return 0;
}

// Luau: `self:setreadindexer(key: type, value: type)`
// Sets the read indexer of the table
static int setTableReadIndexer(lua_State* L)
{
    luaL_error(L, "type.setreadindexer: luau does not yet support separate read/write types for indexers.");
}

// Luau: `self:setwriteindexer(key: type, value: type)`
// Sets the write indexer of the table
static int setTableWriteIndexer(lua_State* L)
{
    luaL_error(L, "type.setwriteindexer: luau does not yet support separate read/write types for indexers.");
}

// Luau: `self:setmetatable(arg: type)`
// Sets the metatable of the table
static int setTableMetatable(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 2)
        luaL_error(L, "type.setmetatable: expected 2 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);

    auto tftt = getMutable<TypeFunctionTableType>(self);
    if (!tftt)
        luaL_error(L, "type.setmetatable: expected self to be a table, but got %s instead", getTag(L, self).c_str());

    TypeFunctionTypeId arg = getTypeUserData(L, 2);
    if (!get<TypeFunctionTableType>(arg))
        luaL_error(L, "type.setmetatable: expected the argument to be a table, but got %s instead", getTag(L, self).c_str());

    tftt->metatable = arg;

    return 0;
}

// Luau: `types.newfunction(parameters: {head: {type}?, tail: type?}, returns: {head: {type}?, tail: type?}) -> type`
// Returns the type instance representing a function
static int createFunction(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount > 2)
        luaL_error(L, "types.newfunction: expected 0-2 arguments, but got %d", argumentCount);

    TypeFunctionTypePackId argTypes = allocateTypeFunctionTypePack(L, TypeFunctionTypePack{});
    if (lua_istable(L, 1))
    {
        std::vector<TypeFunctionTypeId> head{};
        lua_getfield(L, 1, "head");
        if (lua_istable(L, -1))
        {
            int argSize = lua_objlen(L, -1);
            for (int i = 1; i <= argSize; i++)
            {
                lua_pushinteger(L, i);
                lua_gettable(L, -2);

                if (lua_isnil(L, -1))
                {
                    lua_pop(L, 1);
                    break;
                }

                TypeFunctionTypeId ty = getTypeUserData(L, -1);
                head.push_back(ty);

                lua_pop(L, 1); // Remove `ty` from stack
            }
        }
        lua_pop(L, 1); // Pop the "head" field

        std::optional<TypeFunctionTypePackId> tail;
        lua_getfield(L, 1, "tail");
        if (auto type = optionalTypeUserData(L, -1))
            tail = allocateTypeFunctionTypePack(L, TypeFunctionVariadicTypePack{*type});
        lua_pop(L, 1); // Pop the "tail" field

        if (head.size() == 0 && tail.has_value())
            argTypes = *tail;
        else
            argTypes = allocateTypeFunctionTypePack(L, TypeFunctionTypePack{head, tail});
    }
    else if (!lua_isnoneornil(L, 1))
        luaL_typeerrorL(L, 1, "table");

    TypeFunctionTypePackId retTypes = allocateTypeFunctionTypePack(L, TypeFunctionTypePack{});
    if (lua_istable(L, 2))
    {
        std::vector<TypeFunctionTypeId> head{};
        lua_getfield(L, 2, "head");
        if (lua_istable(L, -1))
        {
            int argSize = lua_objlen(L, -1);
            for (int i = 1; i <= argSize; i++)
            {
                lua_pushinteger(L, i);
                lua_gettable(L, -2);

                if (lua_isnil(L, -1))
                {
                    lua_pop(L, 1);
                    break;
                }

                TypeFunctionTypeId ty = getTypeUserData(L, -1);
                head.push_back(ty);

                lua_pop(L, 1); // Remove `ty` from stack
            }
        }
        lua_pop(L, 1); // Pop the "head" field

        std::optional<TypeFunctionTypePackId> tail;
        lua_getfield(L, 2, "tail");
        if (auto type = optionalTypeUserData(L, -1))
            tail = allocateTypeFunctionTypePack(L, TypeFunctionVariadicTypePack{*type});
        lua_pop(L, 1); // Pop the "tail" field

        if (head.size() == 0 && tail.has_value())
            retTypes = *tail;
        else
            retTypes = allocateTypeFunctionTypePack(L, TypeFunctionTypePack{head, tail});
    }
    else if (!lua_isnoneornil(L, 2))
        luaL_typeerrorL(L, 2, "table");

    allocTypeUserData(L, TypeFunctionFunctionType{argTypes, retTypes});

    return 1;
}

// Luau: `self:setparameters(head: {type}?, tail: type?)`
// Sets the parameters of the function
static int setFunctionParameters(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount > 3 || argumentCount < 1)
        luaL_error(L, "type.setparameters: expected 1-3, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tfft = getMutable<TypeFunctionFunctionType>(self);
    if (!tfft)
        luaL_error(L, "type.setparameters: expected self to be a function, but got %s instead", getTag(L, self).c_str());

    std::vector<TypeFunctionTypeId> head{};
    if (lua_istable(L, 2))
    {
        int argSize = lua_objlen(L, 2);
        for (int i = 1; i <= argSize; i++)
        {
            lua_pushinteger(L, i);
            lua_gettable(L, 2);

            if (lua_isnil(L, -1))
            {
                lua_pop(L, 1);
                break;
            }

            TypeFunctionTypeId ty = getTypeUserData(L, -1);
            head.push_back(ty);

            lua_pop(L, 1); // Remove `ty` from stack
        }
    }
    else if (!lua_isnoneornil(L, 2))
        luaL_typeerrorL(L, 2, "table");

    std::optional<TypeFunctionTypePackId> tail;
    if (auto type = optionalTypeUserData(L, 3))
        tail = allocateTypeFunctionTypePack(L, TypeFunctionVariadicTypePack{*type});

    if (head.size() == 0 && tail.has_value()) // Make argTypes a variadic type pack
        tfft->argTypes = *tail;
    else // Make argTypes a type pack
        tfft->argTypes = allocateTypeFunctionTypePack(L, TypeFunctionTypePack{head, tail});

    return 0;
}

// Luau: `self:parameters() -> {head: {type}?, tail: type?}`
// Returns the parameters of the function
static int getFunctionParameters(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "type.parameters: expected 1 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tfft = get<TypeFunctionFunctionType>(self);
    if (!tfft)
        luaL_error(L, "type.parameters: expected self to be a function, but got %s instead", getTag(L, self).c_str());

    if (auto tftp = get<TypeFunctionTypePack>(tfft->argTypes))
    {
        int size = 0;
        if (tftp->head.size() > 0)
            size++;
        if (tftp->tail.has_value())
            size++;

        lua_createtable(L, 0, size);

        int argSize = (int)tftp->head.size();
        if (argSize > 0)
        {
            lua_createtable(L, argSize, 0);
            for (int i = 0; i < argSize; i++)
            {
                allocTypeUserData(L, tftp->head[i]->type);
                lua_rawseti(L, -2, i + 1); // Luau is 1-indexed while C++ is 0-indexed
            }
            lua_setfield(L, -2, "head");
        }

        if (tftp->tail.has_value())
        {
            auto tfvp = get<TypeFunctionVariadicTypePack>(*tftp->tail);
            if (!tfvp)
                LUAU_ASSERT(!"We should only be supporting variadic packs as TypeFunctionTypePack.tail at the moment");

            allocTypeUserData(L, tfvp->type->type);
            lua_setfield(L, -2, "tail");
        }

        return 1;
    }

    if (auto tfvp = get<TypeFunctionVariadicTypePack>(tfft->argTypes))
    {
        lua_createtable(L, 0, 1);

        allocTypeUserData(L, tfvp->type->type);
        lua_setfield(L, -2, "tail");

        return 1;
    }

    lua_createtable(L, 0, 0);
    return 1;
}

// Luau: `self:setreturns(head: {type}?, tail: type?)`
// Sets the returns of the function
static int setFunctionReturns(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount < 2 || argumentCount > 3)
        luaL_error(L, "type.setreturns: expected 1-3 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tfft = getMutable<TypeFunctionFunctionType>(self);
    if (!tfft)
        luaL_error(L, "type.setreturns: expected self to be a function, but got %s instead", getTag(L, self).c_str());

    std::vector<TypeFunctionTypeId> head{};
    if (lua_istable(L, 2))
    {
        int argSize = lua_objlen(L, 2);
        for (int i = 1; i <= argSize; i++)
        {
            lua_pushinteger(L, i);
            lua_gettable(L, 2);

            if (lua_isnil(L, -1))
            {
                lua_pop(L, 1);
                break;
            }

            TypeFunctionTypeId ty = getTypeUserData(L, -1);
            head.push_back(ty);

            lua_pop(L, 1); // Remove `ty` from stack
        }
    }
    else if (!lua_isnoneornil(L, 2))
        luaL_typeerrorL(L, 2, "table");

    std::optional<TypeFunctionTypePackId> tail;
    if (auto type = optionalTypeUserData(L, 3))
        tail = allocateTypeFunctionTypePack(L, TypeFunctionVariadicTypePack{*type});

    if (head.size() == 0 && tail.has_value()) // Make retTypes a variadic type pack
        tfft->retTypes = *tail;
    else // Make retTypes a type pack
        tfft->retTypes = allocateTypeFunctionTypePack(L, TypeFunctionTypePack{head, tail});

    return 0;
}

// Luau: `self:returns() -> {head: {type}?, tail: type?}`
// Returns the returns of the function
static int getFunctionReturns(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "type.returns: expected 1 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tfft = get<TypeFunctionFunctionType>(self);
    if (!tfft)
        luaL_error(L, "type.returns: expected self to be a function, but got %s instead", getTag(L, self).c_str());

    if (auto tftp = get<TypeFunctionTypePack>(tfft->retTypes))
    {
        int size = 0;
        if (tftp->head.size() > 0)
            size++;
        if (tftp->tail.has_value())
            size++;

        lua_createtable(L, 0, size);

        int argSize = (int)tftp->head.size();
        if (argSize > 0)
        {
            lua_createtable(L, argSize, 0);
            for (int i = 0; i < argSize; i++)
            {
                allocTypeUserData(L, tftp->head[i]->type);
                lua_rawseti(L, -2, i + 1); // Luau is 1-indexed while C++ is 0-indexed
            }
            lua_setfield(L, -2, "head");
        }

        if (tftp->tail.has_value())
        {
            auto tfvp = get<TypeFunctionVariadicTypePack>(*tftp->tail);
            if (!tfvp)
                LUAU_ASSERT(!"We should only be supporting variadic packs as TypeFunctionTypePack.tail at the moment");

            allocTypeUserData(L, tfvp->type->type);
            lua_setfield(L, -2, "tail");
        }

        return 1;
    }

    if (auto tfvp = get<TypeFunctionVariadicTypePack>(tfft->retTypes))
    {
        lua_createtable(L, 0, 1);

        allocTypeUserData(L, tfvp->type->type);
        lua_setfield(L, -2, "tail");

        return 1;
    }

    lua_createtable(L, 0, 0);
    return 1;
}

// Luau: `self:parent() -> type`
// Returns the parent of a class type
static int getClassParent(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "type.parent: expected 1 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tfct = get<TypeFunctionClassType>(self);
    if (!tfct)
        luaL_error(L, "type.parent: expected self to be a class, but got %s instead", getTag(L, self).c_str());

    // If the parent does not exist, we should return nil
    if (!tfct->parent)
        lua_pushnil(L);
    else
        allocTypeUserData(L, (*tfct->parent)->type);

    return 1;
}

// Luau: `self:properties() -> {[type]: { read: type?, write: type? }}`
// Returns the properties of a table or class type
static int getProps(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "type.properties: expected 1 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    if (auto tftt = get<TypeFunctionTableType>(self))
    {
        lua_createtable(L, int(tftt->props.size()), 0);
        for (auto& [name, prop] : tftt->props)
        {
            allocTypeUserData(L, TypeFunctionSingletonType{TypeFunctionStringSingleton{name}});

            int size = 0;
            if (prop.readTy)
                size++;
            if (prop.writeTy)
                size++;

            lua_createtable(L, 0, size);
            if (prop.readTy)
            {
                allocTypeUserData(L, (*prop.readTy)->type);
                lua_setfield(L, -2, "read");
            }

            if (prop.writeTy)
            {
                allocTypeUserData(L, (*prop.writeTy)->type);
                lua_setfield(L, -2, "write");
            }

            lua_settable(L, -3);
        }

        return 1;
    }

    if (auto tfct = get<TypeFunctionClassType>(self))
    {
        lua_createtable(L, int(tfct->props.size()), 0);
        for (auto& [name, prop] : tfct->props)
        {
            allocTypeUserData(L, TypeFunctionSingletonType{TypeFunctionStringSingleton{name}});

            int size = 0;
            if (prop.readTy)
                size++;
            if (prop.writeTy)
                size++;

            lua_createtable(L, 0, size);
            if (prop.readTy)
            {
                allocTypeUserData(L, (*prop.readTy)->type);
                lua_setfield(L, -2, "read");
            }

            if (prop.writeTy)
            {
                allocTypeUserData(L, (*prop.writeTy)->type);
                lua_setfield(L, -2, "write");
            }

            lua_settable(L, -3);
        }

        return 1;
    }

    luaL_error(L, "type.properties: expected self to be either a table or class, but got %s instead", getTag(L, self).c_str());
}

// Luau: `self:indexer() -> {index: type, readresult: type, writeresult: type}?`
// Returns the indexer of a table or class type
static int getIndexer(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "type.indexer: expected 1 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    if (auto tftt = get<TypeFunctionTableType>(self))
    {
        // if the indexer does not exist, we should return nil
        if (!tftt->indexer.has_value())
            lua_pushnil(L);
        else
        {
            lua_createtable(L, 0, 3);
            allocTypeUserData(L, tftt->indexer->keyType->type);
            lua_setfield(L, -2, "index");
            allocTypeUserData(L, tftt->indexer->valueType->type);
            lua_setfield(L, -2, "readresult");
            allocTypeUserData(L, tftt->indexer->valueType->type);
            lua_setfield(L, -2, "writeresult");
        }

        return 1;
    }

    if (auto tfct = get<TypeFunctionClassType>(self))
    {
        // if the indexer does not exist, we should return nil
        if (!tfct->indexer.has_value())
            lua_pushnil(L);
        else
        {
            lua_createtable(L, 0, 3);
            allocTypeUserData(L, tfct->indexer->keyType->type);
            lua_setfield(L, -2, "index");
            allocTypeUserData(L, tfct->indexer->valueType->type);
            lua_setfield(L, -2, "readresult");
            allocTypeUserData(L, tfct->indexer->valueType->type);
            lua_setfield(L, -2, "writeresult");
        }

        return 1;
    }

    luaL_error(L, "type.indexer: self to be either a table or class, but got %s instead", getTag(L, self).c_str());
}

// Luau: `self:readindexer() -> {index: type, result: type}?`
// Returns the read indexer of a table or class type
static int getReadIndexer(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "type.readindexer: expected 1 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    if (auto tftt = get<TypeFunctionTableType>(self))
    {
        // if the indexer does not exist, we should return nil
        if (!tftt->indexer.has_value())
            lua_pushnil(L);
        else
        {
            lua_createtable(L, 0, 2);
            allocTypeUserData(L, tftt->indexer->keyType->type);
            lua_setfield(L, -2, "index");
            allocTypeUserData(L, tftt->indexer->valueType->type);
            lua_setfield(L, -2, "result");
        }

        return 1;
    }

    if (auto tfct = get<TypeFunctionClassType>(self))
    {
        // if the indexer does not exist, we should return nil
        if (!tfct->indexer.has_value())
            lua_pushnil(L);
        else
        {
            lua_createtable(L, 0, 2);
            allocTypeUserData(L, tfct->indexer->keyType->type);
            lua_setfield(L, -2, "index");
            allocTypeUserData(L, tfct->indexer->valueType->type);
            lua_setfield(L, -2, "result");
        }

        return 1;
    }

    luaL_error(L, "type.readindexer: expected self to be either a table or class, but got %s instead", getTag(L, self).c_str());
}

// Luau: `self:writeindexer() -> {index: type, result: type}?`
// Returns the write indexer of a table or class type
static int getWriteIndexer(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "type.writeindexer: expected 1 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    if (auto tftt = get<TypeFunctionTableType>(self))
    {
        // if the indexer does not exist, we should return nil
        if (!tftt->indexer.has_value())
            lua_pushnil(L);
        else
        {
            lua_createtable(L, 0, 2);
            allocTypeUserData(L, tftt->indexer->keyType->type);
            lua_setfield(L, -2, "index");
            allocTypeUserData(L, tftt->indexer->valueType->type);
            lua_setfield(L, -2, "result");
        }

        return 1;
    }

    if (auto tfct = get<TypeFunctionClassType>(self))
    {
        // if the indexer does not exist, we should return nil
        if (!tfct->indexer.has_value())
            lua_pushnil(L);
        else
        {
            lua_createtable(L, 0, 2);
            allocTypeUserData(L, tfct->indexer->keyType->type);
            lua_setfield(L, -2, "index");
            allocTypeUserData(L, tfct->indexer->valueType->type);
            lua_setfield(L, -2, "result");
        }

        return 1;
    }

    luaL_error(L, "type.writeindexer: expected self to be either a table or class, but got %s instead", getTag(L, self).c_str());
}

// Luau: `self:metatable() -> type?`
// Returns the metatable of a table or class type
static int getMetatable(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "type.metatable: expected 1 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    if (auto tfmt = get<TypeFunctionTableType>(self))
    {
        // if the metatable does not exist, we should return nil
        if (!tfmt->metatable.has_value())
            lua_pushnil(L);
        else
            allocTypeUserData(L, (*tfmt->metatable)->type);

        return 1;
    }

    if (auto tfct = get<TypeFunctionClassType>(self))
    {
        // if the metatable does not exist, we should return nil
        if (!tfct->metatable.has_value())
            lua_pushnil(L);
        else
            allocTypeUserData(L, (*tfct->metatable)->type);

        return 1;
    }

    luaL_error(L, "type.metatable: expected self to be a table or class, but got %s instead", getTag(L, self).c_str());
}

// Luau: `self:is(arg: string) -> boolean`
// Returns true if given argument is a tag of self
static int checkTag(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 2)
        luaL_error(L, "type.is: expected 2 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    std::string arg = luaL_checkstring(L, 2);

    lua_pushboolean(L, getTag(L, self) == arg);
    return 1;
}

TypeFunctionTypeId deepClone(NotNull<TypeFunctionRuntime> runtime, TypeFunctionTypeId ty); // Forward declaration

// Luau: `types.copy(arg: string) -> type`
// Returns a deep copy of the argument
static int deepCopy(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "types.copy: expected 1 arguments, but got %d", argumentCount);

    TypeFunctionTypeId arg = getTypeUserData(L, 1);

    TypeFunctionTypeId copy = deepClone(NotNull{getTypeFunctionRuntime(L)}, arg);
    allocTypeUserData(L, copy->type);
    return 1;
}

// Luau: `self == arg -> boolean`
// Used to set the __eq metamethod
static int isEqualToType(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 2)
        luaL_error(L, "expected 2 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    TypeFunctionTypeId arg = getTypeUserData(L, 2);

    lua_pushboolean(L, *self == *arg);
    return 1;
}

void registerTypesLibrary(lua_State* L)
{
    LUAU_ASSERT(FFlag::LuauUserTypeFunFixRegister);

    luaL_Reg fields[] = {
        {"unknown", createUnknown},
        {"never", createNever},
        {"any", createAny},
        {"boolean", createBoolean},
        {"number", createNumber},
        {"string", createString},
        {FFlag::LuauUserTypeFunThreadBuffer ? "thread" : nullptr, FFlag::LuauUserTypeFunThreadBuffer ? createThread : nullptr},
        {FFlag::LuauUserTypeFunThreadBuffer ? "buffer" : nullptr, FFlag::LuauUserTypeFunThreadBuffer ? createBuffer : nullptr},
        {nullptr, nullptr}
    };

    luaL_Reg methods[] = {
        {"singleton", createSingleton},
        {"negationof", createNegation},
        {"unionof", createUnion},
        {"intersectionof", createIntersection},
        {"newtable", createTable},
        {"newfunction", createFunction},
        {"copy", deepCopy},

        {nullptr, nullptr}
    };

    luaL_register(L, "types", methods);

    // Set fields for type userdata
    for (luaL_Reg* l = fields; l->name; l++)
    {
        l->func(L);
        lua_setfield(L, -2, l->name);
    }

    lua_pop(L, 1);
}

static int typeUserdataIndex(lua_State* L)
{
    TypeFunctionTypeId self = getTypeUserData(L, 1);
    const char* field = luaL_checkstring(L, 2);

    if (strcmp(field, "tag") == 0)
    {
        lua_pushstring(L, getTag(L, self).c_str());
        return 1;
    }

    lua_pushvalue(L, lua_upvalueindex(1));
    lua_getfield(L, -1, field);
    return 1;
}

void registerTypeUserData(lua_State* L)
{
    if (FFlag::LuauUserTypeFunFixRegister)
    {
        luaL_Reg typeUserdataMethods[] = {
            {"is", checkTag},

            // Negation type methods
            {"inner", getNegatedValue},

            // Singleton type methods
            {"value", getSingletonValue},

            // Table type methods
            {"setproperty", setTableProp},
            {"setreadproperty", setReadTableProp},
            {"setwriteproperty", setWriteTableProp},
            {"readproperty", readTableProp},
            {"writeproperty", writeTableProp},
            {"properties", getProps},
            {"setindexer", setTableIndexer},
            {"setreadindexer", setTableReadIndexer},
            {"setwriteindexer", setTableWriteIndexer},
            {"indexer", getIndexer},
            {"readindexer", getReadIndexer},
            {"writeindexer", getWriteIndexer},
            {"setmetatable", setTableMetatable},
            {"metatable", getMetatable},

            // Function type methods
            {"setparameters", setFunctionParameters},
            {"parameters", getFunctionParameters},
            {"setreturns", setFunctionReturns},
            {"returns", getFunctionReturns},

            // Union and Intersection type methods
            {"components", getComponents},

            // Class type methods
            {"parent", getClassParent},

            {nullptr, nullptr}
        };

        // Create and register metatable for type userdata
        luaL_newmetatable(L, "type");

        // Protect metatable from being changed
        lua_pushstring(L, "The metatable is locked");
        lua_setfield(L, -2, "__metatable");

        lua_pushcfunction(L, isEqualToType, "__eq");
        lua_setfield(L, -2, "__eq");

        // Indexing will be a dynamic function because some type fields are dynamic
        lua_newtable(L);
        luaL_register(L, nullptr, typeUserdataMethods);
        lua_setreadonly(L, -1, true);
        lua_pushcclosure(L, typeUserdataIndex, "__index", 1);
        lua_setfield(L, -2, "__index");

        lua_setreadonly(L, -1, true);
        lua_pop(L, 1);
    }
    else
    {
        // List of fields for type userdata
        luaL_Reg typeUserdataFields[] = {
            {"unknown", createUnknown},
            {"never", createNever},
            {"any", createAny},
            {"boolean", createBoolean},
            {"number", createNumber},
            {"string", createString},
            {nullptr, nullptr}
        };

        // List of methods for type userdata
        luaL_Reg typeUserdataMethods[] = {
            {"singleton", createSingleton},
            {"negationof", createNegation},
            {"unionof", createUnion},
            {"intersectionof", createIntersection},
            {"newtable", createTable},
            {"newfunction", createFunction},
            {"copy", deepCopy},

            // Common methods
            {"is", checkTag},

            // Negation type methods
            {"inner", getNegatedValue},

            // Singleton type methods
            {"value", getSingletonValue},

            // Table type methods
            {"setproperty", setTableProp},
            {"setreadproperty", setReadTableProp},
            {"setwriteproperty", setWriteTableProp},
            {"readproperty", readTableProp},
            {"writeproperty", writeTableProp},
            {"properties", getProps},
            {"setindexer", setTableIndexer},
            {"setreadindexer", setTableReadIndexer},
            {"setwriteindexer", setTableWriteIndexer},
            {"indexer", getIndexer},
            {"readindexer", getReadIndexer},
            {"writeindexer", getWriteIndexer},
            {"setmetatable", setTableMetatable},
            {"metatable", getMetatable},

            // Function type methods
            {"setparameters", setFunctionParameters},
            {"parameters", getFunctionParameters},
            {"setreturns", setFunctionReturns},
            {"returns", getFunctionReturns},

            // Union and Intersection type methods
            {"components", getComponents},

            // Class type methods
            {"parent", getClassParent},
            {"indexer", getIndexer},
            {nullptr, nullptr}
        };

        // Create and register metatable for type userdata
        luaL_newmetatable(L, "type");

        // Protect metatable from being fetched.
        lua_pushstring(L, "The metatable is locked");
        lua_setfield(L, -2, "__metatable");

        // Set type userdata metatable's __eq to type_equals()
        lua_pushcfunction(L, isEqualToType, "__eq");
        lua_setfield(L, -2, "__eq");

        // Set type userdata metatable's __index to itself
        lua_pushvalue(L, -1); // Push a copy of type userdata metatable
        lua_setfield(L, -2, "__index");

        luaL_register(L, nullptr, typeUserdataMethods);

        // Set fields for type userdata
        for (luaL_Reg* l = typeUserdataFields; l->name; l++)
        {
            l->func(L);
            lua_setfield(L, -2, l->name);
        }

        // Set types library as a global name "types"
        lua_setglobal(L, "types");
    }

    // Sets up a destructor for the type userdata.
    lua_setuserdatadtor(L, kTypeUserdataTag, deallocTypeUserData);
}

// Used to redirect all the removed global functions to say "this function is unsupported"
int unsupportedFunction(lua_State* L)
{
    luaL_errorL(L, "this function is not supported in type functions");
    return 0;
}

// Add libraries / globals for type function environment
void setTypeFunctionEnvironment(lua_State* L)
{
    // Register math library
    luaopen_math(L);
    lua_pop(L, 1);

    // Register table library
    luaopen_table(L);
    lua_pop(L, 1);

    // Register string library
    luaopen_string(L);
    lua_pop(L, 1);

    // Register bit32 library
    luaopen_bit32(L);
    lua_pop(L, 1);

    // Register utf8 library
    luaopen_utf8(L);
    lua_pop(L, 1);

    // Register buffer library
    luaopen_buffer(L);
    lua_pop(L, 1);

    // Register base library
    luaopen_base(L);
    lua_pop(L, 1);

    // Remove certain global functions from the base library
    static const std::string unavailableGlobals[] = {"gcinfo", "getfenv", "newproxy", "setfenv", "pcall", "xpcall"};
    for (auto& name : unavailableGlobals)
    {
        lua_pushcfunction(L, unsupportedFunction, "Removing global function from type function environment");
        lua_setglobal(L, name.c_str());
    }
}

void resetTypeFunctionState(lua_State* L)
{
    lua_getglobal(L, "math");
    lua_getfield(L, -1, "randomseed");
    lua_pushnumber(L, 0);
    lua_call(L, 1, 0);
    lua_pop(L, 1);
}

/*
 * Below are helper methods for __eq
 * Same as one from Type.cpp
 */
using SeenSet = std::set<std::pair<const void*, const void*>>;
bool areEqual(SeenSet& seen, const TypeFunctionType& lhs, const TypeFunctionType& rhs);
bool areEqual(SeenSet& seen, const TypeFunctionTypePackVar& lhs, const TypeFunctionTypePackVar& rhs);

bool seenSetContains(SeenSet& seen, const void* lhs, const void* rhs)
{
    if (lhs == rhs)
        return true;

    auto p = std::make_pair(lhs, rhs);
    if (seen.find(p) != seen.end())
        return true;

    seen.insert(p);
    return false;
}

bool areEqual(SeenSet& seen, const TypeFunctionSingletonType& lhs, const TypeFunctionSingletonType& rhs)
{
    if (seenSetContains(seen, &lhs, &rhs))
        return true;

    {
        const TypeFunctionBooleanSingleton* lp = get<TypeFunctionBooleanSingleton>(&lhs);
        const TypeFunctionBooleanSingleton* rp = get<TypeFunctionBooleanSingleton>(&lhs);
        if (lp && rp)
            return lp->value == rp->value;
    }

    {
        const TypeFunctionStringSingleton* lp = get<TypeFunctionStringSingleton>(&lhs);
        const TypeFunctionStringSingleton* rp = get<TypeFunctionStringSingleton>(&lhs);
        if (lp && rp)
            return lp->value == rp->value;
    }

    return false;
}

bool areEqual(SeenSet& seen, const TypeFunctionUnionType& lhs, const TypeFunctionUnionType& rhs)
{
    if (seenSetContains(seen, &lhs, &rhs))
        return true;

    if (lhs.components.size() != rhs.components.size())
        return false;

    auto l = lhs.components.begin();
    auto r = rhs.components.begin();

    while (l != lhs.components.end())
    {
        if (!areEqual(seen, **l, **r))
            return false;
        ++l;
        ++r;
    }

    return true;
}

bool areEqual(SeenSet& seen, const TypeFunctionIntersectionType& lhs, const TypeFunctionIntersectionType& rhs)
{
    if (seenSetContains(seen, &lhs, &rhs))
        return true;

    if (lhs.components.size() != rhs.components.size())
        return false;

    auto l = lhs.components.begin();
    auto r = rhs.components.begin();

    while (l != lhs.components.end())
    {
        if (!areEqual(seen, **l, **r))
            return false;
        ++l;
        ++r;
    }

    return true;
}

bool areEqual(SeenSet& seen, const TypeFunctionNegationType& lhs, const TypeFunctionNegationType& rhs)
{
    if (seenSetContains(seen, &lhs, &rhs))
        return true;

    return areEqual(seen, *lhs.type, *rhs.type);
}

bool areEqual(SeenSet& seen, const TypeFunctionTableType& lhs, const TypeFunctionTableType& rhs)
{
    if (seenSetContains(seen, &lhs, &rhs))
        return true;

    if (lhs.props.size() != rhs.props.size())
        return false;

    if (bool(lhs.indexer) != bool(rhs.indexer))
        return false;

    if (lhs.indexer && rhs.indexer)
    {
        if (!areEqual(seen, *lhs.indexer->keyType, *rhs.indexer->keyType))
            return false;

        if (!areEqual(seen, *lhs.indexer->valueType, *rhs.indexer->valueType))
            return false;
    }

    auto l = lhs.props.begin();
    auto r = rhs.props.begin();

    while (l != lhs.props.end())
    {
        if ((l->second.readTy && !r->second.readTy) || (!l->second.readTy && r->second.readTy))
            return false;

        if (l->second.readTy && r->second.readTy && !areEqual(seen, **(l->second.readTy), **(r->second.readTy)))
            return false;

        if ((l->second.writeTy && !r->second.writeTy) || (!l->second.writeTy && r->second.writeTy))
            return false;

        if (l->second.writeTy && r->second.writeTy && !areEqual(seen, **(l->second.writeTy), **(r->second.writeTy)))
            return false;

        ++l;
        ++r;
    }

    return true;
}

bool areEqual(SeenSet& seen, const TypeFunctionFunctionType& lhs, const TypeFunctionFunctionType& rhs)
{
    if (seenSetContains(seen, &lhs, &rhs))
        return true;

    if (bool(lhs.argTypes) != bool(rhs.argTypes))
        return false;

    if (lhs.argTypes && rhs.argTypes)
    {
        if (!areEqual(seen, *lhs.argTypes, *rhs.argTypes))
            return false;
    }

    if (bool(lhs.retTypes) != bool(rhs.retTypes))
        return false;

    if (lhs.retTypes && rhs.retTypes)
    {
        if (!areEqual(seen, *lhs.retTypes, *rhs.retTypes))
            return false;
    }

    return true;
}

bool areEqual(SeenSet& seen, const TypeFunctionClassType& lhs, const TypeFunctionClassType& rhs)
{
    if (seenSetContains(seen, &lhs, &rhs))
        return true;

    return lhs.name == rhs.name;
}

bool areEqual(SeenSet& seen, const TypeFunctionType& lhs, const TypeFunctionType& rhs)
{

    if (lhs.type.index() != rhs.type.index())
        return false;

    {
        const TypeFunctionPrimitiveType* lp = get<TypeFunctionPrimitiveType>(&lhs);
        const TypeFunctionPrimitiveType* rp = get<TypeFunctionPrimitiveType>(&rhs);
        if (lp && rp)
            return lp->type == rp->type;
    }

    if (get<TypeFunctionAnyType>(&lhs) && get<TypeFunctionAnyType>(&rhs))
        return true;

    if (get<TypeFunctionUnknownType>(&lhs) && get<TypeFunctionUnknownType>(&rhs))
        return true;

    if (get<TypeFunctionNeverType>(&lhs) && get<TypeFunctionNeverType>(&rhs))
        return true;

    {
        const TypeFunctionSingletonType* lf = get<TypeFunctionSingletonType>(&lhs);
        const TypeFunctionSingletonType* rf = get<TypeFunctionSingletonType>(&rhs);
        if (lf && rf)
            return areEqual(seen, *lf, *rf);
    }

    {
        const TypeFunctionUnionType* lf = get<TypeFunctionUnionType>(&lhs);
        const TypeFunctionUnionType* rf = get<TypeFunctionUnionType>(&rhs);
        if (lf && rf)
            return areEqual(seen, *lf, *rf);
    }

    {
        const TypeFunctionIntersectionType* lf = get<TypeFunctionIntersectionType>(&lhs);
        const TypeFunctionIntersectionType* rf = get<TypeFunctionIntersectionType>(&rhs);
        if (lf && rf)
            return areEqual(seen, *lf, *rf);
    }

    {
        const TypeFunctionNegationType* lf = get<TypeFunctionNegationType>(&lhs);
        const TypeFunctionNegationType* rf = get<TypeFunctionNegationType>(&rhs);
        if (lf && rf)
            return areEqual(seen, *lf, *rf);
    }

    {
        const TypeFunctionTableType* lt = get<TypeFunctionTableType>(&lhs);
        const TypeFunctionTableType* rt = get<TypeFunctionTableType>(&rhs);
        if (lt && rt)
            return areEqual(seen, *lt, *rt);
    }

    {
        const TypeFunctionFunctionType* lf = get<TypeFunctionFunctionType>(&lhs);
        const TypeFunctionFunctionType* rf = get<TypeFunctionFunctionType>(&rhs);
        if (lf && rf)
            return areEqual(seen, *lf, *rf);
    }

    {
        const TypeFunctionClassType* lf = get<TypeFunctionClassType>(&lhs);
        const TypeFunctionClassType* rf = get<TypeFunctionClassType>(&rhs);
        if (lf && rf)
            return areEqual(seen, *lf, *rf);
    }

    return false;
}

bool areEqual(SeenSet& seen, const TypeFunctionTypePack& lhs, const TypeFunctionTypePack& rhs)
{
    if (lhs.head.size() != rhs.head.size())
        return false;

    auto l = lhs.head.begin();
    auto r = rhs.head.begin();

    while (l != lhs.head.end())
    {
        if (!areEqual(seen, **l, **r))
            return false;
        ++l;
        ++r;
    }

    return true;
}

bool areEqual(SeenSet& seen, const TypeFunctionVariadicTypePack& lhs, const TypeFunctionVariadicTypePack& rhs)
{
    if (seenSetContains(seen, &lhs, &rhs))
        return true;

    return areEqual(seen, *lhs.type, *rhs.type);
}

bool areEqual(SeenSet& seen, const TypeFunctionTypePackVar& lhs, const TypeFunctionTypePackVar& rhs)
{
    {
        const TypeFunctionTypePack* lb = get<TypeFunctionTypePack>(&lhs);
        const TypeFunctionTypePack* rb = get<TypeFunctionTypePack>(&rhs);
        if (lb && rb)
            return areEqual(seen, *lb, *rb);
    }

    {
        const TypeFunctionVariadicTypePack* lv = get<TypeFunctionVariadicTypePack>(&lhs);
        const TypeFunctionVariadicTypePack* rv = get<TypeFunctionVariadicTypePack>(&rhs);
        if (lv && rv)
            return areEqual(seen, *lv, *rv);
    }

    return false;
}

bool TypeFunctionType::operator==(const TypeFunctionType& rhs) const
{
    SeenSet seen;
    return areEqual(seen, *this, rhs);
}

bool TypeFunctionTypePackVar::operator==(const TypeFunctionTypePackVar& rhs) const
{
    SeenSet seen;
    return areEqual(seen, *this, rhs);
}


TypeFunctionProperty TypeFunctionProperty::readonly(TypeFunctionTypeId ty)
{
    TypeFunctionProperty p;
    p.readTy = ty;
    return p;
}

TypeFunctionProperty TypeFunctionProperty::writeonly(TypeFunctionTypeId ty)
{
    TypeFunctionProperty p;
    p.writeTy = ty;
    return p;
}

TypeFunctionProperty TypeFunctionProperty::rw(TypeFunctionTypeId ty)
{
    return TypeFunctionProperty::rw(ty, ty);
}

TypeFunctionProperty TypeFunctionProperty::rw(TypeFunctionTypeId read, TypeFunctionTypeId write)
{
    TypeFunctionProperty p;
    p.readTy = read;
    p.writeTy = write;
    return p;
}

bool TypeFunctionProperty::isReadOnly() const
{
    return readTy && !writeTy;
}

bool TypeFunctionProperty::isWriteOnly() const
{
    return writeTy && !readTy;
}

/*
 * Below is a helper class for type.copy()
 * Forked version of Clone.cpp
 */
using TypeFunctionKind = Variant<TypeFunctionTypeId, TypeFunctionTypePackId>;

template<typename T>
const T* get(const TypeFunctionKind& kind)
{
    return get_if<T>(&kind);
}

class TypeFunctionCloner
{
    using SeenTypes = DenseHashMap<TypeFunctionTypeId, TypeFunctionTypeId>;
    using SeenTypePacks = DenseHashMap<TypeFunctionTypePackId, TypeFunctionTypePackId>;

    NotNull<TypeFunctionRuntime> typeFunctionRuntime;

    // A queue of TypeFunctionTypeIds that have been cloned, but whose interior types hasn't
    // been updated to point to itself. Once all of its interior types
    // has been updated, it gets removed from the queue.

    // queue.back() should always return two of same type in their respective sides
    // For example `auto [first, second] = queue.back()`: if first is TypeFunctionPrimitiveType,
    // second must be TypeFunctionPrimitiveType; `second` is trying to copy `first`
    std::vector<std::tuple<TypeFunctionKind, TypeFunctionKind>> queue;

    SeenTypes types{{}};     // Mapping of TypeFunctionTypeIds that have been shallow cloned to TypeFunctionTypeIds
    SeenTypePacks packs{{}}; // Mapping of TypeFunctionTypePackIds that have been shallow cloned to TypeFunctionTypePackIds

    int steps = 0;

public:
    explicit TypeFunctionCloner(TypeFunctionRuntime* typeFunctionRuntime)
        : typeFunctionRuntime(typeFunctionRuntime)
    {
    }

    TypeFunctionTypeId clone(TypeFunctionTypeId ty)
    {
        shallowClone(ty);
        run();

        if (hasExceededIterationLimit())
            return nullptr;

        return find(ty).value_or(nullptr);
    }

    TypeFunctionTypePackId clone(TypeFunctionTypePackId tp)
    {
        shallowClone(tp);
        run();

        if (hasExceededIterationLimit())
            return nullptr;

        return find(tp).value_or(nullptr);
    }

private:
    bool hasExceededIterationLimit() const
    {
        return steps + queue.size() >= (size_t)DFInt::LuauTypeFunctionSerdeIterationLimit;
    }

    void run()
    {
        while (!queue.empty())
        {
            ++steps;

            if (hasExceededIterationLimit())
                break;

            auto [ty, tfti] = queue.back();
            queue.pop_back();

            cloneChildren(ty, tfti);
        }
    }

    std::optional<TypeFunctionTypeId> find(TypeFunctionTypeId ty) const
    {
        if (auto result = types.find(ty))
            return *result;

        return std::nullopt;
    }

    std::optional<TypeFunctionTypePackId> find(TypeFunctionTypePackId tp) const
    {
        if (auto result = packs.find(tp))
            return *result;

        return std::nullopt;
    }

    std::optional<TypeFunctionKind> find(TypeFunctionKind kind) const
    {
        if (auto ty = get<TypeFunctionTypeId>(kind))
            return find(*ty);
        else if (auto tp = get<TypeFunctionTypePackId>(kind))
            return find(*tp);
        else
        {
            LUAU_ASSERT(!"Unknown kind?");
            return std::nullopt;
        }
    }

    TypeFunctionTypeId shallowClone(TypeFunctionTypeId ty)
    {
        if (auto it = find(ty))
            return *it;

        // Create a shallow serialization
        TypeFunctionTypeId target = {};
        if (auto p = get<TypeFunctionPrimitiveType>(ty))
        {
            switch (p->type)
            {
            case TypeFunctionPrimitiveType::NilType:
                target = typeFunctionRuntime->typeArena.allocate(TypeFunctionPrimitiveType(TypeFunctionPrimitiveType::NilType));
                break;
            case TypeFunctionPrimitiveType::Boolean:
                target = typeFunctionRuntime->typeArena.allocate(TypeFunctionPrimitiveType(TypeFunctionPrimitiveType::Boolean));
                break;
            case TypeFunctionPrimitiveType::Number:
                target = typeFunctionRuntime->typeArena.allocate(TypeFunctionPrimitiveType(TypeFunctionPrimitiveType::Number));
                break;
            case TypeFunctionPrimitiveType::String:
                target = typeFunctionRuntime->typeArena.allocate(TypeFunctionPrimitiveType(TypeFunctionPrimitiveType::String));
                break;
            case TypeFunctionPrimitiveType::Thread:
                if (FFlag::LuauUserTypeFunThreadBuffer)
                    target = typeFunctionRuntime->typeArena.allocate(TypeFunctionPrimitiveType(TypeFunctionPrimitiveType::Thread));
                break;
            case TypeFunctionPrimitiveType::Buffer:
                if (FFlag::LuauUserTypeFunThreadBuffer)
                    target = typeFunctionRuntime->typeArena.allocate(TypeFunctionPrimitiveType(TypeFunctionPrimitiveType::Buffer));
                break;
            default:
                break;
            }
        }
        else if (auto u = get<TypeFunctionUnknownType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionUnknownType{});
        else if (auto a = get<TypeFunctionNeverType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionNeverType{});
        else if (auto a = get<TypeFunctionAnyType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionAnyType{});
        else if (auto s = get<TypeFunctionSingletonType>(ty))
        {
            if (auto bs = get<TypeFunctionBooleanSingleton>(s))
                target = typeFunctionRuntime->typeArena.allocate(TypeFunctionSingletonType{TypeFunctionBooleanSingleton{bs->value}});
            else if (auto ss = get<TypeFunctionStringSingleton>(s))
                target = typeFunctionRuntime->typeArena.allocate(TypeFunctionSingletonType{TypeFunctionStringSingleton{ss->value}});
        }
        else if (auto u = get<TypeFunctionUnionType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionUnionType{{}});
        else if (auto i = get<TypeFunctionIntersectionType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionIntersectionType{{}});
        else if (auto n = get<TypeFunctionNegationType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionNegationType{{}});
        else if (auto t = get<TypeFunctionTableType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionTableType{{}, std::nullopt, std::nullopt});
        else if (auto f = get<TypeFunctionFunctionType>(ty))
        {
            TypeFunctionTypePackId emptyTypePack = typeFunctionRuntime->typePackArena.allocate(TypeFunctionTypePack{});
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionFunctionType{emptyTypePack, emptyTypePack});
        }
        else if (auto c = get<TypeFunctionClassType>(ty))
            target = ty; // Don't copy a class since they are immutable

        types[ty] = target;
        queue.emplace_back(ty, target);
        return target;
    }

    TypeFunctionTypePackId shallowClone(TypeFunctionTypePackId tp)
    {
        if (auto it = find(tp))
            return *it;

        // Create a shallow serialization
        TypeFunctionTypePackId target = {};
        if (auto tPack = get<TypeFunctionTypePack>(tp))
            target = typeFunctionRuntime->typePackArena.allocate(TypeFunctionTypePack{{}});
        else if (auto vPack = get<TypeFunctionVariadicTypePack>(tp))
            target = typeFunctionRuntime->typePackArena.allocate(TypeFunctionVariadicTypePack{});

        packs[tp] = target;
        queue.emplace_back(tp, target);
        return target;
    }

    void cloneChildren(TypeFunctionTypeId ty, TypeFunctionTypeId tfti)
    {
        if (auto [p1, p2] = std::tuple{getMutable<TypeFunctionPrimitiveType>(ty), getMutable<TypeFunctionPrimitiveType>(tfti)}; p1 && p2)
            cloneChildren(p1, p2);
        else if (auto [u1, u2] = std::tuple{getMutable<TypeFunctionUnknownType>(ty), getMutable<TypeFunctionUnknownType>(tfti)}; u1 && u2)
            cloneChildren(u1, u2);
        else if (auto [n1, n2] = std::tuple{getMutable<TypeFunctionNeverType>(ty), getMutable<TypeFunctionNeverType>(tfti)}; n1 && n2)
            cloneChildren(n1, n2);
        else if (auto [a1, a2] = std::tuple{getMutable<TypeFunctionAnyType>(ty), getMutable<TypeFunctionAnyType>(tfti)}; a1 && a2)
            cloneChildren(a1, a2);
        else if (auto [s1, s2] = std::tuple{getMutable<TypeFunctionSingletonType>(ty), getMutable<TypeFunctionSingletonType>(tfti)}; s1 && s2)
            cloneChildren(s1, s2);
        else if (auto [u1, u2] = std::tuple{getMutable<TypeFunctionUnionType>(ty), getMutable<TypeFunctionUnionType>(tfti)}; u1 && u2)
            cloneChildren(u1, u2);
        else if (auto [i1, i2] = std::tuple{getMutable<TypeFunctionIntersectionType>(ty), getMutable<TypeFunctionIntersectionType>(tfti)}; i1 && i2)
            cloneChildren(i1, i2);
        else if (auto [n1, n2] = std::tuple{getMutable<TypeFunctionNegationType>(ty), getMutable<TypeFunctionNegationType>(tfti)}; n1 && n2)
            cloneChildren(n1, n2);
        else if (auto [t1, t2] = std::tuple{getMutable<TypeFunctionTableType>(ty), getMutable<TypeFunctionTableType>(tfti)}; t1 && t2)
            cloneChildren(t1, t2);
        else if (auto [f1, f2] = std::tuple{getMutable<TypeFunctionFunctionType>(ty), getMutable<TypeFunctionFunctionType>(tfti)}; f1 && f2)
            cloneChildren(f1, f2);
        else if (auto [c1, c2] = std::tuple{getMutable<TypeFunctionClassType>(ty), getMutable<TypeFunctionClassType>(tfti)}; c1 && c2)
            cloneChildren(c1, c2);
        else
            LUAU_ASSERT(!"Unknown pair?"); // First and argument should always represent the same types
    }

    void cloneChildren(TypeFunctionTypePackId tp, TypeFunctionTypePackId tftp)
    {
        if (auto [tPack1, tPack2] = std::tuple{getMutable<TypeFunctionTypePack>(tp), getMutable<TypeFunctionTypePack>(tftp)}; tPack1 && tPack2)
            cloneChildren(tPack1, tPack2);
        else if (auto [vPack1, vPack2] = std::tuple{getMutable<TypeFunctionVariadicTypePack>(tp), getMutable<TypeFunctionVariadicTypePack>(tftp)};
                 vPack1 && vPack2)
            cloneChildren(vPack1, vPack2);
        else
            LUAU_ASSERT(!"Unknown pair?"); // First and argument should always represent the same types
    }

    void cloneChildren(TypeFunctionKind kind, TypeFunctionKind tfkind)
    {
        if (auto [ty, tfty] = std::tuple{get<TypeFunctionTypeId>(kind), get<TypeFunctionTypeId>(tfkind)}; ty && tfty)
            cloneChildren(*ty, *tfty);
        else if (auto [tp, tftp] = std::tuple{get<TypeFunctionTypePackId>(kind), get<TypeFunctionTypePackId>(tfkind)}; tp && tftp)
            cloneChildren(*tp, *tftp);
        else
            LUAU_ASSERT(!"Unknown pair?"); // First and argument should always represent the same types
    }

    void cloneChildren(TypeFunctionPrimitiveType* p1, TypeFunctionPrimitiveType* p2)
    {
        // noop.
    }

    void cloneChildren(TypeFunctionUnknownType* u1, TypeFunctionUnknownType* u2)
    {
        // noop.
    }

    void cloneChildren(TypeFunctionNeverType* n1, TypeFunctionNeverType* n2)
    {
        // noop.
    }

    void cloneChildren(TypeFunctionAnyType* a1, TypeFunctionAnyType* a2)
    {
        // noop.
    }

    void cloneChildren(TypeFunctionSingletonType* s1, TypeFunctionSingletonType* s2)
    {
        // noop.
    }

    void cloneChildren(TypeFunctionUnionType* u1, TypeFunctionUnionType* u2)
    {
        for (TypeFunctionTypeId& ty : u1->components)
            u2->components.push_back(shallowClone(ty));
    }

    void cloneChildren(TypeFunctionIntersectionType* i1, TypeFunctionIntersectionType* i2)
    {
        for (TypeFunctionTypeId& ty : i1->components)
            i2->components.push_back(shallowClone(ty));
    }

    void cloneChildren(TypeFunctionNegationType* n1, TypeFunctionNegationType* n2)
    {
        n2->type = shallowClone(n1->type);
    }

    void cloneChildren(TypeFunctionTableType* t1, TypeFunctionTableType* t2)
    {
        for (auto& [k, p] : t1->props)
        {
            std::optional<TypeFunctionTypeId> readTy;
            if (p.readTy)
                readTy = shallowClone(*p.readTy);

            std::optional<TypeFunctionTypeId> writeTy;
            if (p.writeTy)
                writeTy = shallowClone(*p.writeTy);

            t2->props[k] = TypeFunctionProperty{readTy, writeTy};
        }

        if (t1->indexer.has_value())
            t2->indexer = TypeFunctionTableIndexer(shallowClone(t1->indexer->keyType), shallowClone(t1->indexer->valueType));

        if (t1->metatable.has_value())
            t2->metatable = shallowClone(*t1->metatable);
    }

    void cloneChildren(TypeFunctionFunctionType* f1, TypeFunctionFunctionType* f2)
    {
        f2->argTypes = shallowClone(f1->argTypes);
        f2->retTypes = shallowClone(f1->retTypes);
    }

    void cloneChildren(TypeFunctionClassType* c1, TypeFunctionClassType* c2)
    {
        // noop.
    }

    void cloneChildren(TypeFunctionTypePack* t1, TypeFunctionTypePack* t2)
    {
        for (TypeFunctionTypeId& ty : t1->head)
            t2->head.push_back(shallowClone(ty));
    }

    void cloneChildren(TypeFunctionVariadicTypePack* v1, TypeFunctionVariadicTypePack* v2)
    {
        v2->type = shallowClone(v1->type);
    }
};

TypeFunctionTypeId deepClone(NotNull<TypeFunctionRuntime> runtime, TypeFunctionTypeId ty)
{
    return TypeFunctionCloner(runtime).clone(ty);
}

} // namespace Luau
