// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/TypeFunctionRuntime.h"

#include "Luau/Allocator.h"
#include "Luau/Lexer.h"
#include "Luau/BuiltinTypeFunctions.h"
#include "Luau/BytecodeBuilder.h"
#include "Luau/ParseResult.h"
#include "Luau/Compiler.h"
#include "Luau/DenseHash.h"
#include "Luau/StringUtils.h"
#include "Luau/TypeFunction.h"
#include "Luau/TypeFunctionRuntimeBuilder.h"

#include "lua.h"
#include "lualib.h"

#include <optional>
#include <set>
#include <vector>

LUAU_DYNAMIC_FASTINT(LuauTypeFunctionSerdeIterationLimit)

namespace Luau
{

LuauTempThreadPopper::LuauTempThreadPopper(lua_State* L)
    : L(L)
{
}

LuauTempThreadPopper::~LuauTempThreadPopper()
{
    lua_pop(L, 1);
}

static void dummyStateClose(lua_State*) {}

TypeFunctionRuntime::TypeFunctionRuntime(NotNull<InternalErrorReporter> ice, NotNull<TypeCheckLimits> limits)
    : ice(ice)
    , limits(limits)
    , state(nullptr, dummyStateClose)
{
}

TypeFunctionRuntime::~TypeFunctionRuntime() {}

std::optional<std::string> TypeFunctionRuntime::registerFunction(AstStatTypeFunction* function)
{
    // If evaluation is disabled, we do not generate additional error messages
    if (!allowEvaluation)
        return std::nullopt;

    // Do not evaluate type functions with parse errors inside
    if (function->hasErrors)
        return std::nullopt;

    prepareState();

    lua_State* global = state.get();

    // Fetch to check if function is already registered
    lua_pushlightuserdata(global, function);
    lua_gettable(global, LUA_REGISTRYINDEX);

    if (!lua_isnil(global, -1))
    {
        lua_pop(global, 1);
        return std::nullopt;
    }

    lua_pop(global, 1);

    AstName name = function->name;

    // Construct ParseResult containing the type function
    Allocator allocator;
    AstNameTable names(allocator);

    AstExpr* exprFunction = function->body;
    AstArray<AstExpr*> exprReturns{&exprFunction, 1};
    AstStatReturn stmtReturn{Location{}, exprReturns};
    AstStat* stmtArray[] = {&stmtReturn};
    AstArray<AstStat*> stmts{stmtArray, 1};
    AstStatBlock exec{Location{}, stmts};
    ParseResult parseResult{&exec, 1, {}, {}, {}, CstNodeMap{nullptr}};

    BytecodeBuilder builder;
    try
    {
        compileOrThrow(builder, parseResult, names);
    }
    catch (CompileError& e)
    {
        return format("'%s' type function failed to compile with error message: %s", name.value, e.what());
    }

    std::string bytecode = builder.getBytecode();

    // Separate sandboxed thread for individual execution and private globals
    lua_State* L = lua_newthread(global);
    LuauTempThreadPopper popper(global);

    // Create individual environment for the type function
    luaL_sandboxthread(L);

    // Do not allow global writes to that environment
    lua_pushvalue(L, LUA_GLOBALSINDEX);
    lua_setreadonly(L, -1, true);
    lua_pop(L, 1);

    // Load bytecode into Luau state
    if (auto error = checkResultForError(L, name.value, luau_load(L, name.value, bytecode.data(), bytecode.size(), 0)))
        return error;

    // Execute the global function which should return our user-defined type function
    if (auto error = checkResultForError(L, name.value, lua_resume(L, nullptr, 0)))
        return error;

    if (!lua_isfunction(L, -1))
    {
        lua_pop(L, 1);
        return format("Could not find '%s' type function in the global scope", name.value);
    }

    // Store resulting function in the registry
    lua_pushlightuserdata(global, function);
    lua_xmove(L, global, 1);
    lua_settable(global, LUA_REGISTRYINDEX);

    return std::nullopt;
}

void TypeFunctionRuntime::prepareState()
{
    if (state)
        return;

    state = StateRef(lua_newstate(typeFunctionAlloc, nullptr), lua_close);
    lua_State* L = state.get();

    lua_setthreaddata(L, this);

    setTypeFunctionEnvironment(L);

    registerTypeUserData(L);

    registerTypesLibrary(L);

    luaL_sandbox(L);
    luaL_sandboxthread(L);
}

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

TypeFunctionRuntime* getTypeFunctionRuntime(lua_State* L)
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
    else if (auto s = get<TypeFunctionPrimitiveType>(ty); s && s->type == TypeFunctionPrimitiveType::Type::Thread)
        return "thread";
    else if (auto s = get<TypeFunctionPrimitiveType>(ty); s && s->type == TypeFunctionPrimitiveType::Type::Buffer)
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
    else if (get<TypeFunctionExternType>(ty))
        return "class";
    else if (get<TypeFunctionGenericType>(ty))
        return "generic";

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

// Luau: `types.generic(name: string, ispack: boolean?) -> type
// Create a generic type with the specified type. If an optinal boolean is set to true, result is a generic pack
static int createGeneric(lua_State* L)
{
    const char* name = luaL_checkstring(L, 1);
    bool isPack = luaL_optboolean(L, 2, false);

    if (strlen(name) == 0)
        luaL_error(L, "types.generic: generic name cannot be empty");

    allocTypeUserData(L, TypeFunctionGenericType{/* isNamed */ true, isPack, name});
    return 1;
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

// Luau: `types.optional(ty: type) -> type`
// Returns the type instance representing an optional version of `ty`.
// If `ty` is a union, this adds `nil` to the components of the union.
// Otherwise, makes a union of the two things.
static int createOptional(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "types.optional: expected 1 argument, but got %d", argumentCount);

    TypeFunctionTypeId argument = getTypeUserData(L, 1);

    std::vector<TypeFunctionTypeId> components;

    if (auto unionTy = get<TypeFunctionUnionType>(argument))
    {
        components.reserve(unionTy->components.size() + 1);

        components.insert(components.begin(), unionTy->components.begin(), unionTy->components.end());
    }
    else
        components.emplace_back(argument);

    components.emplace_back(allocateTypeFunctionType(L, TypeFunctionPrimitiveType(TypeFunctionPrimitiveType::NilType)));

    allocTypeUserData(L, TypeFunctionUnionType{std::move(components)});

    return 1;
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

    allocTypeUserData(L, TypeFunctionUnionType{std::move(components)});

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

    allocTypeUserData(L, TypeFunctionIntersectionType{std::move(components)});

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

    if (auto tfnt = get<TypeFunctionNegationType>(self); tfnt)
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

    allocTypeUserData(L, TypeFunctionTableType{std::move(props), indexer, metatable});
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
    else
        lua_pushnil(L);

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
    else
        lua_pushnil(L);

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

static std::tuple<std::vector<TypeFunctionTypeId>, std::vector<TypeFunctionTypePackId>> getGenerics(lua_State* L, int idx, const char* fname)
{
    std::vector<TypeFunctionTypeId> types;
    std::vector<TypeFunctionTypePackId> packs;

    if (lua_istable(L, idx))
    {
        lua_pushvalue(L, idx);

        for (int i = 1; i <= lua_objlen(L, -1); i++)
        {
            lua_pushinteger(L, i);
            lua_gettable(L, -2);

            if (lua_isnil(L, -1))
            {
                lua_pop(L, 1);
                break;
            }

            TypeFunctionTypeId ty = getTypeUserData(L, -1);

            if (auto gty = get<TypeFunctionGenericType>(ty))
            {
                if (gty->isPack)
                {
                    packs.push_back(allocateTypeFunctionTypePack(L, TypeFunctionGenericTypePack{gty->isNamed, gty->name}));
                }
                else
                {
                    if (!packs.empty())
                        luaL_error(L, "%s: generic type cannot follow a generic pack", fname);

                    types.push_back(ty);
                }
            }
            else
            {
                luaL_error(L, "%s: table member was not a generic type", fname);
            }

            lua_pop(L, 1);
        }

        lua_pop(L, 1);
    }
    else if (!lua_isnoneornil(L, idx))
    {
        luaL_typeerrorL(L, idx, "table");
    }

    return {types, packs};
}

static TypeFunctionTypePackId getTypePack(lua_State* L, int headIdx, int tailIdx)
{
    TypeFunctionTypePackId result = allocateTypeFunctionTypePack(L, TypeFunctionTypePack{});

    std::vector<TypeFunctionTypeId> head;

    if (lua_istable(L, headIdx))
    {
        lua_pushvalue(L, headIdx);

        for (int i = 1; i <= lua_objlen(L, -1); i++)
        {
            lua_pushinteger(L, i);
            lua_gettable(L, -2);

            if (lua_isnil(L, -1))
            {
                lua_pop(L, 1);
                break;
            }

            head.push_back(getTypeUserData(L, -1));
            lua_pop(L, 1);
        }

        lua_pop(L, 1);
    }

    std::optional<TypeFunctionTypePackId> tail;

    if (auto type = optionalTypeUserData(L, tailIdx))
    {
        if (auto gty = get<TypeFunctionGenericType>(*type); gty && gty->isPack)
            tail = allocateTypeFunctionTypePack(L, TypeFunctionGenericTypePack{gty->isNamed, gty->name});
        else
            tail = allocateTypeFunctionTypePack(L, TypeFunctionVariadicTypePack{*type});
    }

    if (head.size() == 0 && tail.has_value())
        result = *tail;
    else
        result = allocateTypeFunctionTypePack(L, TypeFunctionTypePack{std::move(head), tail});

    return result;
}

static void pushTypePack(lua_State* L, TypeFunctionTypePackId tp)
{
    if (auto tftp = get<TypeFunctionTypePack>(tp))
    {
        lua_createtable(L, 0, 2);

        if (!tftp->head.empty())
        {
            lua_createtable(L, int(tftp->head.size()), 0);
            int pos = 1;

            for (auto el : tftp->head)
            {
                allocTypeUserData(L, el->type);
                lua_rawseti(L, -2, pos++);
            }

            lua_setfield(L, -2, "head");
        }

        if (tftp->tail.has_value())
        {
            if (auto tfvp = get<TypeFunctionVariadicTypePack>(*tftp->tail))
                allocTypeUserData(L, tfvp->type->type);
            else if (auto tfgp = get<TypeFunctionGenericTypePack>(*tftp->tail))
                allocTypeUserData(L, TypeFunctionGenericType{tfgp->isNamed, true, tfgp->name});
            else
                luaL_error(L, "unsupported type pack type");

            lua_setfield(L, -2, "tail");
        }
    }
    else if (auto tfvp = get<TypeFunctionVariadicTypePack>(tp))
    {
        lua_createtable(L, 0, 1);

        allocTypeUserData(L, tfvp->type->type);
        lua_setfield(L, -2, "tail");
    }
    else if (auto tfgp = get<TypeFunctionGenericTypePack>(tp))
    {
        lua_createtable(L, 0, 1);

        allocTypeUserData(L, TypeFunctionGenericType{tfgp->isNamed, true, tfgp->name});
        lua_setfield(L, -2, "tail");
    }
    else
    {
        luaL_error(L, "unsupported type pack type");
    }
}

// Luau: `types.newfunction(parameters: {head: {type}?, tail: type?}, returns: {head: {type}?, tail: type?}, generics: {type}?) -> type`
// Returns the type instance representing a function
static int createFunction(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount > 3)
        luaL_error(L, "types.newfunction: expected 0-3 arguments, but got %d", argumentCount);

    TypeFunctionTypePackId argTypes = nullptr;

    if (lua_istable(L, 1))
    {
        lua_getfield(L, 1, "head");
        lua_getfield(L, 1, "tail");

        argTypes = getTypePack(L, -2, -1);

        lua_pop(L, 2);
    }
    else if (!lua_isnoneornil(L, 1))
    {
        luaL_typeerrorL(L, 1, "table");
    }
    else
    {
        argTypes = allocateTypeFunctionTypePack(L, TypeFunctionTypePack{});
    }

    TypeFunctionTypePackId retTypes = nullptr;

    if (lua_istable(L, 2))
    {
        lua_getfield(L, 2, "head");
        lua_getfield(L, 2, "tail");

        retTypes = getTypePack(L, -2, -1);

        lua_pop(L, 2);
    }
    else if (!lua_isnoneornil(L, 2))
    {
        luaL_typeerrorL(L, 2, "table");
    }
    else
    {
        retTypes = allocateTypeFunctionTypePack(L, TypeFunctionTypePack{});
    }

    auto [genericTypes, genericPacks] = getGenerics(L, 3, "types.newfunction");

    allocTypeUserData(L, TypeFunctionFunctionType{std::move(genericTypes), std::move(genericPacks), argTypes, retTypes});

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

    tfft->argTypes = getTypePack(L, 2, 3);

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

    pushTypePack(L, tfft->argTypes);

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

    tfft->retTypes = getTypePack(L, 2, 3);

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

    pushTypePack(L, tfft->retTypes);

    return 1;
}

// Luau: `self:setgenerics(generics: {type}?)`
static int setFunctionGenerics(lua_State* L)
{
    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tfft = getMutable<TypeFunctionFunctionType>(self);
    if (!tfft)
        luaL_error(L, "type.setgenerics: expected self to be a function, but got %s instead", getTag(L, self).c_str());

    int argumentCount = lua_gettop(L);
    if (argumentCount > 3)
        luaL_error(L, "type.setgenerics: expected 3 arguments, but got %d", argumentCount);

    auto [genericTypes, genericPacks] = getGenerics(L, 2, "types.setgenerics");

    tfft->generics = std::move(genericTypes);
    tfft->genericPacks = std::move(genericPacks);

    return 0;
}

// Luau: `self:generics() -> {type}`
static int getFunctionGenerics(lua_State* L)
{
    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tfft = get<TypeFunctionFunctionType>(self);
    if (!tfft)
        luaL_error(L, "type.generics: expected self to be a function, but got %s instead", getTag(L, self).c_str());

    lua_createtable(L, int(tfft->generics.size()) + int(tfft->genericPacks.size()), 0);

    int pos = 1;

    for (const auto& el : tfft->generics)
    {
        allocTypeUserData(L, el->type);
        lua_rawseti(L, -2, pos++);
    }

    for (const auto& el : tfft->genericPacks)
    {
        auto gty = get<TypeFunctionGenericTypePack>(el);
        LUAU_ASSERT(gty);
        allocTypeUserData(L, TypeFunctionGenericType{gty->isNamed, true, gty->name});
        lua_rawseti(L, -2, pos++);
    }

    return 1;
}

// Luau: `self:readparent() -> type`
// Returns the read type of the class' parent
static int getReadParent(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "type.parent: expected 1 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tfct = get<TypeFunctionExternType>(self);
    if (!tfct)
        luaL_error(L, "type.parent: expected self to be a class, but got %s instead", getTag(L, self).c_str());

    // If the parent does not exist, we should return nil
    if (!tfct->readParent)
        lua_pushnil(L);
    else
        allocTypeUserData(L, (*tfct->readParent)->type);

    return 1;
}
//
// Luau: `self:writeparent() -> type`
// Returns the write type of the class' parent
static int getWriteParent(lua_State* L)
{
    int argumentCount = lua_gettop(L);
    if (argumentCount != 1)
        luaL_error(L, "type.parent: expected 1 arguments, but got %d", argumentCount);

    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tfct = get<TypeFunctionExternType>(self);
    if (!tfct)
        luaL_error(L, "type.parent: expected self to be a class, but got %s instead", getTag(L, self).c_str());

    // If the parent does not exist, we should return nil
    if (!tfct->writeParent)
        lua_pushnil(L);
    else
        allocTypeUserData(L, (*tfct->writeParent)->type);

    return 1;
}

// Luau: `self:name() -> string?`
// Returns the name of the generic or 'nil' if the generic is unnamed
static int getGenericName(lua_State* L)
{
    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tfgt = get<TypeFunctionGenericType>(self);
    if (!tfgt)
        luaL_error(L, "type.name: expected self to be a generic, but got %s instead", getTag(L, self).c_str());

    if (tfgt->isNamed)
        lua_pushstring(L, tfgt->name.c_str());
    else
        lua_pushnil(L);

    return 1;
}

// Luau: `self:ispack() -> boolean`
// Returns true if the generic is a pack
static int getGenericIsPack(lua_State* L)
{
    TypeFunctionTypeId self = getTypeUserData(L, 1);
    auto tfgt = get<TypeFunctionGenericType>(self);
    if (!tfgt)
        luaL_error(L, "type.ispack: expected self to be a generic, but got %s instead", getTag(L, self).c_str());

    lua_pushboolean(L, tfgt->isPack);
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

    if (auto tfct = get<TypeFunctionExternType>(self))
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

    if (auto tfct = get<TypeFunctionExternType>(self))
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

    if (auto tfct = get<TypeFunctionExternType>(self))
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

    if (auto tfct = get<TypeFunctionExternType>(self))
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

    if (auto tfct = get<TypeFunctionExternType>(self))
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

// Luau: `types.copy(arg: type) -> type`
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
    luaL_Reg fields[] = {
        {"unknown", createUnknown},
        {"never", createNever},
        {"any", createAny},
        {"boolean", createBoolean},
        {"number", createNumber},
        {"string", createString},
        {"thread", createThread},
        {"buffer", createBuffer},
        {nullptr, nullptr}
    };

    luaL_Reg methods[] = {
        {"singleton", createSingleton},
        {"negationof", createNegation},
        {"unionof", createUnion},
        {"intersectionof", createIntersection},
        {"optional", createOptional},
        {"newtable", createTable},
        {"newfunction", createFunction},
        {"copy", deepCopy},
        {"generic", createGeneric},

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
        {"setgenerics", setFunctionGenerics},
        {"generics", getFunctionGenerics},

        // Union and Intersection type methods
        {"components", getComponents},

        //  Extern type methods
        {"readparent", getReadParent},
        {"writeparent", getWriteParent},

        // Function type methods (cont.)
        {"setgenerics", setFunctionGenerics},
        {"generics", getFunctionGenerics},

        // Generic type methods
        {"name", getGenericName},
        {"ispack", getGenericIsPack},

        {nullptr, nullptr}
    };

    // Create and register metatable for type userdata
    luaL_newmetatable(L, "type");

    lua_pushstring(L, "type");
    lua_setfield(L, -2, "__type");

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

    // Sets up a destructor for the type userdata.
    lua_setuserdatadtor(L, kTypeUserdataTag, deallocTypeUserData);
}

// Used to redirect all the removed global functions to say "this function is unsupported"
static int unsupportedFunction(lua_State* L)
{
    luaL_errorL(L, "this function is not supported in type functions");
    return 0;
}

static int print(lua_State* L)
{
    std::string result;

    int n = lua_gettop(L);
    for (int i = 1; i <= n; i++)
    {
        size_t l = 0;
        const char* s = luaL_tolstring(L, i, &l); // convert to string using __tostring et al
        if (i > 1)
        {
            result.append(1, '\t');
        }
        result.append(s, l);
        lua_pop(L, 1);
    }

    auto ctx = getTypeFunctionRuntime(L);

    ctx->messages.push_back(std::move(result));

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
    static const char* unavailableGlobals[] = {"gcinfo", "getfenv", "newproxy", "setfenv", "pcall", "xpcall"};
    for (auto& name : unavailableGlobals)
    {
        lua_pushcfunction(L, unsupportedFunction, name);
        lua_setglobal(L, name);
    }

    lua_pushcfunction(L, print, "print");
    lua_setglobal(L, "print");
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
        const TypeFunctionBooleanSingleton* rp = get<TypeFunctionBooleanSingleton>(&rhs);
        if (lp && rp)
            return lp->value == rp->value;
    }

    {
        const TypeFunctionStringSingleton* lp = get<TypeFunctionStringSingleton>(&lhs);
        const TypeFunctionStringSingleton* rp = get<TypeFunctionStringSingleton>(&rhs);
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

    if (lhs.generics.size() != rhs.generics.size())
        return false;

    for (auto l = lhs.generics.begin(), r = rhs.generics.begin(); l != lhs.generics.end() && r != rhs.generics.end(); ++l, ++r)
    {
        if (!areEqual(seen, **l, **r))
            return false;
    }

    if (lhs.genericPacks.size() != rhs.genericPacks.size())
        return false;

    for (auto l = lhs.genericPacks.begin(), r = rhs.genericPacks.begin(); l != lhs.genericPacks.end() && r != rhs.genericPacks.end(); ++l, ++r)
    {
        if (!areEqual(seen, **l, **r))
            return false;
    }

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

bool areEqual(SeenSet& seen, const TypeFunctionExternType& lhs, const TypeFunctionExternType& rhs)
{
    if (seenSetContains(seen, &lhs, &rhs))
        return true;

    return lhs.externTy == rhs.externTy;
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
        const TypeFunctionExternType* lf = get<TypeFunctionExternType>(&lhs);
        const TypeFunctionExternType* rf = get<TypeFunctionExternType>(&rhs);
        if (lf && rf)
            return areEqual(seen, *lf, *rf);
    }

    {
        const TypeFunctionGenericType* lg = get<TypeFunctionGenericType>(&lhs);
        const TypeFunctionGenericType* rg = get<TypeFunctionGenericType>(&rhs);
        if (lg && rg)
            return lg->isNamed == rg->isNamed && lg->isPack == rg->isPack && lg->name == rg->name;
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

    {
        const TypeFunctionGenericTypePack* lg = get<TypeFunctionGenericTypePack>(&lhs);
        const TypeFunctionGenericTypePack* rg = get<TypeFunctionGenericTypePack>(&rhs);
        if (lg && rg)
            return lg->isNamed == rg->isNamed && lg->name == rg->name;
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
                target = typeFunctionRuntime->typeArena.allocate(TypeFunctionPrimitiveType(TypeFunctionPrimitiveType::Thread));
                break;
            case TypeFunctionPrimitiveType::Buffer:
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
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionFunctionType{{}, {}, emptyTypePack, emptyTypePack});
        }
        else if (auto c = get<TypeFunctionExternType>(ty))
            target = ty; // Don't copy a class since they are immutable
        else if (auto g = get<TypeFunctionGenericType>(ty))
            target = typeFunctionRuntime->typeArena.allocate(TypeFunctionGenericType{g->isNamed, g->isPack, g->name});
        else
            LUAU_ASSERT(!"Unknown type");

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
        else if (auto gPack = get<TypeFunctionGenericTypePack>(tp))
            target = typeFunctionRuntime->typePackArena.allocate(TypeFunctionGenericTypePack{gPack->isNamed, gPack->name});
        else
            LUAU_ASSERT(!"Unknown type");

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
        else if (auto [c1, c2] = std::tuple{getMutable<TypeFunctionExternType>(ty), getMutable<TypeFunctionExternType>(tfti)}; c1 && c2)
            cloneChildren(c1, c2);
        else if (auto [g1, g2] = std::tuple{getMutable<TypeFunctionGenericType>(ty), getMutable<TypeFunctionGenericType>(tfti)}; g1 && g2)
            cloneChildren(g1, g2);
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
        else if (auto [gPack1, gPack2] = std::tuple{getMutable<TypeFunctionGenericTypePack>(tp), getMutable<TypeFunctionGenericTypePack>(tftp)};
                 gPack1 && gPack2)
            cloneChildren(gPack1, gPack2);
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
        f2->generics.reserve(f1->generics.size());
        for (auto ty : f1->generics)
            f2->generics.push_back(shallowClone(ty));

        f2->genericPacks.reserve(f1->genericPacks.size());
        for (auto tp : f1->genericPacks)
            f2->genericPacks.push_back(shallowClone(tp));

        f2->argTypes = shallowClone(f1->argTypes);
        f2->retTypes = shallowClone(f1->retTypes);
    }

    void cloneChildren(TypeFunctionExternType* c1, TypeFunctionExternType* c2)
    {
        // noop.
    }

    void cloneChildren(TypeFunctionGenericType* g1, TypeFunctionGenericType* g2)
    {
        // noop.
    }

    void cloneChildren(TypeFunctionTypePack* t1, TypeFunctionTypePack* t2)
    {
        for (TypeFunctionTypeId& ty : t1->head)
            t2->head.push_back(shallowClone(ty));

        if (t1->tail)
            t2->tail = shallowClone(*t1->tail);
    }

    void cloneChildren(TypeFunctionVariadicTypePack* v1, TypeFunctionVariadicTypePack* v2)
    {
        v2->type = shallowClone(v1->type);
    }

    void cloneChildren(TypeFunctionGenericTypePack* g1, TypeFunctionGenericTypePack* g2)
    {
        // noop.
    }
};

TypeFunctionTypeId deepClone(NotNull<TypeFunctionRuntime> runtime, TypeFunctionTypeId ty)
{
    return TypeFunctionCloner(runtime).clone(ty);
}

} // namespace Luau
