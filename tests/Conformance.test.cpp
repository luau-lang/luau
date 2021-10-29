// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Compiler.h"

#include "Luau/BuiltinDefinitions.h"
#include "Luau/ModuleResolver.h"
#include "Luau/TypeInfer.h"
#include "Luau/StringUtils.h"
#include "Luau/BytecodeBuilder.h"

#include "doctest.h"
#include "ScopedFlags.h"

#include "lua.h"
#include "lualib.h"

#include <fstream>
#include <math.h>

static int lua_collectgarbage(lua_State* L)
{
    static const char* const opts[] = {"stop", "restart", "collect", "count", "isrunning", "step", "setgoal", "setstepmul", "setstepsize", nullptr};
    static const int optsnum[] = {
        LUA_GCSTOP, LUA_GCRESTART, LUA_GCCOLLECT, LUA_GCCOUNT, LUA_GCISRUNNING, LUA_GCSTEP, LUA_GCSETGOAL, LUA_GCSETSTEPMUL, LUA_GCSETSTEPSIZE};

    int o = luaL_checkoption(L, 1, "collect", opts);
    int ex = luaL_optinteger(L, 2, 0);
    int res = lua_gc(L, optsnum[o], ex);
    switch (optsnum[o])
    {
    case LUA_GCSTEP:
    case LUA_GCISRUNNING:
    {
        lua_pushboolean(L, res);
        return 1;
    }
    default:
    {
        lua_pushnumber(L, res);
        return 1;
    }
    }
}

static int lua_loadstring(lua_State* L)
{
    size_t l = 0;
    const char* s = luaL_checklstring(L, 1, &l);
    const char* chunkname = luaL_optstring(L, 2, s);

    lua_setsafeenv(L, LUA_ENVIRONINDEX, false);

    std::string bytecode = Luau::compile(std::string(s, l));
    if (luau_load(L, chunkname, bytecode.data(), bytecode.size()) == 0)
        return 1;

    lua_pushnil(L);
    lua_insert(L, -2); /* put before error message */
    return 2;          /* return nil plus error message */
}

static int lua_vector(lua_State* L)
{
    double x = luaL_checknumber(L, 1);
    double y = luaL_checknumber(L, 2);
    double z = luaL_checknumber(L, 3);

    lua_pushvector(L, float(x), float(y), float(z));
    return 1;
}

static int lua_vector_dot(lua_State* L)
{
    const float* a = lua_tovector(L, 1);
    const float* b = lua_tovector(L, 2);

    if (a && b)
    {
        lua_pushnumber(L, a[0] * b[0] + a[1] * b[1] + a[2] * b[2]);
        return 1;
    }

    throw std::runtime_error("invalid arguments to vector:Dot");
}

static int lua_vector_index(lua_State* L)
{
    const char* name = luaL_checkstring(L, 2);

    if (const float* v = lua_tovector(L, 1))
    {
        if (strcmp(name, "Magnitude") == 0)
        {
            lua_pushnumber(L, sqrtf(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]));
            return 1;
        }

        if (strcmp(name, "Dot") == 0)
        {
            lua_pushcfunction(L, lua_vector_dot, "Dot");
            return 1;
        }
    }

    throw std::runtime_error(Luau::format("%s is not a valid member of vector", name));
}

static int lua_vector_namecall(lua_State* L)
{
    if (const char* str = lua_namecallatom(L, nullptr))
    {
        if (strcmp(str, "Dot") == 0)
            return lua_vector_dot(L);
    }

    throw std::runtime_error(Luau::format("%s is not a valid method of vector", luaL_checkstring(L, 1)));
}

int lua_silence(lua_State* L)
{
    return 0;
}

using StateRef = std::unique_ptr<lua_State, void (*)(lua_State*)>;

static StateRef runConformance(
    const char* name, void (*setup)(lua_State* L) = nullptr, void (*yield)(lua_State* L) = nullptr, lua_State* initialLuaState = nullptr)
{
    std::string path = __FILE__;
    path.erase(path.find_last_of("\\/"));
    path += "/conformance/";
    path += name;

    std::fstream stream(path, std::ios::in | std::ios::binary);
    REQUIRE(stream);

    std::string source(std::istreambuf_iterator<char>(stream), {});

    stream.close();

    if (!initialLuaState)
        initialLuaState = luaL_newstate();
    StateRef globalState(initialLuaState, lua_close);
    lua_State* L = globalState.get();

    luaL_openlibs(L);

    // Register a few global functions for conformance tests
    static const luaL_Reg funcs[] = {
        {"collectgarbage", lua_collectgarbage},
        {"loadstring", lua_loadstring},
        {"print", lua_silence}, // Disable print() by default; comment this out to enable debug prints in tests
        {nullptr, nullptr},
    };

    lua_pushvalue(L, LUA_GLOBALSINDEX);
    luaL_register(L, nullptr, funcs);
    lua_pop(L, 1);

    // In some configurations we have a larger C stack consumption which trips some conformance tests
#if defined(LUAU_ENABLE_ASAN) || defined(_NOOPT) || defined(_DEBUG)
    lua_pushboolean(L, true);
    lua_setglobal(L, "limitedstack");
#endif

    // Extra test-specific setup
    if (setup)
        setup(L);

    // Protect core libraries and metatables from modification
    luaL_sandbox(L);

    // Create a new writable global table for current thread
    luaL_sandboxthread(L);

    // Lua conformance tests treat _G synonymously with getfenv(); for now cater to them
    lua_pushvalue(L, LUA_GLOBALSINDEX);
    lua_pushvalue(L, LUA_GLOBALSINDEX);
    lua_setfield(L, -1, "_G");

    std::string chunkname = "=" + std::string(name);

    Luau::CompileOptions copts;
    copts.debugLevel = 2;        // for debugger tests
    copts.vectorCtor = "vector"; // for vector tests

    std::string bytecode = Luau::compile(source, copts);
    int status = 0;

    if (luau_load(L, chunkname.c_str(), bytecode.data(), bytecode.size()) == 0)
    {
        status = lua_resume(L, nullptr, 0);
    }
    else
    {
        status = LUA_ERRSYNTAX;
    }

    while (yield && (status == LUA_YIELD || status == LUA_BREAK))
    {
        yield(L);
        status = lua_resume(L, nullptr, 0);
    }

    extern void luaC_validate(lua_State * L); // internal function, declared in lgc.h - not exposed via lua.h
    luaC_validate(L);

    if (status == 0)
    {
        REQUIRE(lua_isstring(L, -1));
        CHECK(std::string(lua_tostring(L, -1)) == "OK");
    }
    else
    {
        std::string error = (status == LUA_YIELD) ? "thread yielded unexpectedly" : lua_tostring(L, -1);
        error += "\nstacktrace:\n";
        error += lua_debugtrace(L);

        FAIL(error);
    }

    return globalState;
}

TEST_SUITE_BEGIN("Conformance");

TEST_CASE("Assert")
{
    runConformance("assert.lua");
}

TEST_CASE("Basic")
{
    runConformance("basic.lua");
}

TEST_CASE("Math")
{
    runConformance("math.lua");
}

TEST_CASE("Table")
{
    ScopedFastFlag sff("LuauTableFreeze", true);

    runConformance("nextvar.lua");
}

TEST_CASE("PatternMatch")
{
    runConformance("pm.lua");
}

TEST_CASE("Sort")
{
    runConformance("sort.lua");
}

TEST_CASE("Move")
{
    runConformance("move.lua");
}

TEST_CASE("Clear")
{
    runConformance("clear.lua");
}

TEST_CASE("Strings")
{
    runConformance("strings.lua");
}

TEST_CASE("VarArg")
{
    runConformance("vararg.lua");
}

TEST_CASE("Locals")
{
    runConformance("locals.lua");
}

TEST_CASE("Literals")
{
    runConformance("literals.lua");
}

TEST_CASE("Errors")
{
    runConformance("errors.lua");
}

TEST_CASE("Events")
{
    runConformance("events.lua");
}

TEST_CASE("Constructs")
{
    runConformance("constructs.lua");
}

TEST_CASE("Closure")
{
    runConformance("closure.lua");
}

TEST_CASE("Calls")
{
    runConformance("calls.lua");
}

TEST_CASE("Attrib")
{
    runConformance("attrib.lua");
}

TEST_CASE("GC")
{
    runConformance("gc.lua");
}

TEST_CASE("Bitwise")
{
    runConformance("bitwise.lua");
}

TEST_CASE("UTF8")
{
    runConformance("utf8.lua");
}

TEST_CASE("Coroutine")
{
    runConformance("coroutine.lua");
}

TEST_CASE("PCall")
{
    runConformance("pcall.lua", [](lua_State* L) {
        lua_pushcfunction(L, [](lua_State* L) -> int {
#if LUA_USE_LONGJMP
            luaL_error(L, "oops");
#else
            throw std::runtime_error("oops");
#endif
        });
        lua_setglobal(L, "cxxthrow");

        lua_pushcfunction(L, [](lua_State* L) -> int {
            lua_State* co = lua_tothread(L, 1);
            lua_xmove(L, co, 1);
            lua_resumeerror(co, L);
            return 0;
        });
        lua_setglobal(L, "resumeerror");
    });
}

TEST_CASE("Pack")
{
    runConformance("tpack.lua");
}

TEST_CASE("Vector")
{
    runConformance("vector.lua", [](lua_State* L) {
        lua_pushcfunction(L, lua_vector);
        lua_setglobal(L, "vector");

        lua_pushvector(L, 0.0f, 0.0f, 0.0f);
        luaL_newmetatable(L, "vector");

        lua_pushstring(L, "__index");
        lua_pushcfunction(L, lua_vector_index);
        lua_settable(L, -3);

        lua_pushstring(L, "__namecall");
        lua_pushcfunction(L, lua_vector_namecall);
        lua_settable(L, -3);

        lua_setreadonly(L, -1, true);
        lua_setmetatable(L, -2);
        lua_pop(L, 1);
    });
}

static void populateRTTI(lua_State* L, Luau::TypeId type)
{
    if (auto p = Luau::get<Luau::PrimitiveTypeVar>(type))
    {
        switch (p->type)
        {
        case Luau::PrimitiveTypeVar::Boolean:
            lua_pushstring(L, "boolean");
            break;

        case Luau::PrimitiveTypeVar::NilType:
            lua_pushstring(L, "nil");
            break;

        case Luau::PrimitiveTypeVar::Number:
            lua_pushstring(L, "number");
            break;

        case Luau::PrimitiveTypeVar::String:
            lua_pushstring(L, "string");
            break;

        case Luau::PrimitiveTypeVar::Thread:
            lua_pushstring(L, "thread");
            break;

        default:
            LUAU_ASSERT(!"Unknown primitive type");
        }
    }
    else if (auto t = Luau::get<Luau::TableTypeVar>(type))
    {
        lua_newtable(L);

        for (const auto& [name, prop] : t->props)
        {
            populateRTTI(L, prop.type);
            lua_setfield(L, -2, name.c_str());
        }
    }
    else if (Luau::get<Luau::FunctionTypeVar>(type))
    {
        lua_pushstring(L, "function");
    }
    else if (Luau::get<Luau::AnyTypeVar>(type))
    {
        lua_pushstring(L, "any");
    }
    else if (auto i = Luau::get<Luau::IntersectionTypeVar>(type))
    {
        for (const auto& part : i->parts)
            LUAU_ASSERT(Luau::get<Luau::FunctionTypeVar>(part));

        lua_pushstring(L, "function");
    }
    else
    {
        LUAU_ASSERT(!"Unknown type");
    }
}

TEST_CASE("Types")
{
    runConformance("types.lua", [](lua_State* L) {
        Luau::NullModuleResolver moduleResolver;
        Luau::InternalErrorReporter iceHandler;
        Luau::TypeChecker env(&moduleResolver, &iceHandler);

        Luau::registerBuiltinTypes(env);
        Luau::freeze(env.globalTypes);

        lua_newtable(L);

        for (const auto& [name, binding] : env.globalScope->bindings)
        {
            populateRTTI(L, binding.typeId);
            lua_setfield(L, -2, toString(name).c_str());
        }

        lua_setglobal(L, "RTTI");
    });
}

TEST_CASE("DateTime")
{
    runConformance("datetime.lua");
}

TEST_CASE("Debug")
{
    runConformance("debug.lua");
}

TEST_CASE("Debugger")
{
    static int breakhits = 0;
    static lua_State* interruptedthread = nullptr;

    breakhits = 0;
    interruptedthread = nullptr;

    runConformance(
        "debugger.lua",
        [](lua_State* L) {
            lua_Callbacks* cb = lua_callbacks(L);

            // for breakpoints to work we should make sure debugbreak is installed
            cb->debugbreak = [](lua_State* L, lua_Debug* ar) {
                breakhits++;

                // for every breakpoint, we break on the first invocation and continue on second
                // this allows us to easily step off breakpoints
                // (real implementaiton may require singlestepping)
                if (breakhits % 2 == 1)
                    lua_break(L);
            };

            // for resuming off a breakpoint inside a coroutine we need to resume the interrupted coroutine
            cb->debuginterrupt = [](lua_State* L, lua_Debug* ar) {
                CHECK(interruptedthread == nullptr);
                CHECK(ar->userdata); // userdata contains the interrupted thread

                interruptedthread = static_cast<lua_State*>(ar->userdata);
            };

            // add breakpoint() function
            lua_pushcfunction(L, [](lua_State* L) -> int {
                int line = luaL_checkinteger(L, 1);

                lua_Debug ar = {};
                lua_getinfo(L, 1, "f", &ar);

                lua_breakpoint(L, -1, line, true);
                return 0;
            });
            lua_setglobal(L, "breakpoint");
        },
        [](lua_State* L) {
            CHECK(breakhits % 2 == 1);

            lua_checkstack(L, LUA_MINSTACK);

            if (breakhits == 1)
            {
                // test lua_getargument
                int a = lua_getargument(L, 0, 1);
                REQUIRE(a);
                CHECK(lua_tointeger(L, -1) == 50);
                lua_pop(L, 1);

                // test lua_getlocal
                const char* l = lua_getlocal(L, 0, 1);
                REQUIRE(l);
                CHECK(strcmp(l, "b") == 0);
                CHECK(lua_tointeger(L, -1) == 50);
                lua_pop(L, 1);

                // test lua_getupvalue
                lua_Debug ar = {};
                lua_getinfo(L, 0, "f", &ar);

                const char* u = lua_getupvalue(L, -1, 1);
                REQUIRE(u);
                CHECK(strcmp(u, "a") == 0);
                CHECK(lua_tointeger(L, -1) == 5);
                lua_pop(L, 2);
            }
            else if (breakhits == 3)
            {
                // validate assignment via lua_getlocal
                const char* l = lua_getlocal(L, 0, 1);
                REQUIRE(l);
                CHECK(strcmp(l, "a") == 0);
                CHECK(lua_tointeger(L, -1) == 6);
                lua_pop(L, 1);
            }
            else if (breakhits == 5)
            {
                // validate assignment via lua_getlocal
                const char* l = lua_getlocal(L, 1, 1);
                REQUIRE(l);
                CHECK(strcmp(l, "a") == 0);
                CHECK(lua_tointeger(L, -1) == 7);
                lua_pop(L, 1);
            }
            else if (breakhits == 7)
            {
                // validate assignment via lua_getlocal
                const char* l = lua_getlocal(L, 1, 1);
                REQUIRE(l);
                CHECK(strcmp(l, "a") == 0);
                CHECK(lua_tointeger(L, -1) == 8);
                lua_pop(L, 1);
            }
            else if (breakhits == 9)
            {
                // validate assignment via lua_getlocal
                const char* l = lua_getlocal(L, 1, 1);
                REQUIRE(l);
                CHECK(strcmp(l, "a") == 0);
                CHECK(lua_tointeger(L, -1) == 9);
                lua_pop(L, 1);
            }

            if (interruptedthread)
            {
                lua_resume(interruptedthread, nullptr, 0);
                interruptedthread = nullptr;
            }
        });

    CHECK(breakhits == 10); // 2 hits per breakpoint
}

TEST_CASE("SameHash")
{
    extern unsigned int luaS_hash(const char* str, size_t len); // internal function, declared in lstring.h - not exposed via lua.h

    // To keep VM and compiler separate, we duplicate the hash function definition
    // This test validates that the hash function in question returns the same results on basic inputs
    // If this is violated, some code may regress in performance due to hash slot misprediction in inline caches
    CHECK(luaS_hash("", 0) == Luau::BytecodeBuilder::getStringHash({"", 0}));
    CHECK(luaS_hash("lua", 3) == Luau::BytecodeBuilder::getStringHash({"lua", 3}));
    CHECK(luaS_hash("luau", 4) == Luau::BytecodeBuilder::getStringHash({"luau", 4}));
    CHECK(luaS_hash("luaubytecode", 12) == Luau::BytecodeBuilder::getStringHash({"luaubytecode", 12}));
    CHECK(luaS_hash("luaubytecodehash", 16) == Luau::BytecodeBuilder::getStringHash({"luaubytecodehash", 16}));

    // Also hash should work on unaligned source data even when hashing long strings
    char buf[128] = {};
    CHECK(luaS_hash(buf + 1, 120) == luaS_hash(buf + 2, 120));
}

TEST_CASE("InlineDtor")
{
    static int dtorhits = 0;

    dtorhits = 0;

    {
        StateRef globalState(luaL_newstate(), lua_close);
        lua_State* L = globalState.get();

        void* u1 = lua_newuserdatadtor(L, 4, [](void* data) {
            dtorhits += *(int*)data;
        });

        void* u2 = lua_newuserdatadtor(L, 1, [](void* data) {
            dtorhits += *(char*)data;
        });

        *(int*)u1 = 39;
        *(char*)u2 = 3;
    }

    CHECK(dtorhits == 42);
}

TEST_CASE("Reference")
{
    static int dtorhits = 0;

    dtorhits = 0;

    StateRef globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    // note, we push two userdata objects but only pin one of them (the first one)
    lua_newuserdatadtor(L, 0, [](void*) {
        dtorhits++;
    });
    lua_newuserdatadtor(L, 0, [](void*) {
        dtorhits++;
    });

    lua_gc(L, LUA_GCCOLLECT, 0);
    CHECK(dtorhits == 0);

    int ref = lua_ref(L, -2);
    lua_pop(L, 2);

    lua_gc(L, LUA_GCCOLLECT, 0);
    CHECK(dtorhits == 1);

    lua_getref(L, ref);
    CHECK(lua_isuserdata(L, -1));
    lua_pop(L, 1);

    lua_gc(L, LUA_GCCOLLECT, 0);
    CHECK(dtorhits == 1);

    lua_unref(L, ref);

    lua_gc(L, LUA_GCCOLLECT, 0);
    CHECK(dtorhits == 2);
}

TEST_CASE("ApiFunctionCalls")
{
    StateRef globalState = runConformance("apicalls.lua");
    lua_State* L = globalState.get();

    lua_getfield(L, LUA_GLOBALSINDEX, "add");
    lua_pushnumber(L, 40);
    lua_pushnumber(L, 2);
    lua_call(L, 2, 1);
    CHECK(lua_isnumber(L, -1));
    CHECK(lua_tonumber(L, -1) == 42);
    lua_pop(L, 1);

    lua_getfield(L, LUA_GLOBALSINDEX, "add");
    lua_pushnumber(L, 40);
    lua_pushnumber(L, 2);
    lua_pcall(L, 2, 1, 0);
    CHECK(lua_isnumber(L, -1));
    CHECK(lua_tonumber(L, -1) == 42);
    lua_pop(L, 1);
}

static bool endsWith(const std::string& str, const std::string& suffix)
{
    if (suffix.length() > str.length())
        return false;

    return suffix == std::string_view(str.c_str() + str.length() - suffix.length(), suffix.length());
}

#if !LUA_USE_LONGJMP
TEST_CASE("ExceptionObject")
{
    ScopedFastFlag sff("LuauExceptionMessageFix", true);

    struct ExceptionResult
    {
        bool exceptionGenerated;
        std::string description;
    };

    auto captureException = [](lua_State* L, const char* functionToRun) {
        try
        {
            lua_State* threadState = lua_newthread(L);
            lua_getfield(threadState, LUA_GLOBALSINDEX, functionToRun);
            CHECK(lua_isLfunction(threadState, -1));
            lua_call(threadState, 0, 0);
        }
        catch (std::exception& e)
        {
            CHECK(e.what() != nullptr);
            return ExceptionResult{true, e.what()};
        }
        return ExceptionResult{false, ""};
    };

    auto reallocFunc = [](lua_State* L, void* /*ud*/, void* ptr, size_t /*osize*/, size_t nsize) -> void* {
        if (nsize == 0)
        {
            free(ptr);
            return NULL;
        }
        else if (nsize > 512 * 1024)
        {
            // For testing purposes return null for large allocations
            // so we can generate exceptions related to memory allocation
            // failures.
            return nullptr;
        }
        else
        {
            return realloc(ptr, nsize);
        }
    };

    StateRef globalState = runConformance("exceptions.lua", nullptr, nullptr, lua_newstate(reallocFunc, nullptr));
    lua_State* L = globalState.get();

    {
        ExceptionResult result = captureException(L, "infinite_recursion_error");
        CHECK(result.exceptionGenerated);
    }

    {
        ExceptionResult result = captureException(L, "empty_function");
        CHECK_FALSE(result.exceptionGenerated);
    }

    {
        ExceptionResult result = captureException(L, "pass_number_to_error");
        CHECK(result.exceptionGenerated);
        CHECK(endsWith(result.description, "42"));
    }

    {
        ExceptionResult result = captureException(L, "pass_string_to_error");
        CHECK(result.exceptionGenerated);
        CHECK(endsWith(result.description, "string argument"));
    }

    {
        ExceptionResult result = captureException(L, "pass_table_to_error");
        CHECK(result.exceptionGenerated);
    }

    {
        ExceptionResult result = captureException(L, "large_allocation_error");
        CHECK(result.exceptionGenerated);
    }
}
#endif

TEST_CASE("IfElseExpression")
{
    ScopedFastFlag sff{"LuauIfElseExpressionBaseSupport", true};

    runConformance("ifelseexpr.lua");
}

TEST_SUITE_END();
