// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lua.h"
#include "lualib.h"
#include "luacode.h"
#include "luacodegen.h"

#include "Luau/BuiltinDefinitions.h"
#include "Luau/DenseHash.h"
#include "Luau/ModuleResolver.h"
#include "Luau/TypeInfer.h"
#include "Luau/BytecodeBuilder.h"
#include "Luau/Frontend.h"
#include "Luau/Compiler.h"
#include "Luau/CodeGen.h"
#include "Luau/BytecodeSummary.h"

#include "doctest.h"
#include "ScopedFlags.h"

#include <fstream>
#include <string>
#include <vector>
#include <math.h>

extern bool verbose;
extern bool codegen;
extern int optimizationLevel;

LUAU_FASTFLAG(LuauTaggedLuData);
LUAU_FASTFLAG(LuauSciNumberSkipTrailDot);
LUAU_FASTINT(CodegenHeuristicsInstructionLimit);

static lua_CompileOptions defaultOptions()
{
    lua_CompileOptions copts = {};
    copts.optimizationLevel = optimizationLevel;
    copts.debugLevel = 1;

    copts.vectorCtor = "vector";
    copts.vectorType = "vector";

    return copts;
}

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

    size_t bytecodeSize = 0;
    char* bytecode = luau_compile(s, l, nullptr, &bytecodeSize);
    int result = luau_load(L, chunkname, bytecode, bytecodeSize, 0);
    free(bytecode);

    if (result == 0)
        return 1;

    lua_pushnil(L);
    lua_insert(L, -2); // put before error message
    return 2;          // return nil plus error message
}

static int lua_vector(lua_State* L)
{
    double x = luaL_checknumber(L, 1);
    double y = luaL_checknumber(L, 2);
    double z = luaL_checknumber(L, 3);

#if LUA_VECTOR_SIZE == 4
    double w = luaL_optnumber(L, 4, 0.0);
    lua_pushvector(L, float(x), float(y), float(z), float(w));
#else
    lua_pushvector(L, float(x), float(y), float(z));
#endif
    return 1;
}

static int lua_vector_dot(lua_State* L)
{
    const float* a = luaL_checkvector(L, 1);
    const float* b = luaL_checkvector(L, 2);

    lua_pushnumber(L, a[0] * b[0] + a[1] * b[1] + a[2] * b[2]);
    return 1;
}

static int lua_vector_index(lua_State* L)
{
    const float* v = luaL_checkvector(L, 1);
    const char* name = luaL_checkstring(L, 2);

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

    luaL_error(L, "%s is not a valid member of vector", name);
}

static int lua_vector_namecall(lua_State* L)
{
    if (const char* str = lua_namecallatom(L, nullptr))
    {
        if (strcmp(str, "Dot") == 0)
            return lua_vector_dot(L);
    }

    luaL_error(L, "%s is not a valid method of vector", luaL_checkstring(L, 1));
}

int lua_silence(lua_State* L)
{
    return 0;
}

using StateRef = std::unique_ptr<lua_State, void (*)(lua_State*)>;

static StateRef runConformance(const char* name, void (*setup)(lua_State* L) = nullptr, void (*yield)(lua_State* L) = nullptr,
    lua_State* initialLuaState = nullptr, lua_CompileOptions* options = nullptr, bool skipCodegen = false)
{
#ifdef LUAU_CONFORMANCE_SOURCE_DIR
    std::string path = LUAU_CONFORMANCE_SOURCE_DIR;
    path += "/";
    path += name;
#else
    std::string path = __FILE__;
    path.erase(path.find_last_of("\\/"));
    path += "/conformance/";
    path += name;
#endif

    std::fstream stream(path, std::ios::in | std::ios::binary);
    INFO(path);
    REQUIRE(stream);

    std::string source(std::istreambuf_iterator<char>(stream), {});

    stream.close();

    if (!initialLuaState)
        initialLuaState = luaL_newstate();
    StateRef globalState(initialLuaState, lua_close);
    lua_State* L = globalState.get();

    if (codegen && !skipCodegen && luau_codegen_supported())
        luau_codegen_create(L);

    luaL_openlibs(L);

    // Register a few global functions for conformance tests
    std::vector<luaL_Reg> funcs = {
        {"collectgarbage", lua_collectgarbage},
        {"loadstring", lua_loadstring},
    };

    if (!verbose)
    {
        funcs.push_back({"print", lua_silence});
    }

    // "null" terminate the list of functions to register
    funcs.push_back({nullptr, nullptr});

    lua_pushvalue(L, LUA_GLOBALSINDEX);
    luaL_register(L, nullptr, funcs.data());
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

    // note: luau_compile supports nullptr options, but we need to customize our defaults to improve test coverage
    lua_CompileOptions opts = options ? *options : defaultOptions();

    size_t bytecodeSize = 0;
    char* bytecode = luau_compile(source.data(), source.size(), &opts, &bytecodeSize);
    int result = luau_load(L, chunkname.c_str(), bytecode, bytecodeSize, 0);
    free(bytecode);

    if (result == 0 && codegen && !skipCodegen && luau_codegen_supported())
        Luau::CodeGen::compile(L, -1, Luau::CodeGen::CodeGen_ColdFunctions);

    int status = (result == 0) ? lua_resume(L, nullptr, 0) : LUA_ERRSYNTAX;

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

static void* limitedRealloc(void* ud, void* ptr, size_t osize, size_t nsize)
{
    if (nsize == 0)
    {
        free(ptr);
        return nullptr;
    }
    else if (nsize > 8 * 1024 * 1024)
    {
        // For testing purposes return null for large allocations so we can generate errors related to memory allocation failures
        return nullptr;
    }
    else
    {
        return realloc(ptr, nsize);
    }
}

static std::vector<Luau::CodeGen::FunctionBytecodeSummary> analyzeFile(const char* source, const unsigned nestingLimit)
{
    Luau::BytecodeBuilder bcb;

    Luau::CompileOptions options;
    options.optimizationLevel = optimizationLevel;
    options.debugLevel = 1;

    compileOrThrow(bcb, source, options);

    const std::string& bytecode = bcb.getBytecode();

    std::unique_ptr<lua_State, void (*)(lua_State*)> globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    int result = luau_load(L, "source", bytecode.data(), bytecode.size(), 0);
    REQUIRE(result == 0);

    return Luau::CodeGen::summarizeBytecode(L, -1, nestingLimit);
}

TEST_SUITE_BEGIN("Conformance");

TEST_CASE("CodegenSupported")
{
    if (codegen && !luau_codegen_supported())
        MESSAGE("Native code generation is not supported by the current configuration and will be disabled");
}

TEST_CASE("Assert")
{
    runConformance("assert.lua");
}

TEST_CASE("Basic")
{
    runConformance("basic.lua");
}

TEST_CASE("Buffers")
{
    runConformance("buffers.lua");
}

TEST_CASE("Math")
{
    runConformance("math.lua");
}

TEST_CASE("Tables")
{
    runConformance("tables.lua", [](lua_State* L) {
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                if (lua_type(L, 1) == LUA_TNUMBER)
                {
                    unsigned v = luaL_checkunsigned(L, 1);
                    lua_pushlightuserdata(L, reinterpret_cast<void*>(uintptr_t(v)));
                }
                else
                {
                    const void* p = lua_topointer(L, 1);
                    LUAU_ASSERT(p); // we expect the test call to only pass GC values here
                    lua_pushlightuserdata(L, const_cast<void*>(p));
                }
                return 1;
            },
            "makelud");
        lua_setglobal(L, "makelud");
    });
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

TEST_CASE("StringInterp")
{
    runConformance("stringinterp.lua");
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

static int cxxthrow(lua_State* L)
{
#if LUA_USE_LONGJMP
    luaL_error(L, "oops");
#else
    throw std::runtime_error("oops");
#endif
}

TEST_CASE("PCall")
{
    runConformance(
        "pcall.lua",
        [](lua_State* L) {
            lua_pushcfunction(L, cxxthrow, "cxxthrow");
            lua_setglobal(L, "cxxthrow");

            lua_pushcfunction(
                L,
                [](lua_State* L) -> int {
                    lua_State* co = lua_tothread(L, 1);
                    lua_xmove(L, co, 1);
                    lua_resumeerror(co, L);
                    return 0;
                },
                "resumeerror");
            lua_setglobal(L, "resumeerror");
        },
        nullptr, lua_newstate(limitedRealloc, nullptr));
}

TEST_CASE("Pack")
{
    runConformance("tpack.lua");
}

TEST_CASE("Vector")
{
    runConformance(
        "vector.lua",
        [](lua_State* L) {
            lua_pushcfunction(L, lua_vector, "vector");
            lua_setglobal(L, "vector");

#if LUA_VECTOR_SIZE == 4
            lua_pushvector(L, 0.0f, 0.0f, 0.0f, 0.0f);
#else
            lua_pushvector(L, 0.0f, 0.0f, 0.0f);
#endif
            luaL_newmetatable(L, "vector");

            lua_pushstring(L, "__index");
            lua_pushcfunction(L, lua_vector_index, nullptr);
            lua_settable(L, -3);

            lua_pushstring(L, "__namecall");
            lua_pushcfunction(L, lua_vector_namecall, nullptr);
            lua_settable(L, -3);

            lua_setreadonly(L, -1, true);
            lua_setmetatable(L, -2);
            lua_pop(L, 1);
        },
        nullptr, nullptr, nullptr);
}

static void populateRTTI(lua_State* L, Luau::TypeId type)
{
    if (auto p = Luau::get<Luau::PrimitiveType>(type))
    {
        switch (p->type)
        {
        case Luau::PrimitiveType::Boolean:
            lua_pushstring(L, "boolean");
            break;

        case Luau::PrimitiveType::NilType:
            lua_pushstring(L, "nil");
            break;

        case Luau::PrimitiveType::Number:
            lua_pushstring(L, "number");
            break;

        case Luau::PrimitiveType::String:
            lua_pushstring(L, "string");
            break;

        case Luau::PrimitiveType::Thread:
            lua_pushstring(L, "thread");
            break;

        case Luau::PrimitiveType::Buffer:
            lua_pushstring(L, "buffer");
            break;

        default:
            LUAU_ASSERT(!"Unknown primitive type");
        }
    }
    else if (auto t = Luau::get<Luau::TableType>(type))
    {
        lua_newtable(L);

        for (const auto& [name, prop] : t->props)
        {
            populateRTTI(L, prop.type());
            lua_setfield(L, -2, name.c_str());
        }
    }
    else if (Luau::get<Luau::FunctionType>(type))
    {
        lua_pushstring(L, "function");
    }
    else if (Luau::get<Luau::AnyType>(type))
    {
        lua_pushstring(L, "any");
    }
    else if (auto i = Luau::get<Luau::IntersectionType>(type))
    {
        for (const auto& part : i->parts)
            LUAU_ASSERT(Luau::get<Luau::FunctionType>(part));

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
        Luau::NullFileResolver fileResolver;
        Luau::NullConfigResolver configResolver;
        Luau::Frontend frontend{&fileResolver, &configResolver};
        Luau::registerBuiltinGlobals(frontend, frontend.globals);
        Luau::freeze(frontend.globals.globalTypes);

        lua_newtable(L);

        for (const auto& [name, binding] : frontend.globals.globalScope->bindings)
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
    static bool singlestep = false;
    static int stephits = 0;

    SUBCASE("")
    {
        singlestep = false;
    }
    SUBCASE("SingleStep")
    {
        singlestep = true;
    }

    breakhits = 0;
    interruptedthread = nullptr;
    stephits = 0;

    lua_CompileOptions copts = defaultOptions();
    copts.debugLevel = 2;

    runConformance(
        "debugger.lua",
        [](lua_State* L) {
            lua_Callbacks* cb = lua_callbacks(L);

            lua_singlestep(L, singlestep);

            // this will only be called in single-step mode
            cb->debugstep = [](lua_State* L, lua_Debug* ar) {
                stephits++;
            };

            // for breakpoints to work we should make sure debugbreak is installed
            cb->debugbreak = [](lua_State* L, lua_Debug* ar) {
                breakhits++;

                // make sure we can trace the stack for every breakpoint we hit
                lua_debugtrace(L);

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
            lua_pushcclosurek(
                L,
                [](lua_State* L) -> int {
                    int line = luaL_checkinteger(L, 1);
                    bool enabled = luaL_optboolean(L, 2, true);

                    lua_Debug ar = {};
                    lua_getinfo(L, lua_stackdepth(L) - 1, "f", &ar);

                    lua_breakpoint(L, -1, line, enabled);
                    return 0;
                },
                "breakpoint", 0, nullptr);
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

                int v = lua_getargument(L, 0, 2);
                REQUIRE(v);
                CHECK(lua_tointeger(L, -1) == 42);
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
            else if (breakhits == 13)
            {
                // validate assignment via lua_getlocal
                const char* l = lua_getlocal(L, 0, 1);
                REQUIRE(l);
                CHECK(strcmp(l, "a") == 0);
                CHECK(lua_isnil(L, -1));
                lua_pop(L, 1);
            }

            if (interruptedthread)
            {
                lua_resume(interruptedthread, nullptr, 0);
                interruptedthread = nullptr;
            }
        },
        nullptr, &copts, /* skipCodegen */ true); // Native code doesn't support debugging yet

    CHECK(breakhits == 14); // 2 hits per breakpoint

    if (singlestep)
        CHECK(stephits > 100); // note; this will depend on number of instructions which can vary, so we just make sure the callback gets hit often
}

TEST_CASE("NDebugGetUpValue")
{
    lua_CompileOptions copts = defaultOptions();
    copts.debugLevel = 0;
    // Don't optimize away any upvalues
    copts.optimizationLevel = 0;

    runConformance(
        "ndebug_upvalues.lua", nullptr,
        [](lua_State* L) {
            lua_checkstack(L, LUA_MINSTACK);

            // push the second frame's closure to the stack
            lua_Debug ar = {};
            REQUIRE(lua_getinfo(L, 1, "f", &ar));

            // get the first upvalue
            const char* u = lua_getupvalue(L, -1, 1);
            REQUIRE(u);
            // upvalue name is unknown without debug info
            CHECK(strcmp(u, "") == 0);
            CHECK(lua_tointeger(L, -1) == 5);
            lua_pop(L, 2);
        },
        nullptr, &copts, /* skipCodegen */ false);
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

TEST_CASE("NewUserdataOverflow")
{
    StateRef globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    lua_pushcfunction(
        L,
        [](lua_State* L1) {
            // The following userdata request might cause an overflow.
            lua_newuserdatadtor(L1, SIZE_MAX, [](void* d) {});
            // The overflow might segfault in the following call.
            lua_getmetatable(L1, -1);
            return 0;
        },
        nullptr);

    CHECK(lua_pcall(L, 0, 0, 0) == LUA_ERRRUN);
    CHECK(strcmp(lua_tostring(L, -1), "memory allocation error: block too big") == 0);
}

TEST_CASE("ApiTables")
{
    StateRef globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    lua_newtable(L);
    lua_pushnumber(L, 123.0);
    lua_setfield(L, -2, "key");
    lua_pushnumber(L, 456.0);
    lua_rawsetfield(L, -2, "key2");
    lua_pushstring(L, "test");
    lua_rawseti(L, -2, 5);

    // lua_gettable
    lua_pushstring(L, "key");
    CHECK(lua_gettable(L, -2) == LUA_TNUMBER);
    CHECK(lua_tonumber(L, -1) == 123.0);
    lua_pop(L, 1);

    // lua_getfield
    CHECK(lua_getfield(L, -1, "key") == LUA_TNUMBER);
    CHECK(lua_tonumber(L, -1) == 123.0);
    lua_pop(L, 1);

    // lua_rawgetfield
    CHECK(lua_rawgetfield(L, -1, "key2") == LUA_TNUMBER);
    CHECK(lua_tonumber(L, -1) == 456.0);
    lua_pop(L, 1);

    // lua_rawget
    lua_pushstring(L, "key");
    CHECK(lua_rawget(L, -2) == LUA_TNUMBER);
    CHECK(lua_tonumber(L, -1) == 123.0);
    lua_pop(L, 1);

    // lua_rawgeti
    CHECK(lua_rawgeti(L, -1, 5) == LUA_TSTRING);
    CHECK(strcmp(lua_tostring(L, -1), "test") == 0);
    lua_pop(L, 1);

    // lua_cleartable
    lua_cleartable(L, -1);
    lua_pushnil(L);
    CHECK(lua_next(L, -2) == 0);

    lua_pop(L, 1);
}

TEST_CASE("ApiIter")
{
    StateRef globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    lua_newtable(L);
    lua_pushnumber(L, 123.0);
    lua_setfield(L, -2, "key");
    lua_pushnumber(L, 456.0);
    lua_rawsetfield(L, -2, "key2");
    lua_pushstring(L, "test");
    lua_rawseti(L, -2, 1);

    // Lua-compatible iteration interface: lua_next
    double sum1 = 0;
    lua_pushnil(L);
    while (lua_next(L, -2))
    {
        sum1 += lua_tonumber(L, -2); // key
        sum1 += lua_tonumber(L, -1); // value
        lua_pop(L, 1);               // pop value, key is used by lua_next
    }
    CHECK(sum1 == 580);

    // Luau iteration interface: lua_rawiter (faster and preferable to lua_next)
    double sum2 = 0;
    for (int index = 0; index = lua_rawiter(L, -1, index), index >= 0;)
    {
        sum2 += lua_tonumber(L, -2); // key
        sum2 += lua_tonumber(L, -1); // value
        lua_pop(L, 2);               // pop both key and value
    }
    CHECK(sum2 == 580);

    // pop table
    lua_pop(L, 1);
}

TEST_CASE("ApiCalls")
{
    StateRef globalState = runConformance("apicalls.lua", nullptr, nullptr, lua_newstate(limitedRealloc, nullptr));
    lua_State* L = globalState.get();

    // lua_call
    {
        lua_getfield(L, LUA_GLOBALSINDEX, "add");
        lua_pushnumber(L, 40);
        lua_pushnumber(L, 2);
        lua_call(L, 2, 1);
        CHECK(lua_isnumber(L, -1));
        CHECK(lua_tonumber(L, -1) == 42);
        lua_pop(L, 1);
    }

    // lua_pcall
    {
        lua_getfield(L, LUA_GLOBALSINDEX, "add");
        lua_pushnumber(L, 40);
        lua_pushnumber(L, 2);
        lua_pcall(L, 2, 1, 0);
        CHECK(lua_isnumber(L, -1));
        CHECK(lua_tonumber(L, -1) == 42);
        lua_pop(L, 1);
    }

    // lua_equal with a sleeping thread wake up
    {
        lua_State* L2 = lua_newthread(L);

        lua_getfield(L2, LUA_GLOBALSINDEX, "create_with_tm");
        lua_pushnumber(L2, 42);
        lua_pcall(L2, 1, 1, 0);

        lua_getfield(L2, LUA_GLOBALSINDEX, "create_with_tm");
        lua_pushnumber(L2, 42);
        lua_pcall(L2, 1, 1, 0);

        // Reset GC
        lua_gc(L2, LUA_GCCOLLECT, 0);

        // Try to mark 'L2' as sleeping
        // Can't control GC precisely, even in tests
        lua_gc(L2, LUA_GCSTEP, 8);

        CHECK(lua_equal(L2, -1, -2) == 1);
        lua_pop(L2, 2);
    }

    // lua_clonefunction + fenv
    {
        lua_getfield(L, LUA_GLOBALSINDEX, "getpi");
        lua_call(L, 0, 1);
        CHECK(lua_tonumber(L, -1) == 3.1415926);
        lua_pop(L, 1);

        lua_getfield(L, LUA_GLOBALSINDEX, "getpi");

        // clone & override env
        lua_clonefunction(L, -1);
        lua_newtable(L);
        lua_pushnumber(L, 42);
        lua_setfield(L, -2, "pi");
        lua_setfenv(L, -2);

        lua_call(L, 0, 1);
        CHECK(lua_tonumber(L, -1) == 42);
        lua_pop(L, 1);

        // this one calls original function again
        lua_call(L, 0, 1);
        CHECK(lua_tonumber(L, -1) == 3.1415926);
        lua_pop(L, 1);
    }

    // lua_clonefunction + upvalues
    {
        lua_getfield(L, LUA_GLOBALSINDEX, "incuv");
        lua_call(L, 0, 1);
        CHECK(lua_tonumber(L, -1) == 1);
        lua_pop(L, 1);

        lua_getfield(L, LUA_GLOBALSINDEX, "incuv");
        // two clones
        lua_clonefunction(L, -1);
        lua_clonefunction(L, -2);

        lua_call(L, 0, 1);
        CHECK(lua_tonumber(L, -1) == 2);
        lua_pop(L, 1);

        lua_call(L, 0, 1);
        CHECK(lua_tonumber(L, -1) == 3);
        lua_pop(L, 1);

        // this one calls original function again
        lua_call(L, 0, 1);
        CHECK(lua_tonumber(L, -1) == 4);
        lua_pop(L, 1);
    }

    // lua_pcall on OOM
    {
        lua_getfield(L, LUA_GLOBALSINDEX, "largealloc");
        int res = lua_pcall(L, 0, 0, 0);
        CHECK(res == LUA_ERRMEM);
    }

    // lua_pcall on OOM with an error handler
    {
        lua_getfield(L, LUA_GLOBALSINDEX, "oops");
        lua_getfield(L, LUA_GLOBALSINDEX, "largealloc");
        int res = lua_pcall(L, 0, 1, -2);
        CHECK(res == LUA_ERRMEM);
        CHECK((lua_isstring(L, -1) && strcmp(lua_tostring(L, -1), "oops") == 0));
        lua_pop(L, 1);
    }

    // lua_pcall on OOM with an error handler that errors
    {
        lua_getfield(L, LUA_GLOBALSINDEX, "error");
        lua_getfield(L, LUA_GLOBALSINDEX, "largealloc");
        int res = lua_pcall(L, 0, 1, -2);
        CHECK(res == LUA_ERRERR);
        CHECK((lua_isstring(L, -1) && strcmp(lua_tostring(L, -1), "error in error handling") == 0));
        lua_pop(L, 1);
    }

    // lua_pcall on OOM with an error handler that OOMs
    {
        lua_getfield(L, LUA_GLOBALSINDEX, "largealloc");
        lua_getfield(L, LUA_GLOBALSINDEX, "largealloc");
        int res = lua_pcall(L, 0, 1, -2);
        CHECK(res == LUA_ERRMEM);
        CHECK((lua_isstring(L, -1) && strcmp(lua_tostring(L, -1), "not enough memory") == 0));
        lua_pop(L, 1);
    }

    // lua_pcall on error with an error handler that OOMs
    {
        lua_getfield(L, LUA_GLOBALSINDEX, "largealloc");
        lua_getfield(L, LUA_GLOBALSINDEX, "error");
        int res = lua_pcall(L, 0, 1, -2);
        CHECK(res == LUA_ERRERR);
        CHECK((lua_isstring(L, -1) && strcmp(lua_tostring(L, -1), "error in error handling") == 0));
        lua_pop(L, 1);
    }
}

TEST_CASE("ApiAtoms")
{
    StateRef globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    lua_callbacks(L)->useratom = [](const char* s, size_t l) -> int16_t {
        if (strcmp(s, "string") == 0)
            return 0;
        if (strcmp(s, "important") == 0)
            return 1;

        return -1;
    };

    lua_pushstring(L, "string");
    lua_pushstring(L, "import");
    lua_pushstring(L, "ant");
    lua_concat(L, 2);
    lua_pushstring(L, "unimportant");

    int a1, a2, a3;
    const char* s1 = lua_tostringatom(L, -3, &a1);
    const char* s2 = lua_tostringatom(L, -2, &a2);
    const char* s3 = lua_tostringatom(L, -1, &a3);

    CHECK(strcmp(s1, "string") == 0);
    CHECK(a1 == 0);

    CHECK(strcmp(s2, "important") == 0);
    CHECK(a2 == 1);

    CHECK(strcmp(s3, "unimportant") == 0);
    CHECK(a3 == -1);
}

static bool endsWith(const std::string& str, const std::string& suffix)
{
    if (suffix.length() > str.length())
        return false;

    return suffix == std::string_view(str.c_str() + str.length() - suffix.length(), suffix.length());
}

TEST_CASE("ApiType")
{
    StateRef globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    lua_pushnumber(L, 2);
    CHECK(strcmp(luaL_typename(L, -1), "number") == 0);
    CHECK(strcmp(luaL_typename(L, 1), "number") == 0);
    CHECK(lua_type(L, -1) == LUA_TNUMBER);
    CHECK(lua_type(L, 1) == LUA_TNUMBER);

    CHECK(strcmp(luaL_typename(L, 2), "no value") == 0);
    CHECK(lua_type(L, 2) == LUA_TNONE);
    CHECK(strcmp(lua_typename(L, lua_type(L, 2)), "no value") == 0);

    lua_newuserdata(L, 0);
    CHECK(strcmp(luaL_typename(L, -1), "userdata") == 0);
    CHECK(lua_type(L, -1) == LUA_TUSERDATA);

    lua_newtable(L);
    lua_pushstring(L, "hello");
    lua_setfield(L, -2, "__type");
    lua_setmetatable(L, -2);

    CHECK(strcmp(luaL_typename(L, -1), "hello") == 0);
    CHECK(lua_type(L, -1) == LUA_TUSERDATA);
}

TEST_CASE("ApiBuffer")
{
    StateRef globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    lua_newbuffer(L, 1000);

    REQUIRE(lua_type(L, -1) == LUA_TBUFFER);

    CHECK(lua_isbuffer(L, -1));
    CHECK(lua_objlen(L, -1) == 1000);

    CHECK(strcmp(lua_typename(L, LUA_TBUFFER), "buffer") == 0);

    CHECK(strcmp(luaL_typename(L, -1), "buffer") == 0);

    void* p1 = lua_tobuffer(L, -1, nullptr);

    size_t len = 0;
    void* p2 = lua_tobuffer(L, -1, &len);
    CHECK(len == 1000);
    CHECK(p1 == p2);

    void* p3 = luaL_checkbuffer(L, -1, nullptr);
    CHECK(p1 == p3);

    len = 0;
    void* p4 = luaL_checkbuffer(L, -1, &len);
    CHECK(len == 1000);
    CHECK(p1 == p4);

    memset(p1, 0xab, 1000);

    CHECK(lua_topointer(L, -1) != nullptr);

    lua_newbuffer(L, 0);

    lua_pushvalue(L, -2);

    CHECK(lua_equal(L, -3, -1));
    CHECK(!lua_equal(L, -2, -1));

    lua_pop(L, 1);
}

TEST_CASE("AllocApi")
{
    int ud = 0;
    StateRef globalState(lua_newstate(limitedRealloc, &ud), lua_close);
    lua_State* L = globalState.get();

    void* udCheck = nullptr;
    bool allocfIsSet = lua_getallocf(L, &udCheck) == limitedRealloc;
    CHECK(allocfIsSet);
    CHECK(udCheck == &ud);
}

#if !LUA_USE_LONGJMP
TEST_CASE("ExceptionObject")
{
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

    StateRef globalState = runConformance("exceptions.lua", nullptr, nullptr, lua_newstate(limitedRealloc, nullptr));
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
    runConformance("ifelseexpr.lua");
}

// Optionally returns debug info for the first Luau stack frame that is encountered on the callstack.
static std::optional<lua_Debug> getFirstLuauFrameDebugInfo(lua_State* L)
{
    static std::string_view kLua = "Lua";
    lua_Debug ar;
    for (int i = 0; lua_getinfo(L, i, "sl", &ar); i++)
    {
        if (kLua == ar.what)
            return ar;
    }
    return std::nullopt;
}

TEST_CASE("TagMethodError")
{
    static std::vector<int> expectedHits;

    // Loop over two modes:
    //   when doLuaBreak is false the test only verifies that callbacks occur on the expected lines in the Luau source
    //   when doLuaBreak is true the test additionally calls lua_break to ensure breaking the debugger doesn't cause the VM to crash
    for (bool doLuaBreak : {false, true})
    {
        expectedHits = {22, 32};

        static int index;
        static bool luaBreak;
        index = 0;
        luaBreak = doLuaBreak;

        // 'yieldCallback' doesn't do anything, but providing the callback to runConformance
        // ensures that the call to lua_break doesn't cause an error to be generated because
        // runConformance doesn't expect the VM to be in the state LUA_BREAK.
        auto yieldCallback = [](lua_State* L) {};

        runConformance(
            "tmerror.lua",
            [](lua_State* L) {
                auto* cb = lua_callbacks(L);

                cb->debugprotectederror = [](lua_State* L) {
                    std::optional<lua_Debug> ar = getFirstLuauFrameDebugInfo(L);

                    CHECK(lua_isyieldable(L));
                    REQUIRE(ar.has_value());
                    REQUIRE(index < int(std::size(expectedHits)));
                    CHECK(ar->currentline == expectedHits[index++]);

                    if (luaBreak)
                    {
                        // Cause luau execution to break when 'error' is called via 'pcall'
                        // This call to lua_break is a regression test for an issue where debugprotectederror
                        // was called on a thread that couldn't be yielded even though lua_isyieldable was true.
                        lua_break(L);
                    }
                };
            },
            yieldCallback);

        // Make sure the number of break points hit was the expected number
        CHECK(index == std::size(expectedHits));
    }
}

TEST_CASE("Coverage")
{
    lua_CompileOptions copts = defaultOptions();
    copts.optimizationLevel = 1; // disable inlining to get fixed expected hit results
    copts.coverageLevel = 2;

    runConformance(
        "coverage.lua",
        [](lua_State* L) {
            lua_pushcfunction(
                L,
                [](lua_State* L) -> int {
                    luaL_argexpected(L, lua_isLfunction(L, 1), 1, "function");

                    lua_newtable(L);
                    lua_getcoverage(L, 1, L, [](void* context, const char* function, int linedefined, int depth, const int* hits, size_t size) {
                        lua_State* L = static_cast<lua_State*>(context);

                        lua_newtable(L);

                        lua_pushstring(L, function);
                        lua_setfield(L, -2, "name");

                        lua_pushinteger(L, linedefined);
                        lua_setfield(L, -2, "linedefined");

                        lua_pushinteger(L, depth);
                        lua_setfield(L, -2, "depth");

                        for (size_t i = 0; i < size; ++i)
                            if (hits[i] != -1)
                            {
                                lua_pushinteger(L, hits[i]);
                                lua_rawseti(L, -2, int(i));
                            }

                        lua_rawseti(L, -2, lua_objlen(L, -2) + 1);
                    });

                    return 1;
                },
                "getcoverage");
            lua_setglobal(L, "getcoverage");
        },
        nullptr, nullptr, &copts);
}

TEST_CASE("StringConversion")
{
    ScopedFastFlag luauSciNumberSkipTrailDot{FFlag::LuauSciNumberSkipTrailDot, true};

    runConformance("strconv.lua");
}

TEST_CASE("GCDump")
{
    // internal function, declared in lgc.h - not exposed via lua.h
    extern void luaC_dump(lua_State * L, void* file, const char* (*categoryName)(lua_State * L, uint8_t memcat));
    extern void luaC_enumheap(lua_State * L, void* context,
        void (*node)(void* context, void* ptr, uint8_t tt, uint8_t memcat, size_t size, const char* name),
        void (*edge)(void* context, void* from, void* to, const char* name));

    StateRef globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    // push various objects on stack to cover different paths
    lua_createtable(L, 1, 2);
    lua_pushstring(L, "value");
    lua_setfield(L, -2, "key");

    lua_pushstring(L, "u42");
    lua_setfield(L, -2, "__type");

    lua_pushinteger(L, 42);
    lua_rawseti(L, -2, 1000);

    lua_pushinteger(L, 42);
    lua_rawseti(L, -2, 1);

    lua_pushvalue(L, -1);
    lua_setmetatable(L, -2);

    lua_newuserdata(L, 42);
    lua_pushvalue(L, -2);
    lua_setmetatable(L, -2);

    lua_pushinteger(L, 1);
    lua_pushcclosure(L, lua_silence, "test", 1);

    lua_newbuffer(L, 100);

    lua_State* CL = lua_newthread(L);

    lua_pushstring(CL, "local x x = {} local function f() x[1] = math.abs(42) end function foo() coroutine.yield() end foo() return f");
    lua_loadstring(CL);
    lua_resume(CL, nullptr, 0);

#ifdef _WIN32
    const char* path = "NUL";
#else
    const char* path = "/dev/null";
#endif

    FILE* f = fopen(path, "w");
    REQUIRE(f);

    luaC_dump(L, f, nullptr);

    fclose(f);

    struct Node
    {
        void* ptr;
        uint8_t tag;
        uint8_t memcat;
        size_t size;
        std::string name;
    };

    struct EnumContext
    {
        EnumContext()
            : nodes{nullptr}
            , edges{nullptr}
        {
        }

        Luau::DenseHashMap<void*, Node> nodes;
        Luau::DenseHashMap<void*, void*> edges;
    } ctx;

    luaC_enumheap(
        L, &ctx,
        [](void* ctx, void* gco, uint8_t tt, uint8_t memcat, size_t size, const char* name) {
            EnumContext& context = *(EnumContext*)ctx;

            if (tt == LUA_TUSERDATA)
                CHECK(strcmp(name, "u42") == 0);

            context.nodes[gco] = {gco, tt, memcat, size, name ? name : ""};
        },
        [](void* ctx, void* s, void* t, const char*) {
            EnumContext& context = *(EnumContext*)ctx;
            context.edges[s] = t;
        });

    CHECK(!ctx.nodes.empty());
    CHECK(!ctx.edges.empty());
}

TEST_CASE("Interrupt")
{
    lua_CompileOptions copts = defaultOptions();
    copts.optimizationLevel = 1; // disable loop unrolling to get fixed expected hit results

    static int index;

    StateRef globalState = runConformance("interrupt.lua", nullptr, nullptr, nullptr, &copts);

    lua_State* L = globalState.get();

    // note: for simplicity here we setup the interrupt callback when the test starts
    // however, this carries a noticeable performance cost. in a real application,
    // it's advised to set interrupt callback on a timer from a different thread,
    // and set it back to nullptr once the interrupt triggered.

    // define the interrupt to check the expected hits
    static const int expectedhits[] = {11, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 20, 15, 15, 15, 15, 18, 25, 23, 26};

    lua_callbacks(L)->interrupt = [](lua_State* L, int gc) {
        if (gc >= 0)
            return;

        CHECK(index < int(std::size(expectedhits)));

        lua_Debug ar = {};
        lua_getinfo(L, 0, "l", &ar);

        CHECK(ar.currentline == expectedhits[index]);

        index++;

        // check that we can yield inside an interrupt
        if (index == 4)
            lua_yield(L, 0);
    };

    {
        lua_State* T = lua_newthread(L);

        lua_getglobal(T, "test");

        index = 0;
        int status = lua_resume(T, nullptr, 0);
        CHECK(status == LUA_YIELD);
        CHECK(index == 4);

        status = lua_resume(T, nullptr, 0);
        CHECK(status == LUA_OK);
        CHECK(index == int(std::size(expectedhits)));

        lua_pop(L, 1);
    }

    // redefine the interrupt to break after 10 iterations of a loop that would otherwise be infinite
    // the test exposes a few global functions that we will call; the interrupt will force a yield
    lua_callbacks(L)->interrupt = [](lua_State* L, int gc) {
        if (gc >= 0)
            return;

        CHECK(index < 11);
        if (++index == 11)
            lua_yield(L, 0);
    };

    for (int test = 1; test <= 10; ++test)
    {
        lua_State* T = lua_newthread(L);

        std::string name = "infloop" + std::to_string(test);
        lua_getglobal(T, name.c_str());

        index = 0;
        int status = lua_resume(T, nullptr, 0);
        CHECK(status == LUA_YIELD);
        CHECK(index == 11);

        // abandon the thread
        lua_pop(L, 1);
    }
}

TEST_CASE("UserdataApi")
{
    static int dtorhits = 0;

    dtorhits = 0;

    StateRef globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    // setup dtor for tag 42 (created later)
    auto dtor = [](lua_State* l, void* data) {
        dtorhits += *(int*)data;
    };
    bool dtorIsNull = lua_getuserdatadtor(L, 42) == nullptr;
    CHECK(dtorIsNull);
    lua_setuserdatadtor(L, 42, dtor);
    bool dtorIsSet = lua_getuserdatadtor(L, 42) == dtor;
    CHECK(dtorIsSet);

    // light user data
    int lud;
    lua_pushlightuserdata(L, &lud);

    CHECK(lua_tolightuserdata(L, -1) == &lud);
    CHECK(lua_touserdata(L, -1) == &lud);
    CHECK(lua_topointer(L, -1) == &lud);

    // regular user data
    int* ud1 = (int*)lua_newuserdata(L, 4);
    *ud1 = 42;

    CHECK(lua_tolightuserdata(L, -1) == nullptr);
    CHECK(lua_touserdata(L, -1) == ud1);
    CHECK(lua_topointer(L, -1) == ud1);

    // tagged user data
    int* ud2 = (int*)lua_newuserdatatagged(L, 4, 42);
    *ud2 = -4;

    CHECK(lua_touserdatatagged(L, -1, 42) == ud2);
    CHECK(lua_touserdatatagged(L, -1, 41) == nullptr);
    CHECK(lua_userdatatag(L, -1) == 42);

    lua_setuserdatatag(L, -1, 43);
    CHECK(lua_userdatatag(L, -1) == 43);
    lua_setuserdatatag(L, -1, 42);

    // user data with inline dtor
    void* ud3 = lua_newuserdatadtor(L, 4, [](void* data) {
        dtorhits += *(int*)data;
    });

    void* ud4 = lua_newuserdatadtor(L, 1, [](void* data) {
        dtorhits += *(char*)data;
    });

    *(int*)ud3 = 43;
    *(char*)ud4 = 3;

    // user data with named metatable
    luaL_newmetatable(L, "udata1");
    luaL_newmetatable(L, "udata2");

    void* ud5 = lua_newuserdata(L, 0);
    lua_getfield(L, LUA_REGISTRYINDEX, "udata1");
    lua_setmetatable(L, -2);

    void* ud6 = lua_newuserdata(L, 0);
    lua_getfield(L, LUA_REGISTRYINDEX, "udata2");
    lua_setmetatable(L, -2);

    CHECK(luaL_checkudata(L, -2, "udata1") == ud5);
    CHECK(luaL_checkudata(L, -1, "udata2") == ud6);

    globalState.reset();

    CHECK(dtorhits == 42);
}

TEST_CASE("LightuserdataApi")
{
    ScopedFastFlag luauTaggedLuData{FFlag::LuauTaggedLuData, true};

    StateRef globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    void* value = (void*)0x12345678;

    lua_pushlightuserdatatagged(L, value, 1);
    CHECK(lua_lightuserdatatag(L, -1) == 1);
    CHECK(lua_tolightuserdatatagged(L, -1, 0) == nullptr);
    CHECK(lua_tolightuserdatatagged(L, -1, 1) == value);

    lua_setlightuserdataname(L, 1, "id");
    CHECK(!lua_getlightuserdataname(L, 0));
    CHECK(strcmp(lua_getlightuserdataname(L, 1), "id") == 0);
    CHECK(strcmp(luaL_typename(L, -1), "id") == 0);
    lua_pop(L, 1);

    lua_pushlightuserdatatagged(L, value, 0);
    lua_pushlightuserdatatagged(L, value, 1);
    CHECK(lua_rawequal(L, -1, -2) == 0);
    lua_pop(L, 2);

    // Check lightuserdata table key uniqueness
    lua_newtable(L);

    lua_pushlightuserdatatagged(L, value, 2);
    lua_pushinteger(L, 20);
    lua_settable(L, -3);
    lua_pushlightuserdatatagged(L, value, 3);
    lua_pushinteger(L, 30);
    lua_settable(L, -3);

    lua_pushlightuserdatatagged(L, value, 2);
    lua_gettable(L, -2);
    lua_pushinteger(L, 20);
    CHECK(lua_rawequal(L, -1, -2) == 1);
    lua_pop(L, 2);

    lua_pushlightuserdatatagged(L, value, 3);
    lua_gettable(L, -2);
    lua_pushinteger(L, 30);
    CHECK(lua_rawequal(L, -1, -2) == 1);
    lua_pop(L, 2);

    lua_pop(L, 1);

    globalState.reset();
}

TEST_CASE("Iter")
{
    runConformance("iter.lua");
}

const int kInt64Tag = 1;
static int gInt64MT = -1;

static int64_t getInt64(lua_State* L, int idx)
{
    if (void* p = lua_touserdatatagged(L, idx, kInt64Tag))
        return *static_cast<int64_t*>(p);

    if (lua_isnumber(L, idx))
        return lua_tointeger(L, idx);

    luaL_typeerror(L, 1, "int64");
}

static void pushInt64(lua_State* L, int64_t value)
{
    void* p = lua_newuserdatatagged(L, sizeof(int64_t), kInt64Tag);

    lua_getref(L, gInt64MT);
    lua_setmetatable(L, -2);

    *static_cast<int64_t*>(p) = value;
}

TEST_CASE("Userdata")
{
    runConformance("userdata.lua", [](lua_State* L) {
        // create metatable with all the metamethods
        lua_newtable(L);
        gInt64MT = lua_ref(L, -1);

        // __index
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                void* p = lua_touserdatatagged(L, 1, kInt64Tag);
                if (!p)
                    luaL_typeerror(L, 1, "int64");

                const char* name = luaL_checkstring(L, 2);

                if (strcmp(name, "value") == 0)
                {
                    lua_pushnumber(L, double(*static_cast<int64_t*>(p)));
                    return 1;
                }

                luaL_error(L, "unknown field %s", name);
            },
            nullptr);
        lua_setfield(L, -2, "__index");

        // __newindex
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                void* p = lua_touserdatatagged(L, 1, kInt64Tag);
                if (!p)
                    luaL_typeerror(L, 1, "int64");

                const char* name = luaL_checkstring(L, 2);

                if (strcmp(name, "value") == 0)
                {
                    double value = luaL_checknumber(L, 3);
                    *static_cast<int64_t*>(p) = int64_t(value);
                    return 0;
                }

                luaL_error(L, "unknown field %s", name);
            },
            nullptr);
        lua_setfield(L, -2, "__newindex");

        // __eq
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                lua_pushboolean(L, getInt64(L, 1) == getInt64(L, 2));
                return 1;
            },
            nullptr);
        lua_setfield(L, -2, "__eq");

        // __lt
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                lua_pushboolean(L, getInt64(L, 1) < getInt64(L, 2));
                return 1;
            },
            nullptr);
        lua_setfield(L, -2, "__lt");

        // __le
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                lua_pushboolean(L, getInt64(L, 1) <= getInt64(L, 2));
                return 1;
            },
            nullptr);
        lua_setfield(L, -2, "__le");

        // __add
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                pushInt64(L, getInt64(L, 1) + getInt64(L, 2));
                return 1;
            },
            nullptr);
        lua_setfield(L, -2, "__add");

        // __sub
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                pushInt64(L, getInt64(L, 1) - getInt64(L, 2));
                return 1;
            },
            nullptr);
        lua_setfield(L, -2, "__sub");

        // __mul
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                pushInt64(L, getInt64(L, 1) * getInt64(L, 2));
                return 1;
            },
            nullptr);
        lua_setfield(L, -2, "__mul");

        // __div
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                // ideally we'd guard against 0 but it's a test so eh
                pushInt64(L, getInt64(L, 1) / getInt64(L, 2));
                return 1;
            },
            nullptr);
        lua_setfield(L, -2, "__div");

        // __idiv
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                // for testing we use different semantics here compared to __div: __idiv rounds to negative inf, __div truncates (rounds to zero)
                // additionally, division loses precision here outside of 2^53 range
                // we do not necessarily recommend this behavior in production code!
                pushInt64(L, int64_t(floor(double(getInt64(L, 1)) / double(getInt64(L, 2)))));
                return 1;
            },
            nullptr);
        lua_setfield(L, -2, "__idiv");

        // __mod
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                // ideally we'd guard against 0 and INT64_MIN but it's a test so eh
                pushInt64(L, getInt64(L, 1) % getInt64(L, 2));
                return 1;
            },
            nullptr);
        lua_setfield(L, -2, "__mod");

        // __pow
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                pushInt64(L, int64_t(pow(double(getInt64(L, 1)), double(getInt64(L, 2)))));
                return 1;
            },
            nullptr);
        lua_setfield(L, -2, "__pow");

        // __unm
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                pushInt64(L, -getInt64(L, 1));
                return 1;
            },
            nullptr);
        lua_setfield(L, -2, "__unm");

        // __tostring
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                int64_t value = getInt64(L, 1);
                std::string str = std::to_string(value);
                lua_pushlstring(L, str.c_str(), str.length());
                return 1;
            },
            nullptr);
        lua_setfield(L, -2, "__tostring");

        // ctor
        lua_pushcfunction(
            L,
            [](lua_State* L) {
                double v = luaL_checknumber(L, 1);
                pushInt64(L, int64_t(v));
                return 1;
            },
            "int64");
        lua_setglobal(L, "int64");
    });
}

TEST_CASE("SafeEnv")
{
    runConformance("safeenv.lua");
}

TEST_CASE("Native")
{
    runConformance("native.lua");
}

TEST_CASE("NativeTypeAnnotations")
{
    // This tests requires code to run natively, otherwise all 'is_native' checks will fail
    if (!codegen || !luau_codegen_supported())
        return;

    runConformance(
        "native_types.lua",
        [](lua_State* L) {
            // add is_native() function
            lua_pushcclosurek(
                L,
                [](lua_State* L) -> int {
                    extern int luaG_isnative(lua_State * L, int level);

                    lua_pushboolean(L, luaG_isnative(L, 1));
                    return 1;
                },
                "is_native", 0, nullptr);
            lua_setglobal(L, "is_native");

            // for vector tests
            lua_pushcfunction(L, lua_vector, "vector");
            lua_setglobal(L, "vector");

#if LUA_VECTOR_SIZE == 4
            lua_pushvector(L, 0.0f, 0.0f, 0.0f, 0.0f);
#else
            lua_pushvector(L, 0.0f, 0.0f, 0.0f);
#endif
            luaL_newmetatable(L, "vector");

            lua_setreadonly(L, -1, true);
            lua_setmetatable(L, -2);
            lua_pop(L, 1);
        },
        nullptr, nullptr, nullptr);
}

TEST_CASE("HugeFunction")
{
    std::string source;

    // add non-executed block that requires JUMPKX and generates a lot of constants that take available short (15-bit) constant space
    source += "if ... then\n";
    source += "local _ = {\n";

    for (int i = 0; i < 40000; ++i)
    {
        source += "0.";
        source += std::to_string(i);
        source += ",";
    }

    source += "}\n";
    source += "end\n";

    // use failed fast-calls with imports and constants to exercise all of the more complex fallback sequences
    source += "return bit32.lshift('84', -1)";

    StateRef globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    if (codegen && luau_codegen_supported())
        luau_codegen_create(L);

    luaL_openlibs(L);
    luaL_sandbox(L);
    luaL_sandboxthread(L);

    size_t bytecodeSize = 0;
    char* bytecode = luau_compile(source.data(), source.size(), nullptr, &bytecodeSize);
    int result = luau_load(L, "=HugeFunction", bytecode, bytecodeSize, 0);
    free(bytecode);

    REQUIRE(result == 0);

    if (codegen && luau_codegen_supported())
        Luau::CodeGen::compile(L, -1, Luau::CodeGen::CodeGen_ColdFunctions);

    int status = lua_resume(L, nullptr, 0);
    REQUIRE(status == 0);

    CHECK(lua_tonumber(L, -1) == 42);
}

TEST_CASE("IrInstructionLimit")
{
    if (!codegen || !luau_codegen_supported())
        return;

    ScopedFastInt codegenHeuristicsInstructionLimit{FInt::CodegenHeuristicsInstructionLimit, 50'000};

    std::string source;

    // Generate a hundred fat functions
    for (int fn = 0; fn < 100; fn++)
    {
        source += "local function fn" + std::to_string(fn) + "(...)\n";
        source += "if ... then\n";
        source += "local p1, p2 = ...\n";
        source += "local _ = {\n";

        for (int i = 0; i < 100; ++i)
        {
            source += "p1*0." + std::to_string(i) + ",";
            source += "p2+0." + std::to_string(i) + ",";
        }

        source += "}\n";
        source += "return _\n";
        source += "end\n";
        source += "end\n";
    }

    StateRef globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    luau_codegen_create(L);

    luaL_openlibs(L);
    luaL_sandbox(L);
    luaL_sandboxthread(L);

    size_t bytecodeSize = 0;
    char* bytecode = luau_compile(source.data(), source.size(), nullptr, &bytecodeSize);
    int result = luau_load(L, "=HugeFunction", bytecode, bytecodeSize, 0);
    free(bytecode);

    REQUIRE(result == 0);

    Luau::CodeGen::CompilationStats nativeStats = {};
    Luau::CodeGen::CodeGenCompilationResult nativeResult = Luau::CodeGen::compile(L, -1, Luau::CodeGen::CodeGen_ColdFunctions, &nativeStats);

    // Limit is not hit immediately, so with some functions compiled it should be a success
    CHECK(nativeResult != Luau::CodeGen::CodeGenCompilationResult::CodeGenFailed);

    // We should be able to compile at least one of our functions
    CHECK(nativeStats.functionsCompiled > 0);

    // But because of the limit, not all of them (101 because there's an extra global function)
    CHECK(nativeStats.functionsCompiled < 101);
}

TEST_CASE("BytecodeDistributionPerFunctionTest")
{
    const char* source = R"(
local function first(n, p)
  local t = {}
  for i=1,p do t[i] = i*10 end

  local function inner(_,n)
    if n > 0 then
      n = n-1
      return n, unpack(t)
    end
  end
  return inner, nil, n
end

local function second(x)
 return x[1]
end
)";

    std::vector<Luau::CodeGen::FunctionBytecodeSummary> summaries(analyzeFile(source, 0));

    CHECK_EQ(summaries[0].getName(), "inner");
    CHECK_EQ(summaries[0].getLine(), 6);
    CHECK_EQ(summaries[0].getCounts(0),
        std::vector<unsigned>({0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0}));

    CHECK_EQ(summaries[1].getName(), "first");
    CHECK_EQ(summaries[1].getLine(), 2);
    CHECK_EQ(summaries[1].getCounts(0),
        std::vector<unsigned>({0, 0, 1, 0, 2, 0, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));


    CHECK_EQ(summaries[2].getName(), "second");
    CHECK_EQ(summaries[2].getLine(), 15);
    CHECK_EQ(summaries[2].getCounts(0),
        std::vector<unsigned>({0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));

    CHECK_EQ(summaries[3].getName(), "");
    CHECK_EQ(summaries[3].getLine(), 1);
    CHECK_EQ(summaries[3].getCounts(0),
        std::vector<unsigned>({0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
}

TEST_SUITE_END();
