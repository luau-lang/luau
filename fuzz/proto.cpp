// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "src/libfuzzer/libfuzzer_macro.h"
#include "luau.pb.h"

#include "Luau/TypeInfer.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/ModuleResolver.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Compiler.h"
#include "Luau/Linter.h"
#include "Luau/BytecodeBuilder.h"
#include "Luau/Common.h"
#include "Luau/ToString.h"
#include "Luau/Transpiler.h"

#include "lua.h"
#include "lualib.h"

#include <chrono>

// Select components to fuzz
const bool kFuzzCompiler = true;
const bool kFuzzLinter = true;
const bool kFuzzTypeck = true;
const bool kFuzzVM = true;
const bool kFuzzTranspile = true;

// Should we generate type annotations?
const bool kFuzzTypes = true;

static_assert(!(kFuzzVM && !kFuzzCompiler), "VM requires the compiler!");

std::string protoprint(const luau::StatBlock& stat, bool types);

LUAU_FASTINT(LuauTypeInferRecursionLimit)
LUAU_FASTINT(LuauTypeInferTypePackLoopLimit)
LUAU_FASTINT(LuauCheckRecursionLimit)
LUAU_FASTINT(LuauTableTypeMaximumStringifierLength)
LUAU_FASTINT(LuauTypeInferIterationLimit)
LUAU_FASTINT(LuauTarjanChildLimit)

std::chrono::milliseconds kInterruptTimeout(10);
std::chrono::time_point<std::chrono::system_clock> interruptDeadline;

size_t kHeapLimit = 512 * 1024 * 1024;
size_t heapSize = 0;

void interrupt(lua_State* L, int gc)
{
    if (gc >= 0)
        return;

    if (std::chrono::system_clock::now() > interruptDeadline)
    {
        lua_checkstack(L, 1);
        luaL_error(L, "execution timed out");
    }
}

void* allocate(lua_State* L, void* ud, void* ptr, size_t osize, size_t nsize)
{
    if (nsize == 0)
    {
        heapSize -= osize;
        free(ptr);
        return NULL;
    }
    else
    {
        if (heapSize - osize + nsize > kHeapLimit)
            return NULL;

        heapSize -= osize;
        heapSize += nsize;

        return realloc(ptr, nsize);
    }
}

lua_State* createGlobalState()
{
    lua_State* L = lua_newstate(allocate, NULL);

    lua_callbacks(L)->interrupt = interrupt;

    luaL_openlibs(L);
    luaL_sandbox(L);

    return L;
}

int registerTypes(Luau::TypeChecker& env)
{
    using namespace Luau;
    using std::nullopt;

    Luau::registerBuiltinTypes(env);

    TypeArena& arena = env.globalTypes;

    // Vector3 stub
    TypeId vector3MetaType = arena.addType(TableTypeVar{});

    TypeId vector3InstanceType = arena.addType(ClassTypeVar{"Vector3", {}, nullopt, vector3MetaType, {}, {}});
    getMutable<ClassTypeVar>(vector3InstanceType)->props = {
        {"X", {env.numberType}},
        {"Y", {env.numberType}},
        {"Z", {env.numberType}},
    };

    getMutable<TableTypeVar>(vector3MetaType)->props = {
        {"__add", {makeFunction(arena, nullopt, {vector3InstanceType, vector3InstanceType}, {vector3InstanceType})}},
    };

    env.globalScope->exportedTypeBindings["Vector3"] = TypeFun{{}, vector3InstanceType};

    // Instance stub
    TypeId instanceType = arena.addType(ClassTypeVar{"Instance", {}, nullopt, nullopt, {}, {}});
    getMutable<ClassTypeVar>(instanceType)->props = {
        {"Name", {env.stringType}},
    };

    env.globalScope->exportedTypeBindings["Instance"] = TypeFun{{}, instanceType};

    // Part stub
    TypeId partType = arena.addType(ClassTypeVar{"Part", {}, instanceType, nullopt, {}, {}});
    getMutable<ClassTypeVar>(partType)->props = {
        {"Position", {vector3InstanceType}},
    };

    env.globalScope->exportedTypeBindings["Part"] = TypeFun{{}, partType};

    for (const auto& [_, fun] : env.globalScope->exportedTypeBindings)
        persist(fun.type);

    return 0;
}

static std::string debugsource;

DEFINE_PROTO_FUZZER(const luau::StatBlock& message)
{
    FInt::LuauTypeInferRecursionLimit.value = 100;
    FInt::LuauTypeInferTypePackLoopLimit.value = 100;
    FInt::LuauCheckRecursionLimit.value = 100;
    FInt::LuauTypeInferIterationLimit.value = 1000;
    FInt::LuauTarjanChildLimit.value = 1000;
    FInt::LuauTableTypeMaximumStringifierLength.value = 100;

    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = true;

    Luau::Allocator allocator;
    Luau::AstNameTable names(allocator);

    std::string source = protoprint(message, kFuzzTypes);

    // stash source in a global for easier crash dump debugging
    debugsource = source;

    Luau::ParseResult parseResult = Luau::Parser::parse(source.c_str(), source.size(), names, allocator);

    // "static" here is to accelerate fuzzing process by only creating and populating the type environment once
    static Luau::NullModuleResolver moduleResolver;
    static Luau::InternalErrorReporter iceHandler;
    static Luau::TypeChecker sharedEnv(&moduleResolver, &iceHandler);
    static int once = registerTypes(sharedEnv);
    (void)once;
    static int once2 = (Luau::freeze(sharedEnv.globalTypes), 0);
    (void)once2;

    iceHandler.onInternalError = [](const char* error) {
        printf("ICE: %s\n", error);
        LUAU_ASSERT(!"ICE");
    };

    static bool debug = getenv("LUAU_DEBUG") != 0;

    if (debug)
    {
        fprintf(stdout, "--\n%s\n", source.c_str());
        fflush(stdout);
    }

    std::string bytecode;

    // compile
    if (kFuzzCompiler && parseResult.errors.empty())
    {
        Luau::CompileOptions compileOptions;

        try
        {
            Luau::BytecodeBuilder bcb;
            Luau::compileOrThrow(bcb, parseResult.root, names, compileOptions);
            bytecode = bcb.getBytecode();
        }
        catch (const Luau::CompileError&)
        {
            // not all valid ASTs can be compiled due to limits on number of registers
        }
    }

    // typecheck
    if (kFuzzTypeck && parseResult.root)
    {
        Luau::SourceModule sourceModule;
        sourceModule.root = parseResult.root;
        sourceModule.mode = Luau::Mode::Nonstrict;

        Luau::TypeChecker typeck(&moduleResolver, &iceHandler);
        typeck.globalScope = sharedEnv.globalScope;

        Luau::ModulePtr module = nullptr;

        try
        {
            module = typeck.check(sourceModule, Luau::Mode::Nonstrict);
        }
        catch (std::exception&)
        {
            // This catches internal errors that the type checker currently (unfortunately) throws in some cases
        }

        // lint (note that we need access to types so we need to do this with typeck in scope)
        if (kFuzzLinter)
        {
            Luau::LintOptions lintOptions = {~0u};
            Luau::lint(parseResult.root, names, sharedEnv.globalScope, module.get(), lintOptions);
        }
    }

    // validate sharedEnv post-typecheck; valuable for debugging some typeck crashes but slows fuzzing down
    // note: it's important for typeck to be destroyed at this point!
    if (kFuzzTypeck)
    {
        for (auto& p : sharedEnv.globalScope->bindings)
        {
            Luau::ToStringOptions opts;
            opts.exhaustive = true;
            opts.maxTableLength = 0;
            opts.maxTypeLength = 0;

            toString(p.second.typeId, opts); // toString walks the entire type, making sure ASAN catches access to destroyed type arenas
        }
    }

    if (kFuzzTranspile && parseResult.root)
    {
        transpileWithTypes(*parseResult.root);
    }

    // run resulting bytecode
    if (kFuzzVM && bytecode.size())
    {
        static lua_State* globalState = createGlobalState();

        lua_State* L = lua_newthread(globalState);
        luaL_sandboxthread(L);

        if (luau_load(L, "=fuzz", bytecode.data(), bytecode.size(), 0) == 0)
        {
            interruptDeadline = std::chrono::system_clock::now() + kInterruptTimeout;

            lua_resume(L, NULL, 0);
        }

        lua_pop(globalState, 1);

        // we'd expect full GC to reclaim all memory allocated by the script
        lua_gc(globalState, LUA_GCCOLLECT, 0);
        LUAU_ASSERT(heapSize < 256 * 1024);
    }
}
