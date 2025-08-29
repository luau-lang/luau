// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "src/libfuzzer/libfuzzer_macro.h"
#include "luau.pb.h"

#include "Luau/BuiltinDefinitions.h"
#include "Luau/BytecodeBuilder.h"
#include "Luau/CodeGen.h"
#include "Luau/Common.h"
#include "Luau/Compiler.h"
#include "Luau/Config.h"
#include "Luau/Frontend.h"
#include "Luau/Linter.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Parser.h"
#include "Luau/ToString.h"
#include "Luau/Transpiler.h"
#include "Luau/TypeInfer.h"

#include "lua.h"
#include "lualib.h"

#include <chrono>
#include <cstring>

static bool getEnvParam(const char* name, bool def)
{
    char* val = getenv(name);
    if (val == nullptr)
        return def;
    else
        return strcmp(val, "0") != 0;
}

// Select components to fuzz
const bool kFuzzCompiler = getEnvParam("LUAU_FUZZ_COMPILER", true);
const bool kFuzzLinter = getEnvParam("LUAU_FUZZ_LINTER", true);
const bool kFuzzTypeck = getEnvParam("LUAU_FUZZ_TYPE_CHECK", true);
const bool kFuzzVM = getEnvParam("LUAU_FUZZ_VM", true);
const bool kFuzzTranspile = getEnvParam("LUAU_FUZZ_TRANSPILE", true);
const bool kFuzzCodegenVM = getEnvParam("LUAU_FUZZ_CODEGEN_VM", true);
const bool kFuzzCodegenAssembly = getEnvParam("LUAU_FUZZ_CODEGEN_ASM", true);
const bool kFuzzUseNewSolver = getEnvParam("LUAU_FUZZ_NEW_SOLVER", false);

// Should we generate type annotations?
const bool kFuzzTypes = getEnvParam("LUAU_FUZZ_GEN_TYPES", true);

const Luau::CodeGen::AssemblyOptions::Target kFuzzCodegenTarget = Luau::CodeGen::AssemblyOptions::A64;

std::vector<std::string> protoprint(const luau::ModuleSet& stat, bool types);

LUAU_FASTINT(LuauTypeInferRecursionLimit)
LUAU_FASTINT(LuauTypeInferTypePackLoopLimit)
LUAU_FASTINT(LuauCheckRecursionLimit)
LUAU_FASTINT(LuauTableTypeMaximumStringifierLength)
LUAU_FASTINT(LuauTypeInferIterationLimit)
LUAU_FASTINT(LuauTarjanChildLimit)
LUAU_FASTFLAG(DebugLuauFreezeArena)
LUAU_FASTFLAG(DebugLuauAbortingChecks)
LUAU_FASTFLAG(LuauSolverV2)

const double kTypecheckTimeoutSec = 4.0;

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

void* allocate(void* ud, void* ptr, size_t osize, size_t nsize)
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

    if (kFuzzCodegenVM && Luau::CodeGen::isSupported())
        Luau::CodeGen::create(L);

    lua_callbacks(L)->interrupt = interrupt;

    luaL_openlibs(L);
    luaL_sandbox(L);

    return L;
}

int registerTypes(Luau::Frontend& frontend, Luau::GlobalTypes& globals, bool forAutocomplete)
{
    using namespace Luau;
    using std::nullopt;

    Luau::registerBuiltinGlobals(frontend, globals, forAutocomplete);

    TypeArena& arena = globals.globalTypes;
    BuiltinTypes& builtinTypes = *globals.builtinTypes;

    // Vector3 stub
    TypeId vector3MetaType = arena.addType(TableType{});

    TypeId vector3InstanceType = arena.addType(ExternType{"Vector3", {}, nullopt, vector3MetaType, {}, {}, "Test", {}});
    getMutable<ExternType>(vector3InstanceType)->props = {
        {"X", {builtinTypes.numberType}},
        {"Y", {builtinTypes.numberType}},
        {"Z", {builtinTypes.numberType}},
    };

    getMutable<TableType>(vector3MetaType)->props = {
        {"__add", {makeFunction(arena, nullopt, {vector3InstanceType, vector3InstanceType}, {vector3InstanceType})}},
    };
    getMutable<TableType>(vector3MetaType)->state = TableState::Sealed;

    globals.globalScope->exportedTypeBindings["Vector3"] = TypeFun{{}, vector3InstanceType};

    // Instance stub
    TypeId instanceType = arena.addType(ExternType{"Instance", {}, nullopt, nullopt, {}, {}, "Test", {}});
    getMutable<ExternType>(instanceType)->props = {
        {"Name", {builtinTypes.stringType}},
    };

    globals.globalScope->exportedTypeBindings["Instance"] = TypeFun{{}, instanceType};

    // Part stub
    TypeId partType = arena.addType(ExternType{"Part", {}, instanceType, nullopt, {}, {}, "Test", {}});
    getMutable<ExternType>(partType)->props = {
        {"Position", {vector3InstanceType}},
    };

    globals.globalScope->exportedTypeBindings["Part"] = TypeFun{{}, partType};

    for (const auto& [_, fun] : globals.globalScope->exportedTypeBindings)
        persist(fun.type);

    return 0;
}

static void setupFrontend(Luau::Frontend& frontend)
{
    registerTypes(frontend, frontend.globals, false);
    Luau::freeze(frontend.globals.globalTypes);

    registerTypes(frontend, frontend.globalsForAutocomplete, true);
    Luau::freeze(frontend.globalsForAutocomplete.globalTypes);

    frontend.iceHandler.onInternalError = [](const char* error)
    {
        printf("ICE: %s\n", error);
        LUAU_ASSERT(!"ICE");
    };
}

static Luau::FrontendOptions getFrontendOptions()
{
    Luau::FrontendOptions options;

    options.retainFullTypeGraphs = true;
    options.forAutocomplete = false;
    options.runLintChecks = kFuzzLinter;

    options.moduleTimeLimitSec = kTypecheckTimeoutSec;

    return options;
}

struct FuzzFileResolver : Luau::FileResolver
{
    std::optional<Luau::SourceCode> readSource(const Luau::ModuleName& name) override
    {
        auto it = source.find(name);
        if (it == source.end())
            return std::nullopt;

        return Luau::SourceCode{it->second, Luau::SourceCode::Module};
    }

    std::optional<Luau::ModuleInfo> resolveModule(const Luau::ModuleInfo* context, Luau::AstExpr* expr) override
    {
        if (Luau::AstExprGlobal* g = expr->as<Luau::AstExprGlobal>())
            return Luau::ModuleInfo{g->name.value};

        return std::nullopt;
    }

    std::string getHumanReadableModuleName(const Luau::ModuleName& name) const override
    {
        return name;
    }

    std::optional<std::string> getEnvironmentForModule(const Luau::ModuleName& name) const override
    {
        return std::nullopt;
    }

    std::unordered_map<Luau::ModuleName, std::string> source;
};

struct FuzzConfigResolver : Luau::ConfigResolver
{
    FuzzConfigResolver()
    {
        defaultConfig.mode = Luau::Mode::Nonstrict;
        defaultConfig.enabledLint.warningMask = ~0ull;
        defaultConfig.parseOptions.captureComments = true;
    }

    virtual const Luau::Config& getConfig(const Luau::ModuleName& name) const override
    {
        return defaultConfig;
    }

    Luau::Config defaultConfig;
};

static std::vector<std::string> debugsources;

DEFINE_PROTO_FUZZER(const luau::ModuleSet& message)
{
    if (!kFuzzCompiler && (kFuzzCodegenAssembly || kFuzzCodegenVM || kFuzzVM))
    {
        printf("Compiler is required in order to fuzz codegen or the VM\n");
        LUAU_ASSERT(false);
        return;
    }

    FInt::LuauTypeInferRecursionLimit.value = 100;
    FInt::LuauTypeInferTypePackLoopLimit.value = 100;
    FInt::LuauCheckRecursionLimit.value = 100;
    FInt::LuauTypeInferIterationLimit.value = 1000;
    FInt::LuauTarjanChildLimit.value = 1000;
    FInt::LuauTableTypeMaximumStringifierLength.value = 100;

    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
    {
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = true;
    }

    FFlag::DebugLuauFreezeArena.value = true;
    FFlag::DebugLuauAbortingChecks.value = true;
    FFlag::LuauSolverV2.value = kFuzzUseNewSolver;

    std::vector<std::string> sources = protoprint(message, kFuzzTypes);

    // stash source in a global for easier crash dump debugging
    debugsources = sources;

    static bool debug = getenv("LUAU_DEBUG") != 0;

    if (debug)
    {
        for (std::string& source : sources)
            fprintf(stdout, "--\n%s\n", source.c_str());
        fflush(stdout);
    }

    // parse all sources
    std::vector<std::unique_ptr<Luau::Allocator>> parseAllocators;
    std::vector<std::unique_ptr<Luau::AstNameTable>> parseNameTables;

    Luau::ParseOptions parseOptions;
    parseOptions.captureComments = true;

    std::vector<Luau::ParseResult> parseResults;

    for (std::string& source : sources)
    {
        parseAllocators.push_back(std::make_unique<Luau::Allocator>());
        parseNameTables.push_back(std::make_unique<Luau::AstNameTable>(*parseAllocators.back()));

        parseResults.push_back(Luau::Parser::parse(source.c_str(), source.size(), *parseNameTables.back(), *parseAllocators.back(), parseOptions));
    }

    // typecheck all sources
    if (kFuzzTypeck)
    {
        static FuzzFileResolver fileResolver;
        static FuzzConfigResolver configResolver;
        static Luau::FrontendOptions defaultOptions = getFrontendOptions();
        static Luau::Frontend frontend(&fileResolver, &configResolver, defaultOptions);

        static int once = (setupFrontend(frontend), 0);
        (void)once;

        // restart
        frontend.clear();
        fileResolver.source.clear();

        // load sources
        for (size_t i = 0; i < sources.size(); i++)
        {
            std::string name = "module" + std::to_string(i);
            fileResolver.source[name] = sources[i];
        }

        // check sources
        for (size_t i = 0; i < sources.size(); i++)
        {
            std::string name = "module" + std::to_string(i);

            try
            {
                frontend.check(name);

                // Second pass in strict mode (forced by auto-complete)
                Luau::FrontendOptions options = defaultOptions;
                options.forAutocomplete = true;
                frontend.check(name, options);
            }
            catch (std::exception&)
            {
                // This catches internal errors that the type checker currently (unfortunately) throws in some cases
            }
        }

        // validate sharedEnv post-typecheck; valuable for debugging some typeck crashes but slows fuzzing down
        // note: it's important for typeck to be destroyed at this point!
        for (auto& p : frontend.globals.globalScope->bindings)
        {
            Luau::ToStringOptions opts;
            opts.exhaustive = true;
            opts.maxTableLength = 0;
            opts.maxTypeLength = 0;

            toString(p.second.typeId, opts); // toString walks the entire type, making sure ASAN catches access to destroyed type arenas
        }
    }

    if (kFuzzTranspile)
    {
        for (Luau::ParseResult& parseResult : parseResults)
        {
            if (parseResult.root)
                transpileWithTypes(*parseResult.root);
        }
    }

    std::string bytecode;

    // compile
    if (kFuzzCompiler)
    {
        for (size_t i = 0; i < parseResults.size(); i++)
        {
            Luau::ParseResult& parseResult = parseResults[i];
            Luau::AstNameTable& parseNameTable = *parseNameTables[i];

            if (parseResult.errors.empty())
            {
                Luau::CompileOptions compileOptions;

                try
                {
                    Luau::BytecodeBuilder bcb;
                    Luau::compileOrThrow(bcb, parseResult, parseNameTable, compileOptions);
                    bytecode = bcb.getBytecode();
                }
                catch (const Luau::CompileError&)
                {
                    // not all valid ASTs can be compiled due to limits on number of registers
                }
            }
        }
    }

    // run codegen on resulting bytecode (in separate state)
    if (kFuzzCodegenAssembly && bytecode.size())
    {
        static lua_State* globalState = luaL_newstate();

        if (luau_load(globalState, "=fuzz", bytecode.data(), bytecode.size(), 0) == 0)
        {
            Luau::CodeGen::AssemblyOptions options;
            options.compilationOptions.flags = Luau::CodeGen::CodeGen_ColdFunctions;
            options.outputBinary = true;
            options.target = kFuzzCodegenTarget;
            Luau::CodeGen::getAssembly(globalState, -1, options);
        }

        lua_pop(globalState, 1);
        lua_gc(globalState, LUA_GCCOLLECT, 0);
    }

    // run resulting bytecode (from last successfully compiler module)
    if ((kFuzzVM || kFuzzCodegenVM) && bytecode.size())
    {
        static lua_State* globalState = createGlobalState();

        auto runCode = [](const std::string& bytecode, bool useCodegen)
        {
            lua_State* L = lua_newthread(globalState);
            luaL_sandboxthread(L);

            if (luau_load(L, "=fuzz", bytecode.data(), bytecode.size(), 0) == 0)
            {
                if (useCodegen)
                    Luau::CodeGen::compile(L, -1, Luau::CodeGen::CodeGen_ColdFunctions);

                interruptDeadline = std::chrono::system_clock::now() + kInterruptTimeout;

                lua_resume(L, NULL, 0);
            }

            lua_pop(globalState, 1);

            // we'd expect full GC to reclaim all memory allocated by the script
            lua_gc(globalState, LUA_GCCOLLECT, 0);
            LUAU_ASSERT(heapSize < 256 * 1024);
        };

        if (kFuzzVM)
            runCode(bytecode, false);

        if (kFuzzCodegenVM && Luau::CodeGen::isSupported())
            runCode(bytecode, true);
    }
}
