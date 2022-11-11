// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Repl.h"

#include "lua.h"
#include "lualib.h"

#include "Luau/CodeGen.h"
#include "Luau/Compiler.h"
#include "Luau/BytecodeBuilder.h"
#include "Luau/Parser.h"

#include "Coverage.h"
#include "FileUtils.h"
#include "Flags.h"
#include "Profiler.h"

#include "isocline.h"

#include <memory>

#ifdef _WIN32
#include <io.h>
#include <fcntl.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#ifdef CALLGRIND
#include <valgrind/callgrind.h>
#endif

#include <locale.h>
#include <signal.h>

LUAU_FASTFLAG(DebugLuauTimeTracing)

enum class CliMode
{
    Unknown,
    Repl,
    Compile,
    RunSourceFiles
};

enum class CompileFormat
{
    Text,
    Binary,
    Remarks,
    Codegen,
    CodegenVerbose,
    CodegenNull,
    Null
};

constexpr int MaxTraversalLimit = 50;

static bool codegen = false;

// Ctrl-C handling
static void sigintCallback(lua_State* L, int gc)
{
    if (gc >= 0)
        return;

    lua_callbacks(L)->interrupt = NULL;

    lua_rawcheckstack(L, 1); // reserve space for error string
    luaL_error(L, "Execution interrupted");
}

static lua_State* replState = NULL;

#ifdef _WIN32
BOOL WINAPI sigintHandler(DWORD signal)
{
    if (signal == CTRL_C_EVENT && replState)
        lua_callbacks(replState)->interrupt = &sigintCallback;
    return TRUE;
}
#else
static void sigintHandler(int signum)
{
    if (signum == SIGINT && replState)
        lua_callbacks(replState)->interrupt = &sigintCallback;
}
#endif

struct GlobalOptions
{
    int optimizationLevel = 1;
    int debugLevel = 1;
} globalOptions;

static Luau::CompileOptions copts()
{
    Luau::CompileOptions result = {};
    result.optimizationLevel = globalOptions.optimizationLevel;
    result.debugLevel = globalOptions.debugLevel;
    result.coverageLevel = coverageActive() ? 2 : 0;

    return result;
}

static int lua_loadstring(lua_State* L)
{
    size_t l = 0;
    const char* s = luaL_checklstring(L, 1, &l);
    const char* chunkname = luaL_optstring(L, 2, s);

    lua_setsafeenv(L, LUA_ENVIRONINDEX, false);

    std::string bytecode = Luau::compile(std::string(s, l), copts());
    if (luau_load(L, chunkname, bytecode.data(), bytecode.size(), 0) == 0)
        return 1;

    lua_pushnil(L);
    lua_insert(L, -2); // put before error message
    return 2;          // return nil plus error message
}

static int finishrequire(lua_State* L)
{
    if (lua_isstring(L, -1))
        lua_error(L);

    return 1;
}

static int lua_require(lua_State* L)
{
    std::string name = luaL_checkstring(L, 1);
    std::string chunkname = "=" + name;

    luaL_findtable(L, LUA_REGISTRYINDEX, "_MODULES", 1);

    // return the module from the cache
    lua_getfield(L, -1, name.c_str());
    if (!lua_isnil(L, -1))
    {
        // L stack: _MODULES result
        return finishrequire(L);
    }

    lua_pop(L, 1);

    std::optional<std::string> source = readFile(name + ".luau");
    if (!source)
    {
        source = readFile(name + ".lua"); // try .lua if .luau doesn't exist
        if (!source)
            luaL_argerrorL(L, 1, ("error loading " + name).c_str()); // if neither .luau nor .lua exist, we have an error
    }

    // module needs to run in a new thread, isolated from the rest
    // note: we create ML on main thread so that it doesn't inherit environment of L
    lua_State* GL = lua_mainthread(L);
    lua_State* ML = lua_newthread(GL);
    lua_xmove(GL, L, 1);

    // new thread needs to have the globals sandboxed
    luaL_sandboxthread(ML);

    // now we can compile & run module on the new thread
    std::string bytecode = Luau::compile(*source, copts());
    if (luau_load(ML, chunkname.c_str(), bytecode.data(), bytecode.size(), 0) == 0)
    {
        if (codegen)
            Luau::CodeGen::compile(ML, -1);

        if (coverageActive())
            coverageTrack(ML, -1);

        int status = lua_resume(ML, L, 0);

        if (status == 0)
        {
            if (lua_gettop(ML) == 0)
                lua_pushstring(ML, "module must return a value");
            else if (!lua_istable(ML, -1) && !lua_isfunction(ML, -1))
                lua_pushstring(ML, "module must return a table or function");
        }
        else if (status == LUA_YIELD)
        {
            lua_pushstring(ML, "module can not yield");
        }
        else if (!lua_isstring(ML, -1))
        {
            lua_pushstring(ML, "unknown error while running module");
        }
    }

    // there's now a return value on top of ML; L stack: _MODULES ML
    lua_xmove(ML, L, 1);
    lua_pushvalue(L, -1);
    lua_setfield(L, -4, name.c_str());

    // L stack: _MODULES ML result
    return finishrequire(L);
}

static int lua_collectgarbage(lua_State* L)
{
    const char* option = luaL_optstring(L, 1, "collect");

    if (strcmp(option, "collect") == 0)
    {
        lua_gc(L, LUA_GCCOLLECT, 0);
        return 0;
    }

    if (strcmp(option, "count") == 0)
    {
        int c = lua_gc(L, LUA_GCCOUNT, 0);
        lua_pushnumber(L, c);
        return 1;
    }

    luaL_error(L, "collectgarbage must be called with 'count' or 'collect'");
}

#ifdef CALLGRIND
static int lua_callgrind(lua_State* L)
{
    const char* option = luaL_checkstring(L, 1);

    if (strcmp(option, "running") == 0)
    {
        int r = RUNNING_ON_VALGRIND;
        lua_pushboolean(L, r);
        return 1;
    }

    if (strcmp(option, "zero") == 0)
    {
        CALLGRIND_ZERO_STATS;
        return 0;
    }

    if (strcmp(option, "dump") == 0)
    {
        const char* name = luaL_checkstring(L, 2);

        CALLGRIND_DUMP_STATS_AT(name);
        return 0;
    }

    luaL_error(L, "callgrind must be called with one of 'running', 'zero', 'dump'");
}
#endif

void setupState(lua_State* L)
{
    if (codegen)
        Luau::CodeGen::create(L);

    luaL_openlibs(L);

    static const luaL_Reg funcs[] = {
        {"loadstring", lua_loadstring},
        {"require", lua_require},
        {"collectgarbage", lua_collectgarbage},
#ifdef CALLGRIND
        {"callgrind", lua_callgrind},
#endif
        {NULL, NULL},
    };

    lua_pushvalue(L, LUA_GLOBALSINDEX);
    luaL_register(L, NULL, funcs);
    lua_pop(L, 1);

    luaL_sandbox(L);
}

std::string runCode(lua_State* L, const std::string& source)
{
    std::string bytecode = Luau::compile(source, copts());

    if (luau_load(L, "=stdin", bytecode.data(), bytecode.size(), 0) != 0)
    {
        size_t len;
        const char* msg = lua_tolstring(L, -1, &len);

        std::string error(msg, len);
        lua_pop(L, 1);

        return error;
    }

    if (codegen)
        Luau::CodeGen::compile(L, -1);

    lua_State* T = lua_newthread(L);

    lua_pushvalue(L, -2);
    lua_remove(L, -3);
    lua_xmove(L, T, 1);

    int status = lua_resume(T, NULL, 0);

    if (status == 0)
    {
        int n = lua_gettop(T);

        if (n)
        {
            luaL_checkstack(T, LUA_MINSTACK, "too many results to print");
            lua_getglobal(T, "_PRETTYPRINT");
            // If _PRETTYPRINT is nil, then use the standard print function instead
            if (lua_isnil(T, -1))
            {
                lua_pop(T, 1);
                lua_getglobal(T, "print");
            }
            lua_insert(T, 1);
            lua_pcall(T, n, 0, 0);
        }
    }
    else
    {
        std::string error;

        if (status == LUA_YIELD)
        {
            error = "thread yielded unexpectedly";
        }
        else if (const char* str = lua_tostring(T, -1))
        {
            error = str;
        }

        error += "\nstack backtrace:\n";
        error += lua_debugtrace(T);

        fprintf(stdout, "%s", error.c_str());
    }

    lua_pop(L, 1);
    return std::string();
}

// Replaces the top of the lua stack with the metatable __index for the value
// if it exists.  Returns true iff __index exists.
static bool tryReplaceTopWithIndex(lua_State* L)
{
    if (luaL_getmetafield(L, -1, "__index"))
    {
        // Remove the table leaving __index on the top of stack
        lua_remove(L, -2);
        return true;
    }
    return false;
}


// This function is similar to lua_gettable, but it avoids calling any
// lua callback functions (e.g. __index) which might modify the Lua VM state.
static void safeGetTable(lua_State* L, int tableIndex)
{
    lua_pushvalue(L, tableIndex); // Duplicate the table

    // The loop invariant is that the table to search is at -1
    // and the key is at -2.
    for (int loopCount = 0;; loopCount++)
    {
        lua_pushvalue(L, -2); // Duplicate the key
        lua_rawget(L, -2);    // Try to find the key
        if (!lua_isnil(L, -1) || loopCount >= MaxTraversalLimit)
        {
            // Either the key has been found, and/or we have reached the max traversal limit
            break;
        }
        else
        {
            lua_pop(L, 1); // Pop the nil result
            if (!luaL_getmetafield(L, -1, "__index"))
            {
                lua_pushnil(L);
                break;
            }
            else if (lua_istable(L, -1))
            {
                // Replace the current table being searched with __index table
                lua_replace(L, -2);
            }
            else
            {
                lua_pop(L, 1); // Pop the value
                lua_pushnil(L);
                break;
            }
        }
    }

    lua_remove(L, -2); // Remove the table
    lua_remove(L, -2); // Remove the original key
}

// completePartialMatches finds keys that match the specified 'prefix'
// Note: the table/object to be searched must be on the top of the Lua stack
static void completePartialMatches(lua_State* L, bool completeOnlyFunctions, const std::string& editBuffer, std::string_view prefix,
    const AddCompletionCallback& addCompletionCallback)
{
    for (int i = 0; i < MaxTraversalLimit && lua_istable(L, -1); i++)
    {
        // table, key
        lua_pushnil(L);

        // Loop over all the keys in the current table
        while (lua_next(L, -2) != 0)
        {
            if (lua_type(L, -2) == LUA_TSTRING)
            {
                // table, key, value
                std::string_view key = lua_tostring(L, -2);
                int valueType = lua_type(L, -1);

                // If the last separator was a ':' (i.e. a method call) then only functions should be completed.
                bool requiredValueType = (!completeOnlyFunctions || valueType == LUA_TFUNCTION);

                if (!key.empty() && requiredValueType && Luau::startsWith(key, prefix))
                {
                    std::string completedComponent(key.substr(prefix.size()));
                    std::string completion(editBuffer + completedComponent);
                    if (valueType == LUA_TFUNCTION)
                    {
                        // Add an opening paren for function calls by default.
                        completion += "(";
                    }
                    addCompletionCallback(completion, std::string(key));
                }
            }
            lua_pop(L, 1);
        }

        // Replace the current table being searched with an __index table if one exists
        if (!tryReplaceTopWithIndex(L))
        {
            break;
        }
    }
}

static void completeIndexer(lua_State* L, const std::string& editBuffer, const AddCompletionCallback& addCompletionCallback)
{
    std::string_view lookup = editBuffer;
    bool completeOnlyFunctions = false;

    // Push the global variable table to begin the search
    lua_pushvalue(L, LUA_GLOBALSINDEX);

    for (;;)
    {
        size_t sep = lookup.find_first_of(".:");
        std::string_view prefix = lookup.substr(0, sep);

        if (sep == std::string_view::npos)
        {
            completePartialMatches(L, completeOnlyFunctions, editBuffer, prefix, addCompletionCallback);
            break;
        }
        else
        {
            // find the key in the table
            lua_pushlstring(L, prefix.data(), prefix.size());
            safeGetTable(L, -2);
            lua_remove(L, -2);

            if (lua_istable(L, -1) || tryReplaceTopWithIndex(L))
            {
                completeOnlyFunctions = lookup[sep] == ':';
                lookup.remove_prefix(sep + 1);
            }
            else
            {
                // Unable to search for keys, so stop searching
                break;
            }
        }
    }

    lua_pop(L, 1);
}

void getCompletions(lua_State* L, const std::string& editBuffer, const AddCompletionCallback& addCompletionCallback)
{
    completeIndexer(L, editBuffer, addCompletionCallback);
}

static void icGetCompletions(ic_completion_env_t* cenv, const char* editBuffer)
{
    auto* L = reinterpret_cast<lua_State*>(ic_completion_arg(cenv));

    getCompletions(L, std::string(editBuffer), [cenv](const std::string& completion, const std::string& display) {
        ic_add_completion_ex(cenv, completion.data(), display.data(), nullptr);
    });
}

static bool isMethodOrFunctionChar(const char* s, long len)
{
    char c = *s;
    return len == 1 && (isalnum(c) || c == '.' || c == ':' || c == '_');
}

static void completeRepl(ic_completion_env_t* cenv, const char* editBuffer)
{
    ic_complete_word(cenv, editBuffer, icGetCompletions, isMethodOrFunctionChar);
}

static void loadHistory(const char* name)
{
    std::string path;

    if (const char* home = getenv("HOME"))
    {
        path = joinPaths(home, name);
    }
    else if (const char* userProfile = getenv("USERPROFILE"))
    {
        path = joinPaths(userProfile, name);
    }

    if (!path.empty())
        ic_set_history(path.c_str(), -1 /* default entries (= 200) */);
}

static void runReplImpl(lua_State* L)
{
    ic_set_default_completer(completeRepl, L);

    // Reset the locale to C
    setlocale(LC_ALL, "C");

    // Make brace matching easier to see
    ic_style_def("ic-bracematch", "teal");

    // Prevent auto insertion of braces
    ic_enable_brace_insertion(false);

    // Loads history from the given file; isocline automatically saves the history on process exit
    loadHistory(".luau_history");

    std::string buffer;

    for (;;)
    {
        const char* prompt = buffer.empty() ? "" : ">";
        std::unique_ptr<char, void (*)(void*)> line(ic_readline(prompt), free);
        if (!line)
            break;

        if (buffer.empty() && runCode(L, std::string("return ") + line.get()) == std::string())
        {
            ic_history_add(line.get());
            continue;
        }

        if (!buffer.empty())
            buffer += "\n";
        buffer += line.get();

        std::string error = runCode(L, buffer);

        if (error.length() >= 5 && error.compare(error.length() - 5, 5, "<eof>") == 0)
        {
            continue;
        }

        if (error.length())
        {
            fprintf(stdout, "%s\n", error.c_str());
        }

        ic_history_add(buffer.c_str());
        buffer.clear();
    }
}

static void runRepl()
{
    std::unique_ptr<lua_State, void (*)(lua_State*)> globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    setupState(L);

    // setup Ctrl+C handling
    replState = L;
#ifdef _WIN32
    SetConsoleCtrlHandler(sigintHandler, TRUE);
#else
    signal(SIGINT, sigintHandler);
#endif

    luaL_sandboxthread(L);
    runReplImpl(L);
}

// `repl` is used it indicate if a repl should be started after executing the file.
static bool runFile(const char* name, lua_State* GL, bool repl)
{
    std::optional<std::string> source = readFile(name);
    if (!source)
    {
        fprintf(stderr, "Error opening %s\n", name);
        return false;
    }

    // module needs to run in a new thread, isolated from the rest
    lua_State* L = lua_newthread(GL);

    // new thread needs to have the globals sandboxed
    luaL_sandboxthread(L);

    std::string chunkname = "=" + std::string(name);

    std::string bytecode = Luau::compile(*source, copts());
    int status = 0;

    if (luau_load(L, chunkname.c_str(), bytecode.data(), bytecode.size(), 0) == 0)
    {
        if (codegen)
            Luau::CodeGen::compile(L, -1);

        if (coverageActive())
            coverageTrack(L, -1);

        status = lua_resume(L, NULL, 0);
    }
    else
    {
        status = LUA_ERRSYNTAX;
    }

    if (status != 0)
    {
        std::string error;

        if (status == LUA_YIELD)
        {
            error = "thread yielded unexpectedly";
        }
        else if (const char* str = lua_tostring(L, -1))
        {
            error = str;
        }

        error += "\nstacktrace:\n";
        error += lua_debugtrace(L);

        fprintf(stderr, "%s", error.c_str());
    }

    if (repl)
    {
        runReplImpl(L);
    }
    lua_pop(GL, 1);
    return status == 0;
}

static void report(const char* name, const Luau::Location& location, const char* type, const char* message)
{
    fprintf(stderr, "%s(%d,%d): %s: %s\n", name, location.begin.line + 1, location.begin.column + 1, type, message);
}

static void reportError(const char* name, const Luau::ParseError& error)
{
    report(name, error.getLocation(), "SyntaxError", error.what());
}

static void reportError(const char* name, const Luau::CompileError& error)
{
    report(name, error.getLocation(), "CompileError", error.what());
}

static std::string getCodegenAssembly(const char* name, const std::string& bytecode, Luau::CodeGen::AssemblyOptions options)
{
    std::unique_ptr<lua_State, void (*)(lua_State*)> globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    if (luau_load(L, name, bytecode.data(), bytecode.size(), 0) == 0)
        return Luau::CodeGen::getAssembly(L, -1, options);

    fprintf(stderr, "Error loading bytecode %s\n", name);
    return "";
}

static void annotateInstruction(void* context, std::string& text, int fid, int instpos)
{
    Luau::BytecodeBuilder& bcb = *(Luau::BytecodeBuilder*)context;

    bcb.annotateInstruction(text, fid, instpos);
}

struct CompileStats
{
    size_t lines;
    size_t bytecode;
    size_t codegen;
};

static bool compileFile(const char* name, CompileFormat format, CompileStats& stats)
{
    std::optional<std::string> source = readFile(name);
    if (!source)
    {
        fprintf(stderr, "Error opening %s\n", name);
        return false;
    }

    // NOTE: Normally, you should use Luau::compile or luau_compile (see lua_require as an example)
    // This function is much more complicated because it supports many output human-readable formats through internal interfaces

    try
    {
        Luau::BytecodeBuilder bcb;
        Luau::CodeGen::AssemblyOptions options = {format == CompileFormat::CodegenNull, format == CompileFormat::Codegen, annotateInstruction, &bcb};

        if (format == CompileFormat::Text)
        {
            bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Source | Luau::BytecodeBuilder::Dump_Locals |
                             Luau::BytecodeBuilder::Dump_Remarks);
            bcb.setDumpSource(*source);
        }
        else if (format == CompileFormat::Remarks)
        {
            bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Source | Luau::BytecodeBuilder::Dump_Remarks);
            bcb.setDumpSource(*source);
        }
        else if (format == CompileFormat::Codegen || format == CompileFormat::CodegenVerbose)
        {
            bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Source | Luau::BytecodeBuilder::Dump_Locals |
                             Luau::BytecodeBuilder::Dump_Remarks);
            bcb.setDumpSource(*source);
        }

        Luau::Allocator allocator;
        Luau::AstNameTable names(allocator);
        Luau::ParseResult result = Luau::Parser::parse(source->c_str(), source->size(), names, allocator);

        if (!result.errors.empty())
            throw Luau::ParseErrors(result.errors);

        stats.lines += result.lines;

        Luau::compileOrThrow(bcb, result, names, copts());
        stats.bytecode += bcb.getBytecode().size();

        switch (format)
        {
        case CompileFormat::Text:
            printf("%s", bcb.dumpEverything().c_str());
            break;
        case CompileFormat::Remarks:
            printf("%s", bcb.dumpSourceRemarks().c_str());
            break;
        case CompileFormat::Binary:
            fwrite(bcb.getBytecode().data(), 1, bcb.getBytecode().size(), stdout);
            break;
        case CompileFormat::Codegen:
        case CompileFormat::CodegenVerbose:
            printf("%s", getCodegenAssembly(name, bcb.getBytecode(), options).c_str());
            break;
        case CompileFormat::CodegenNull:
            stats.codegen += getCodegenAssembly(name, bcb.getBytecode(), options).size();
            break;
        case CompileFormat::Null:
            break;
        }

        return true;
    }
    catch (Luau::ParseErrors& e)
    {
        for (auto& error : e.getErrors())
            reportError(name, error);
        return false;
    }
    catch (Luau::CompileError& e)
    {
        reportError(name, e);
        return false;
    }
}

static void displayHelp(const char* argv0)
{
    printf("Usage: %s [--mode] [options] [file list]\n", argv0);
    printf("\n");
    printf("When mode and file list are omitted, an interactive REPL is started instead.\n");
    printf("\n");
    printf("Available modes:\n");
    printf("  omitted: compile and run input files one by one\n");
    printf("  --compile[=format]: compile input files and output resulting bytecode/assembly (binary, text, remarks, codegen)\n");
    printf("\n");
    printf("Available options:\n");
    printf("  --coverage: collect code coverage while running the code and output results to coverage.out\n");
    printf("  -h, --help: Display this usage message.\n");
    printf("  -i, --interactive: Run an interactive REPL after executing the last script specified.\n");
    printf("  -O<n>: compile with optimization level n (default 1, n should be between 0 and 2).\n");
    printf("  -g<n>: compile with debug level n (default 1, n should be between 0 and 2).\n");
    printf("  --profile[=N]: profile the code using N Hz sampling (default 10000) and output results to profile.out\n");
    printf("  --timetrace: record compiler time tracing information into trace.json\n");
    printf("  --codegen: execute code using native code generation\n");
}

static int assertionHandler(const char* expr, const char* file, int line, const char* function)
{
    printf("%s(%d): ASSERTION FAILED: %s\n", file, line, expr);
    return 1;
}

int replMain(int argc, char** argv)
{
    Luau::assertHandler() = assertionHandler;

    setLuauFlagsDefault();

    CliMode mode = CliMode::Unknown;
    CompileFormat compileFormat{};
    int profile = 0;
    bool coverage = false;
    bool interactive = false;

    // Set the mode if the user has explicitly specified one.
    int argStart = 1;
    if (argc >= 2 && strncmp(argv[1], "--compile", strlen("--compile")) == 0)
    {
        argStart++;
        mode = CliMode::Compile;
        if (strcmp(argv[1], "--compile") == 0)
        {
            compileFormat = CompileFormat::Text;
        }
        else if (strcmp(argv[1], "--compile=binary") == 0)
        {
            compileFormat = CompileFormat::Binary;
        }
        else if (strcmp(argv[1], "--compile=text") == 0)
        {
            compileFormat = CompileFormat::Text;
        }
        else if (strcmp(argv[1], "--compile=remarks") == 0)
        {
            compileFormat = CompileFormat::Remarks;
        }
        else if (strcmp(argv[1], "--compile=codegen") == 0)
        {
            compileFormat = CompileFormat::Codegen;
        }
        else if (strcmp(argv[1], "--compile=codegenverbose") == 0)
        {
            compileFormat = CompileFormat::CodegenVerbose;
        }
        else if (strcmp(argv[1], "--compile=codegennull") == 0)
        {
            compileFormat = CompileFormat::CodegenNull;
        }
        else if (strcmp(argv[1], "--compile=null") == 0)
        {
            compileFormat = CompileFormat::Null;
        }
        else
        {
            fprintf(stderr, "Error: Unrecognized value for '--compile' specified.\n");
            return 1;
        }
    }

    for (int i = argStart; i < argc; i++)
    {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0)
        {
            displayHelp(argv[0]);
            return 0;
        }
        else if (strcmp(argv[i], "-i") == 0 || strcmp(argv[i], "--interactive") == 0)
        {
            interactive = true;
        }
        else if (strncmp(argv[i], "-O", 2) == 0)
        {
            int level = atoi(argv[i] + 2);
            if (level < 0 || level > 2)
            {
                fprintf(stderr, "Error: Optimization level must be between 0 and 2 inclusive.\n");
                return 1;
            }
            globalOptions.optimizationLevel = level;
        }
        else if (strncmp(argv[i], "-g", 2) == 0)
        {
            int level = atoi(argv[i] + 2);
            if (level < 0 || level > 2)
            {
                fprintf(stderr, "Error: Debug level must be between 0 and 2 inclusive.\n");
                return 1;
            }
            globalOptions.debugLevel = level;
        }
        else if (strcmp(argv[i], "--profile") == 0)
        {
            profile = 10000; // default to 10 KHz
        }
        else if (strncmp(argv[i], "--profile=", 10) == 0)
        {
            profile = atoi(argv[i] + 10);
        }
        else if (strcmp(argv[i], "--codegen") == 0)
        {
            codegen = true;
        }
        else if (strcmp(argv[i], "--coverage") == 0)
        {
            coverage = true;
        }
        else if (strcmp(argv[i], "--timetrace") == 0)
        {
            FFlag::DebugLuauTimeTracing.value = true;
        }
        else if (strncmp(argv[i], "--fflags=", 9) == 0)
        {
            setLuauFlags(argv[i] + 9);
        }
        else if (argv[i][0] == '-')
        {
            fprintf(stderr, "Error: Unrecognized option '%s'.\n\n", argv[i]);
            displayHelp(argv[0]);
            return 1;
        }
    }

#if !defined(LUAU_ENABLE_TIME_TRACE)
    if (FFlag::DebugLuauTimeTracing)
    {
        fprintf(stderr, "To run with --timetrace, Luau has to be built with LUAU_ENABLE_TIME_TRACE enabled\n");
        return 1;
    }
#endif

#if !LUA_CUSTOM_EXECUTION
    if (codegen)
    {
        fprintf(stderr, "To run with --codegen, Luau has to be built with LUA_CUSTOM_EXECUTION enabled\n");
        return 1;
    }
#endif

    const std::vector<std::string> files = getSourceFiles(argc, argv);
    if (mode == CliMode::Unknown)
    {
        mode = files.empty() ? CliMode::Repl : CliMode::RunSourceFiles;
    }

    if (mode != CliMode::Compile && codegen && !Luau::CodeGen::isSupported())
    {
        fprintf(stderr, "Cannot enable --codegen, native code generation is not supported in current configuration\n");
        return 1;
    }

    switch (mode)
    {
    case CliMode::Compile:
    {
#ifdef _WIN32
        if (compileFormat == CompileFormat::Binary)
            _setmode(_fileno(stdout), _O_BINARY);
#endif

        CompileStats stats = {};
        int failed = 0;

        for (const std::string& path : files)
            failed += !compileFile(path.c_str(), compileFormat, stats);

        if (compileFormat == CompileFormat::Null)
            printf("Compiled %d KLOC into %d KB bytecode\n", int(stats.lines / 1000), int(stats.bytecode / 1024));
        else if (compileFormat == CompileFormat::CodegenNull)
            printf("Compiled %d KLOC into %d KB bytecode => %d KB native code\n", int(stats.lines / 1000), int(stats.bytecode / 1024),
                int(stats.codegen / 1024));

        return failed ? 1 : 0;
    }
    case CliMode::Repl:
    {
        runRepl();
        return 0;
    }
    case CliMode::RunSourceFiles:
    {
        std::unique_ptr<lua_State, void (*)(lua_State*)> globalState(luaL_newstate(), lua_close);
        lua_State* L = globalState.get();

        setupState(L);

        if (profile)
            profilerStart(L, profile);

        if (coverage)
            coverageInit(L);

        int failed = 0;

        for (size_t i = 0; i < files.size(); ++i)
        {
            bool isLastFile = i == files.size() - 1;
            failed += !runFile(files[i].c_str(), L, interactive && isLastFile);
        }

        if (profile)
        {
            profilerStop();
            profilerDump("profile.out");
        }

        if (coverage)
            coverageDump("coverage.out");

        return failed ? 1 : 0;
    }
    case CliMode::Unknown:
    default:
        LUAU_ASSERT(!"Unhandled cli mode.");
        return 1;
    }
}
