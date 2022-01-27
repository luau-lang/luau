// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lua.h"
#include "lualib.h"

#include "Luau/Compiler.h"
#include "Luau/BytecodeBuilder.h"
#include "Luau/Parser.h"

#include "FileUtils.h"
#include "Profiler.h"
#include "Coverage.h"

#include "linenoise.hpp"

#include <memory>

#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#endif

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
    Binary
};

struct GlobalOptions
{
    int optimizationLevel = 1;
} globalOptions;

static Luau::CompileOptions copts()
{
    Luau::CompileOptions result = {};
    result.optimizationLevel = globalOptions.optimizationLevel;
    result.debugLevel = 1;
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
    lua_insert(L, -2); /* put before error message */
    return 2;          /* return nil plus error message */
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
        return finishrequire(L);
    lua_pop(L, 1);

    std::optional<std::string> source = readFile(name + ".luau");
    if (!source)
    {
        source = readFile(name + ".lua"); // try .lua if .luau doesn't exist
        if (!source)
            luaL_argerrorL(L, 1, ("error loading " + name).c_str()); // if neither .luau nor .lua exist, we have an error
    }

    // module needs to run in a new thread, isolated from the rest
    lua_State* GL = lua_mainthread(L);
    lua_State* ML = lua_newthread(GL);
    lua_xmove(GL, L, 1);

    // new thread needs to have the globals sandboxed
    luaL_sandboxthread(ML);

    // now we can compile & run module on the new thread
    std::string bytecode = Luau::compile(*source, copts());
    if (luau_load(ML, chunkname.c_str(), bytecode.data(), bytecode.size(), 0) == 0)
    {
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

    // there's now a return value on top of ML; stack of L is MODULES thread
    lua_xmove(ML, L, 1);
    lua_pushvalue(L, -1);
    lua_setfield(L, -4, name.c_str());

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

void setupState(lua_State* L)
{
    luaL_openlibs(L);

    static const luaL_Reg funcs[] = {
        {"loadstring", lua_loadstring},
        {"require", lua_require},
        {"collectgarbage", lua_collectgarbage},
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

static void completeIndexer(lua_State* L, const char* editBuffer, size_t start, std::vector<std::string>& completions)
{
    std::string_view lookup = editBuffer + start;
    char lastSep = 0;

    for (;;)
    {
        size_t sep = lookup.find_first_of(".:");
        std::string_view prefix = lookup.substr(0, sep);

        if (sep == std::string_view::npos)
        {
            // table, key
            lua_pushnil(L);

            while (lua_next(L, -2) != 0)
            {
                if (lua_type(L, -2) == LUA_TSTRING)
                {
                    // table, key, value
                    std::string_view key = lua_tostring(L, -2);
                    int valueType = lua_type(L, -1);

                    // If the last separator was a ':' (i.e. a method call) then only functions should be completed.
                    bool requiredValueType = (lastSep != ':' || valueType == LUA_TFUNCTION);

                    if (!key.empty() && requiredValueType && Luau::startsWith(key, prefix))
                    {
                        std::string completion(editBuffer + std::string(key.substr(prefix.size())));
                        if (valueType == LUA_TFUNCTION)
                        {
                            // Add an opening paren for function calls by default.
                            completion += "(";
                        }
                        completions.push_back(completion);
                    }
                }
                lua_pop(L, 1);
            }

            break;
        }
        else
        {
            // find the key in the table
            lua_pushlstring(L, prefix.data(), prefix.size());
            lua_rawget(L, -2);
            lua_remove(L, -2);

            if (lua_type(L, -1) == LUA_TSTRING)
            {
                // Replace the string object with the string class to perform further lookups of string functions
                // Note: We retrieve the string class from _G to prevent issues if the user assigns to `string`.
                lua_getglobal(L, "_G");
                lua_pushlstring(L, "string", 6);
                lua_rawget(L, -2);
                lua_remove(L, -2);
                LUAU_ASSERT(lua_istable(L, -1));
            }
            else if (!lua_istable(L, -1))
                break;

            lastSep = lookup[sep];
            lookup.remove_prefix(sep + 1);
        }
    }

    lua_pop(L, 1);
}

static void completeRepl(lua_State* L, const char* editBuffer, std::vector<std::string>& completions)
{
    size_t start = strlen(editBuffer);
    while (start > 0 && (isalnum(editBuffer[start - 1]) || editBuffer[start - 1] == '.' || editBuffer[start - 1] == ':' || editBuffer[start - 1] == '_'))
        start--;

    // look the value up in current global table first
    lua_pushvalue(L, LUA_GLOBALSINDEX);
    completeIndexer(L, editBuffer, start, completions);

    // and in actual global table after that
    lua_getglobal(L, "_G");
    completeIndexer(L, editBuffer, start, completions);
}

struct LinenoiseScopedHistory
{
    LinenoiseScopedHistory()
    {
        const std::string name(".luau_history");

        if (const char* home = getenv("HOME"))
        {
            historyFilepath = joinPaths(home, name);
        }
        else if (const char* userProfile = getenv("USERPROFILE"))
        {
            historyFilepath = joinPaths(userProfile, name);
        }

        if (!historyFilepath.empty())
            linenoise::LoadHistory(historyFilepath.c_str());
    }

    ~LinenoiseScopedHistory()
    {
        if (!historyFilepath.empty())
            linenoise::SaveHistory(historyFilepath.c_str());
    }

    std::string historyFilepath;
};

static void runReplImpl(lua_State* L)
{
    linenoise::SetCompletionCallback([L](const char* editBuffer, std::vector<std::string>& completions) {
        completeRepl(L, editBuffer, completions);
    });

    std::string buffer;
    LinenoiseScopedHistory scopedHistory;

    for (;;)
    {
        bool quit = false;
        std::string line = linenoise::Readline(buffer.empty() ? "> " : ">> ", quit);
        if (quit)
            break;

        if (buffer.empty() && runCode(L, std::string("return ") + line) == std::string())
        {
            linenoise::AddHistory(line.c_str());
            continue;
        }

        buffer += line;
        buffer += " "; // linenoise doesn't work very well with multiline history entries

        std::string error = runCode(L, buffer);

        if (error.length() >= 5 && error.compare(error.length() - 5, 5, "<eof>") == 0)
        {
            continue;
        }

        if (error.length())
        {
            fprintf(stdout, "%s\n", error.c_str());
        }

        linenoise::AddHistory(buffer.c_str());
        buffer.clear();
    }
}

static void runRepl()
{
    std::unique_ptr<lua_State, void (*)(lua_State*)> globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    setupState(L);
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

static bool compileFile(const char* name, CompileFormat format)
{
    std::optional<std::string> source = readFile(name);
    if (!source)
    {
        fprintf(stderr, "Error opening %s\n", name);
        return false;
    }

    try
    {
        Luau::BytecodeBuilder bcb;

        if (format == CompileFormat::Text)
        {
            bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Source);
            bcb.setDumpSource(*source);
        }

        Luau::compileOrThrow(bcb, *source, copts());

        switch (format)
        {
        case CompileFormat::Text:
            printf("%s", bcb.dumpEverything().c_str());
            break;
        case CompileFormat::Binary:
            fwrite(bcb.getBytecode().data(), 1, bcb.getBytecode().size(), stdout);
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
    printf("  --compile[=format]: compile input files and output resulting formatted bytecode (binary or text)\n");
    printf("\n");
    printf("Available options:\n");
    printf("  --coverage: collect code coverage while running the code and output results to coverage.out\n");
    printf("  -h, --help: Display this usage message.\n");
    printf("  -i, --interactive: Run an interactive REPL after executing the last script specified.\n");
    printf("  -O<n>: use compiler optimization level (n=0-2).\n");
    printf("  --profile[=N]: profile the code using N Hz sampling (default 10000) and output results to profile.out\n");
    printf("  --timetrace: record compiler time tracing information into trace.json\n");
}

static int assertionHandler(const char* expr, const char* file, int line, const char* function)
{
    printf("%s(%d): ASSERTION FAILED: %s\n", file, line, expr);
    return 1;
}

int replMain(int argc, char** argv)
{
    Luau::assertHandler() = assertionHandler;

    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = true;

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
        else if (strcmp(argv[i], "--profile") == 0)
        {
            profile = 10000; // default to 10 KHz
        }
        else if (strncmp(argv[i], "--profile=", 10) == 0)
        {
            profile = atoi(argv[i] + 10);
        }
        else if (strcmp(argv[i], "--coverage") == 0)
        {
            coverage = true;
        }
        else if (strcmp(argv[i], "--timetrace") == 0)
        {
            FFlag::DebugLuauTimeTracing.value = true;

#if !defined(LUAU_ENABLE_TIME_TRACE)
            printf("To run with --timetrace, Luau has to be built with LUAU_ENABLE_TIME_TRACE enabled\n");
            return 1;
#endif
        }
        else if (argv[i][0] == '-')
        {
            fprintf(stderr, "Error: Unrecognized option '%s'.\n\n", argv[i]);
            displayHelp(argv[0]);
            return 1;
        }
    }

    const std::vector<std::string> files = getSourceFiles(argc, argv);
    if (mode == CliMode::Unknown)
    {
        mode = files.empty() ? CliMode::Repl : CliMode::RunSourceFiles;
    }

    switch (mode)
    {
    case CliMode::Compile:
    {
#ifdef _WIN32
        if (compileFormat == CompileFormat::Binary)
            _setmode(_fileno(stdout), _O_BINARY);
#endif

        int failed = 0;

        for (const std::string& path : files)
            failed += !compileFile(path.c_str(), compileFormat);

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
