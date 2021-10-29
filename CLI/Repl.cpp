// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lua.h"
#include "lualib.h"

#include "Luau/Compiler.h"
#include "Luau/BytecodeBuilder.h"
#include "Luau/Parser.h"

#include "FileUtils.h"
#include "Profiler.h"

#include "linenoise.hpp"

#include <memory>

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

    std::optional<std::string> source = readFile(name + ".lua");
    if (!source)
        luaL_argerrorL(L, 1, ("error loading " + name).c_str());

    // module needs to run in a new thread, isolated from the rest
    lua_State* GL = lua_mainthread(L);
    lua_State* ML = lua_newthread(GL);
    lua_xmove(GL, L, 1);

    // new thread needs to have the globals sandboxed
    luaL_sandboxthread(ML);

    // now we can compile & run module on the new thread
    std::string bytecode = Luau::compile(*source);
    if (luau_load(ML, chunkname.c_str(), bytecode.data(), bytecode.size()) == 0)
    {
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

static void setupState(lua_State* L)
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

static std::string runCode(lua_State* L, const std::string& source)
{
    std::string bytecode = Luau::compile(source);

    if (luau_load(L, "=stdin", bytecode.data(), bytecode.size()) != 0)
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
            lua_getglobal(T, "print");
            lua_insert(T, 1);
            lua_pcall(T, n, 0, 0);
        }
    }
    else
    {
        std::string error = (status == LUA_YIELD) ? "thread yielded unexpectedly" : lua_tostring(T, -1);
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

    for (;;)
    {
        size_t dot = lookup.find('.');
        std::string_view prefix = lookup.substr(0, dot);

        if (dot == std::string_view::npos)
        {
            // table, key
            lua_pushnil(L);

            while (lua_next(L, -2) != 0)
            {
                // table, key, value
                std::string_view key = lua_tostring(L, -2);

                if (!key.empty() && Luau::startsWith(key, prefix))
                    completions.push_back(editBuffer + std::string(key.substr(prefix.size())));

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

            if (lua_isnil(L, -1))
                break;

            lookup.remove_prefix(dot + 1);
        }
    }

    lua_pop(L, 1);
}

static void completeRepl(lua_State* L, const char* editBuffer, std::vector<std::string>& completions)
{
    size_t start = strlen(editBuffer);
    while (start > 0 && (isalnum(editBuffer[start - 1]) || editBuffer[start - 1] == '.'))
        start--;

    // look the value up in current global table first
    lua_pushvalue(L, LUA_GLOBALSINDEX);
    completeIndexer(L, editBuffer, start, completions);

    // and in actual global table after that
    lua_getglobal(L, "_G");
    completeIndexer(L, editBuffer, start, completions);
}

static void runRepl()
{
    std::unique_ptr<lua_State, void (*)(lua_State*)> globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    setupState(L);

    luaL_sandboxthread(L);

    linenoise::SetCompletionCallback([L](const char* editBuffer, std::vector<std::string>& completions) {
        completeRepl(L, editBuffer, completions);
    });

    std::string buffer;

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

static bool runFile(const char* name, lua_State* GL)
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

    std::string bytecode = Luau::compile(*source);
    int status = 0;

    if (luau_load(L, chunkname.c_str(), bytecode.data(), bytecode.size()) == 0)
    {
        status = lua_resume(L, NULL, 0);
    }
    else
    {
        status = LUA_ERRSYNTAX;
    }

    if (status == 0)
    {
        return true;
    }
    else
    {
        std::string error = (status == LUA_YIELD) ? "thread yielded unexpectedly" : lua_tostring(L, -1);
        error += "\nstacktrace:\n";
        error += lua_debugtrace(L);

        fprintf(stderr, "%s", error.c_str());
        return false;
    }
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

static bool compileFile(const char* name)
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
        bcb.setDumpFlags(Luau::BytecodeBuilder::Dump_Code | Luau::BytecodeBuilder::Dump_Source);
        bcb.setDumpSource(*source);

        Luau::compileOrThrow(bcb, *source);

        printf("%s", bcb.dumpEverything().c_str());

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
    printf("  --compile: compile input files and output resulting bytecode\n");
    printf("\n");
    printf("Available options:\n");
    printf("  --profile[=N]: profile the code using N Hz sampling (default 10000) and output results to profile.out\n");
}

static int assertionHandler(const char* expr, const char* file, int line)
{
    printf("%s(%d): ASSERTION FAILED: %s\n", file, line, expr);
    return 1;
}

int main(int argc, char** argv)
{
    Luau::assertHandler() = assertionHandler;

    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = true;

    if (argc == 1)
    {
        runRepl();
        return 0;
    }

    if (argc >= 2 && strcmp(argv[1], "--help") == 0)
    {
        displayHelp(argv[0]);
        return 0;
    }

    if (argc >= 2 && strcmp(argv[1], "--compile") == 0)
    {
        int failed = 0;

        for (int i = 2; i < argc; ++i)
        {
            if (argv[i][0] == '-')
                continue;

            if (isDirectory(argv[i]))
            {
                traverseDirectory(argv[i], [&](const std::string& name) {
                    if (name.length() > 4 && name.rfind(".lua") == name.length() - 4)
                        failed += !compileFile(name.c_str());
                });
            }
            else
            {
                failed += !compileFile(argv[i]);
            }
        }

        return failed;
    }

    {
        std::unique_ptr<lua_State, void (*)(lua_State*)> globalState(luaL_newstate(), lua_close);
        lua_State* L = globalState.get();

        setupState(L);

        int profile = 0;

        for (int i = 1; i < argc; ++i)
            if (strcmp(argv[i], "--profile") == 0)
                profile = 10000; // default to 10 KHz
            else if (strncmp(argv[i], "--profile=", 10) == 0)
                profile = atoi(argv[i] + 10);

        if (profile)
            profilerStart(L, profile);

        int failed = 0;

        for (int i = 1; i < argc; ++i)
        {
            if (argv[i][0] == '-')
                continue;

            if (isDirectory(argv[i]))
            {
                traverseDirectory(argv[i], [&](const std::string& name) {
                    if (name.length() > 4 && name.rfind(".lua") == name.length() - 4)
                        failed += !runFile(name.c_str(), L);
                });
            }
            else
            {
                failed += !runFile(argv[i], L);
            }
        }

        if (profile)
        {
            profilerStop();
            profilerDump("profile.out");
        }

        return failed;
    }
}


