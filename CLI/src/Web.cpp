// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lua.h"
#include "lualib.h"
#include "luacode.h"

#include "Luau/Common.h"
#include "Luau/Frontend.h"
#include "Luau/BuiltinDefinitions.h"

#include <string>
#include <memory>

#include <string.h>

// Simple FileResolver for type checking on luau.org/demo
struct DemoFileResolver
    : Luau::FileResolver
{
    DemoFileResolver()
        : Luau::FileResolver(nullptr)
    {
    }

    std::optional<Luau::SourceCode> readSource(const Luau::ModuleName& name)
    {
        auto it = source.find(name);
        if (it == source.end())
            return std::nullopt;

        Luau::SourceCode::Type sourceType = Luau::SourceCode::Module;
        auto it2 = sourceTypes.find(name);
        if (it2 != sourceTypes.end())
            sourceType = it2->second;

        return Luau::SourceCode{it->second, sourceType};
    }

    std::optional<Luau::ModuleInfo> resolveModuleInfo(
        const Luau::ModuleName& currentModuleName, const Luau::AstExpr& pathExpr)
    {
        return std::nullopt;
    }

    const Luau::ModulePtr getModule(const Luau::ModuleName& moduleName) const
    {
        return nullptr;
    }

    bool moduleExists(const Luau::ModuleName& moduleName) const
    {
        return false;
    }

    std::optional<Luau::ModuleInfo> resolveModule(const Luau::ModuleInfo* context, Luau::AstExpr* expr)
    {
        return std::nullopt;
    }

    std::string getHumanReadableModuleName(const Luau::ModuleName& name) const
    {
        return name;
    }

    std::optional<std::string> getEnvironmentForModule(const Luau::ModuleName& name) const
    {
        return std::nullopt;
    }

    std::unordered_map<Luau::ModuleName, std::string> source;
    std::unordered_map<Luau::ModuleName, Luau::SourceCode::Type> sourceTypes;
};

static void setupState(lua_State* L)
{
    luaL_openlibs(L);

    luaL_sandbox(L);
}

static std::string runCode(lua_State* L, const std::string& source)
{
    size_t bytecodeSize = 0;
    char* bytecode = luau_compile(source.data(), source.length(), nullptr, &bytecodeSize);
    int result = luau_load(L, "=stdin", bytecode, bytecodeSize, 0);
    free(bytecode);

    if (result != 0)
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

        lua_pop(L, 1); // pop T
        return std::string();
    }
    else
    {
        std::string error;

        lua_Debug ar;
        if (lua_getinfo(L, 0, "sln", &ar))
        {
            error += ar.short_src;
            error += ':';
            error += std::to_string(ar.currentline);
            error += ": ";
        }

        if (status == LUA_YIELD)
        {
            error += "thread yielded unexpectedly";
        }
        else if (const char* str = lua_tostring(T, -1))
        {
            error += str;
        }

        error += "\nstack backtrace:\n";
        error += lua_debugtrace(T);

        lua_pop(L, 1); // pop T
        return error;
    }
}

extern "C" const char* checkScript(const char* source)
{
    static std::string result;
    result.clear();

    try
    {
        DemoFileResolver fileResolver;
        Luau::NullConfigResolver configResolver;
        Luau::FrontendOptions options;

        Luau::Frontend frontend(&fileResolver, &configResolver, options);
        // Add Luau builtins
        Luau::unfreeze(frontend.globals.globalTypes);
        Luau::registerBuiltinGlobals(frontend, frontend.globals);
        Luau::freeze(frontend.globals.globalTypes);

        fileResolver.source["main"] = source;

        Luau::CheckResult checkResult = frontend.check("main");
        for (const Luau::TypeError& err : checkResult.errors)
        {
            if (!result.empty())
                result += "\n";
            result += std::to_string(err.location.begin.line + 1);
            result += ": ";
            result += Luau::toString(err);
        }
    }
    catch (const std::exception& e)
    {
        result = e.what();
    }

    return result.empty() ? nullptr : result.c_str();
}

extern "C" const char* executeScript(const char* source)
{
    // setup flags
    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = true;

    // create new state
    std::unique_ptr<lua_State, void (*)(lua_State*)> globalState(luaL_newstate(), lua_close);
    lua_State* L = globalState.get();

    // setup state
    setupState(L);

    // sandbox thread
    luaL_sandboxthread(L);

    // static string for caching result (prevents dangling ptr on function exit)
    static std::string result;

    // run code + collect error
    result = runCode(L, source);

    return result.empty() ? NULL : result.c_str();
}
