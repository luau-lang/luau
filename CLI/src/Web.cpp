// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lua.h"
#include "lualib.h"
#include "luacode.h"

#include "Luau/Common.h"

// Analysis files
#include "Luau/Frontend.h"
#include "Luau/FileResolver.h"
#include <optional>

#include <string>

#include <string.h>

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

// Analysis
namespace Luau
{
static std::vector<std::string_view> parsePathExpr(const AstExpr& pathExpr)
{
    const AstExprIndexName* indexName = pathExpr.as<AstExprIndexName>();
    if (!indexName)
        return {};

    std::vector<std::string_view> segments{indexName->index.value};

    while (true)
    {
        if (AstExprIndexName* in = indexName->expr->as<AstExprIndexName>())
        {
            segments.push_back(in->index.value);
            indexName = in;
            continue;
        }
        else if (AstExprGlobal* indexNameAsGlobal = indexName->expr->as<AstExprGlobal>())
        {
            segments.push_back(indexNameAsGlobal->name.value);
            break;
        }
        else if (AstExprLocal* indexNameAsLocal = indexName->expr->as<AstExprLocal>())
        {
            segments.push_back(indexNameAsLocal->local->name.value);
            break;
        }
        else
            return {};
    }

    std::reverse(segments.begin(), segments.end());
    return segments;
}


std::optional<std::string> pathExprToModuleName(const ModuleName& currentModuleName, const std::vector<std::string_view>& segments)
{
    if (segments.empty())
        return std::nullopt;

    std::vector<std::string_view> result;

    auto it = segments.begin();

    if (*it == "script" && !currentModuleName.empty())
    {
        result = split(currentModuleName, '/');
        ++it;
    }

    for (; it != segments.end(); ++it)
    {
        if (result.size() > 1 && *it == "Parent")
            result.pop_back();
        else
            result.push_back(*it);
    }

    return join(result, "/");
}

std::optional<std::string> pathExprToModuleName(const ModuleName& currentModuleName, const AstExpr& pathExpr)
{
    std::vector<std::string_view> segments = parsePathExpr(pathExpr);
    return pathExprToModuleName(currentModuleName, segments);
}

struct TestFileResolver
    : FileResolver
    , ModuleResolver
{
    std::optional<ModuleInfo> resolveModuleInfo(const ModuleName& currentModuleName, const AstExpr& pathExpr) override;

    const ModulePtr getModule(const ModuleName& moduleName) const override;

    bool moduleExists(const ModuleName& moduleName) const override;

    std::optional<SourceCode> readSource(const ModuleName& name) override;

    std::optional<ModuleInfo> resolveModule(const ModuleInfo* context, AstExpr* expr) override;

    std::string getHumanReadableModuleName(const ModuleName& name) const override;

    std::optional<std::string> getEnvironmentForModule(const ModuleName& name) const override;

    std::unordered_map<ModuleName, std::string> source;
    std::unordered_map<ModuleName, SourceCode::Type> sourceTypes;
    std::unordered_map<ModuleName, std::string> environments;
};

struct TestConfigResolver : ConfigResolver
{
    Config defaultConfig;
    std::unordered_map<ModuleName, Config> configFiles;

    const Config& getConfig(const ModuleName& name) const override;
};

std::optional<ModuleInfo> TestFileResolver::resolveModuleInfo(const ModuleName& currentModuleName, const AstExpr& pathExpr)
{
    if (auto name = pathExprToModuleName(currentModuleName, pathExpr))
        return {{*name, false}};

    return std::nullopt;
}

const ModulePtr TestFileResolver::getModule(const ModuleName& moduleName) const
{
    LUAU_ASSERT(false);
    return nullptr;
}

bool TestFileResolver::moduleExists(const ModuleName& moduleName) const
{
    auto it = source.find(moduleName);
    return (it != source.end());
}

std::optional<SourceCode> TestFileResolver::readSource(const ModuleName& name)
{
    auto it = source.find(name);
    if (it == source.end())
        return std::nullopt;

    SourceCode::Type sourceType = SourceCode::Module;

    auto it2 = sourceTypes.find(name);
    if (it2 != sourceTypes.end())
        sourceType = it2->second;

    return SourceCode{it->second, sourceType};
}

std::optional<ModuleInfo> TestFileResolver::resolveModule(const ModuleInfo* context, AstExpr* expr)
{
    if (AstExprGlobal* g = expr->as<AstExprGlobal>())
    {
        if (g->name == "game")
            return ModuleInfo{"game"};
        if (g->name == "workspace")
            return ModuleInfo{"workspace"};
        if (g->name == "script")
            return context ? std::optional<ModuleInfo>(*context) : std::nullopt;
    }
    else if (AstExprIndexName* i = expr->as<AstExprIndexName>(); i && context)
    {
        if (i->index == "Parent")
        {
            std::string_view view = context->name;
            size_t lastSeparatorIndex = view.find_last_of('/');

            if (lastSeparatorIndex == std::string_view::npos)
                return std::nullopt;

            return ModuleInfo{ModuleName(view.substr(0, lastSeparatorIndex)), context->optional};
        }
        else
        {
            return ModuleInfo{context->name + '/' + i->index.value, context->optional};
        }
    }
    else if (AstExprIndexExpr* i = expr->as<AstExprIndexExpr>(); i && context)
    {
        if (AstExprConstantString* index = i->index->as<AstExprConstantString>())
        {
            return ModuleInfo{context->name + '/' + std::string(index->value.data, index->value.size), context->optional};
        }
    }
    else if (AstExprCall* call = expr->as<AstExprCall>(); call && call->self && call->args.size >= 1 && context)
    {
        if (AstExprConstantString* index = call->args.data[0]->as<AstExprConstantString>())
        {
            AstName func = call->func->as<AstExprIndexName>()->index;

            if (func == "GetService" && context->name == "game")
                return ModuleInfo{"game/" + std::string(index->value.data, index->value.size)};
        }
    }

    return std::nullopt;
}

std::string TestFileResolver::getHumanReadableModuleName(const ModuleName& name) const
{
    // We have a handful of tests that need to distinguish between a canonical
    // ModuleName and the human-readable version so we apply a simple transform
    // here:  We replace all slashes with dots.
    std::string result = name;
    for (size_t i = 0; i < result.size(); ++i)
    {
        if (result[i] == '/')
            result[i] = '.';
    }

    return result;
}

std::optional<std::string> TestFileResolver::getEnvironmentForModule(const ModuleName& name) const
{
    auto it = environments.find(name);
    if (it != environments.end())
        return it->second;

    return std::nullopt;
}

const Config& TestConfigResolver::getConfig(const ModuleName& name) const
{
    auto it = configFiles.find(name);
    if (it != configFiles.end())
        return it->second;

    return defaultConfig;
}

TestFileResolver fileResolver;
TestConfigResolver configResolver;

CheckResult frontendCheck(Mode mode, const std::string& source, std::optional<FrontendOptions> options)
{
    Luau::Frontend frontend(&fileResolver, &configResolver);

    ModuleName mm = "web";
    configResolver.defaultConfig.mode = mode;
    fileResolver.source[mm] = std::move(source);
    frontend.markDirty(mm);

    CheckResult result = frontend.check(mm, options);

    return result;
}

std::string runAnalysis(const std::string& source)
{
    std::string strResult;

    CheckResult checkResult = frontendCheck(Mode::Strict, source, std::nullopt);

    // Collect errors
    for (auto error : checkResult.errors)
    {
        strResult += toString(error) += "\n";
    }

    return strResult;
}

}; // namespace Luau

extern "C" const char* executeAnalysis(const char* source)
{
    // setup flags
    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = true;

    std::string result;

    // run Analysis
    result = Luau::runAnalysis(source);

    return result.empty() ? NULL : result.c_str();
}