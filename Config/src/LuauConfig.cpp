// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/LuauConfig.h"

#include "Luau/Compiler.h"
#include "Luau/Config.h"

#include "lua.h"
#include "lualib.h"

#include <functional>
#include <memory>
#include <optional>
#include <string>

#define RETURN_WITH_ERROR(msg) \
    do \
    { \
        if (error) \
            *error = msg; \
        return std::nullopt; \
    } while (false)

namespace Luau
{

struct ThreadPopper
{
    explicit ThreadPopper(lua_State* L)
        : L(L)
    {
        LUAU_ASSERT(L);
    }

    ThreadPopper(const ThreadPopper&) = delete;
    ThreadPopper& operator=(const ThreadPopper&) = delete;

    ~ThreadPopper()
    {
        lua_pop(L, 1);
    }

    lua_State* L = nullptr;
};

static std::optional<ConfigTable> serializeTable(lua_State* L, std::string* error)
{
    ThreadPopper popper(L); // Remove table from stack after processing
    ConfigTable table;

    lua_pushnil(L);
    while (lua_next(L, -2) != 0)
    {
        ThreadPopper popper(L); // Remove value from stack after processing

        // Process key
        ConfigTableKey key;
        switch (lua_type(L, -2))
        {
        case LUA_TNUMBER:
            key = lua_tonumber(L, -2);
            break;
        case LUA_TSTRING:
            key = std::string{lua_tostring(L, -2)};
            break;
        default:
            RETURN_WITH_ERROR("configuration table keys must be strings or numbers");
        }

        // Process value
        switch (lua_type(L, -1))
        {
        case LUA_TNUMBER:
            table[key] = lua_tonumber(L, -1);
            break;
        case LUA_TSTRING:
            table[key] = std::string{lua_tostring(L, -1)};
            break;
        case LUA_TBOOLEAN:
            table[key] = static_cast<bool>(lua_toboolean(L, -1));
            break;
        case LUA_TTABLE:
        {
            lua_pushvalue(L, -1); // Copy table for recursive call
            if (std::optional<ConfigTable> nested = serializeTable(L, error))
                table[key] = std::move(*nested);
            else
                return std::nullopt; // Error already set in recursive call
            break;
        }
        default:
            std::string msg = "configuration value for key \"" + key.toString() + "\" must be a string, number, boolean, or nested table";
            RETURN_WITH_ERROR(std::move(msg));
        }
    }

    return table;
}

std::optional<ConfigTable> extractConfig(const std::string& source, const InterruptCallbacks& callbacks, std::string* error)
{
    // Initialize Luau VM
    std::unique_ptr<lua_State, void (*)(lua_State*)> state{luaL_newstate(), lua_close};
    lua_State* L = state.get();
    luaL_openlibs(L);
    luaL_sandbox(L);

    // Compile and load source code
    std::string bytecode = compile(source);
    if (luau_load(L, "=config", bytecode.data(), bytecode.size(), 0) != 0)
        RETURN_WITH_ERROR(lua_tostring(L, -1));

    // Execute configuration
    if (callbacks.initCallback)
        callbacks.initCallback(L);
    lua_callbacks(L)->interrupt = callbacks.interruptCallback;
    switch (lua_resume(L, nullptr, 0))
    {
    case LUA_OK:
        break;
    case LUA_BREAK: // debugging not supported, at least for now
    case LUA_YIELD:
        RETURN_WITH_ERROR("configuration execution cannot yield");
    default:
        RETURN_WITH_ERROR(lua_tostring(L, -1));
    }

    // Extract returned table
    if (lua_gettop(L) != 1)
        RETURN_WITH_ERROR("configuration must return exactly one value");

    if (lua_type(L, -1) != LUA_TTABLE)
        RETURN_WITH_ERROR("configuration did not return a table");

    return serializeTable(L, error);
}

static std::optional<std::string> createLuauConfigFromLuauTable(
    Config& config,
    const ConfigTable& luauTable,
    std::optional<ConfigOptions::AliasOptions> aliasOptions
)
{
    for (const auto& [k, v] : luauTable)
    {
        const std::string* key = k.get_if<std::string>();
        if (!key)
            return "configuration keys in \"luau\" table must be strings";

        if (*key == "languagemode")
        {
            const std::string* value = v.get_if<std::string>();
            if (!value)
                return "configuration value for key \"languagemode\" must be a string";

            if (std::optional<std::string> errorMessage = parseModeString(config.mode, *value))
                return errorMessage;
        }

        if (*key == "lint")
        {
            const ConfigTable* lint = v.get_if<ConfigTable>();
            if (!lint)
                return "configuration value for key \"lint\" must be a table";

            // Handle wildcard first to ensure overrides work as expected.
            if (const ConfigValue* value = lint->find("*"))
            {
                const bool* enabled = value->get_if<bool>();
                if (!enabled)
                    return "configuration values in \"lint\" table must be booleans";

                if (std::optional<std::string> errorMessage =
                        parseLintRuleString(config.enabledLint, config.fatalLint, "*", *enabled ? "true" : "false"))
                    return errorMessage;
            }

            for (const auto& [k, v] : *lint)
            {
                const std::string* warningName = k.get_if<std::string>();
                if (!warningName)
                    return "configuration keys in \"lint\" table must be strings";

                if (*warningName == "*")
                    continue; // Handled above

                const bool* enabled = v.get_if<bool>();
                if (!enabled)
                    return "configuration values in \"lint\" table must be booleans";

                if (std::optional<std::string> errorMessage =
                        parseLintRuleString(config.enabledLint, config.fatalLint, *warningName, *enabled ? "true" : "false"))
                    return errorMessage;
            }
        }

        if (*key == "linterrors")
        {
            const bool* value = v.get_if<bool>();
            if (!value)
                return "configuration value for key \"linterrors\" must be a boolean";

            config.lintErrors = *value;
        }

        if (*key == "typeerrors")
        {
            const bool* value = v.get_if<bool>();
            if (!value)
                return "configuration value for key \"typeerrors\" must be a boolean";

            config.typeErrors = *value;
        }

        if (*key == "globals")
        {
            const ConfigTable* globalsTable = v.get_if<ConfigTable>();
            if (!globalsTable)
                return "configuration value for key \"globals\" must be an array of strings";

            std::vector<std::string> globals;
            globals.resize(globalsTable->size());

            for (const auto& [k, v] : *globalsTable)
            {
                const double* key = k.get_if<double>();
                if (!key)
                    return "configuration array \"globals\" must only have numeric keys";

                const size_t index = static_cast<size_t>(*key);
                if (index < 1 || globalsTable->size() < index)
                    return "configuration array \"globals\" contains invalid numeric key";

                const std::string* global = v.get_if<std::string>();
                if (!global)
                    return "configuration value in \"globals\" table must be a string";

                LUAU_ASSERT(0 <= index - 1 && index - 1 < globalsTable->size());
                globals[index - 1] = *global;
            }

            config.globals = std::move(globals);
        }

        if (*key == "aliases")
        {
            const ConfigTable* aliases = v.get_if<ConfigTable>();
            if (!aliases)
                return "configuration value for key \"aliases\" must be a table";

            for (const auto& [k, v] : *aliases)
            {
                const std::string* aliasKey = k.get_if<std::string>();
                if (!aliasKey)
                    return "configuration keys in \"aliases\" table must be strings";

                const std::string* aliasValue = v.get_if<std::string>();
                if (!aliasValue)
                    return "configuration values in \"aliases\" table must be strings";

                if (std::optional<std::string> errorMessage = parseAlias(config, *aliasKey, *aliasValue, aliasOptions))
                    return errorMessage;
            }
        }
    }

    return std::nullopt;
}

std::optional<std::string> extractLuauConfig(
    const std::string& source,
    Config& config,
    std::optional<ConfigOptions::AliasOptions> aliasOptions,
    InterruptCallbacks callbacks
)
{
    std::string error;
    std::optional<ConfigTable> configTable = extractConfig(source, callbacks, &error);
    if (!configTable)
        return error;

    if (!configTable->contains("luau"))
        return std::nullopt;

    ConfigTable* luauTable = (*configTable)["luau"].get_if<ConfigTable>();
    if (!luauTable)
        return "configuration value for key \"luau\" must be a table";

    return createLuauConfigFromLuauTable(config, *luauTable, aliasOptions);
}

} // namespace Luau
