// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/Config.h"
#include "Luau/DenseHash.h"
#include "Luau/Variant.h"

#include <string>

struct lua_State;

namespace Luau
{

constexpr const char* kLuauConfigName = ".config.luau";

struct ConfigTableKey : public Variant<std::string, double>
{
    ConfigTableKey() = default;
    ConfigTableKey(const std::string& s)
        : Variant(s)
    {
    }
    ConfigTableKey(std::string&& s)
        : Variant(std::move(s))
    {
    }
    ConfigTableKey(const char* s)
        : Variant(std::string(s))
    {
    }
    ConfigTableKey(double d)
        : Variant(d)
    {
    }

    std::string toString() const
    {
        if (const double* number = get_if<double>())
            return std::to_string(*number);
        else if (const std::string* str = get_if<std::string>())
            return *str;

        LUAU_UNREACHABLE();
    }
};

template<typename VariantType>
struct VariantHashDefault
{
    size_t operator()(const VariantType& variant) const
    {
        size_t result = 0;
        visit(
            [&result](auto&& value)
            {
                result = detail::DenseHashDefault<std::decay_t<decltype(value)>>{}(value);
            },
            variant
        );
        return result;
    }
};

// Forward declaration
struct ConfigValue;

struct ConfigTable : public DenseHashMap<ConfigTableKey, ConfigValue, VariantHashDefault<ConfigTableKey>>
{
    ConfigTable()
        : DenseHashMap<ConfigTableKey, ConfigValue, VariantHashDefault<ConfigTableKey>>({})
    {
    }
};

struct ConfigValue : public Variant<std::string, double, bool, ConfigTable>
{
    using Variant::Variant;
};

struct InterruptCallbacks
{
    std::function<void(lua_State*)> initCallback;
    void (*interruptCallback)(lua_State* L, int gc);
};

// Executes the contents of `source` in a sandboxed Luau VM and extracts the
// configuration table that it returns. If extraction fails, std::nullopt is
// returned and `error` is populated with the error message, if provided.
std::optional<ConfigTable> extractConfig(const std::string& source, const InterruptCallbacks& callbacks, std::string* error = nullptr);

// Extracts a Luau::Config object into `config` from the configuration code in
// `source`, returning an error message if extraction fails.
std::optional<std::string> extractLuauConfig(
    const std::string& source,
    Config& config,
    std::optional<ConfigOptions::AliasOptions> aliasOptions,
    InterruptCallbacks callbacks
);

} // namespace Luau
