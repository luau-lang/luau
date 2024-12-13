// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/DenseHash.h"
#include "Luau/LinterConfig.h"
#include "Luau/ParseOptions.h"

#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace Luau
{

using ModuleName = std::string;

constexpr const char* kConfigName = ".luaurc";

struct Config
{
    Config();
    Config(const Config& other);
    Config& operator=(const Config& other);
    Config(Config&& other) = default;
    Config& operator=(Config&& other) = default;

    Mode mode = Mode::Nonstrict;

    ParseOptions parseOptions;

    LintOptions enabledLint;
    LintOptions fatalLint;

    bool lintErrors = false;
    bool typeErrors = true;

    std::vector<std::string> globals;

    struct AliasInfo
    {
        std::string value;
        std::string_view configLocation;
        std::string originalCase; // The alias in its original case.
    };

    DenseHashMap<std::string, AliasInfo> aliases{""};

    void setAlias(std::string alias, std::string value, const std::string& configLocation);

private:
    // Prevents making unnecessary copies of the same config location string.
    DenseHashMap<std::string, std::unique_ptr<std::string>> configLocationCache{""};
};

struct ConfigResolver
{
    virtual ~ConfigResolver() {}

    virtual const Config& getConfig(const ModuleName& name) const = 0;
};

struct NullConfigResolver : ConfigResolver
{
    Config defaultConfig;

    virtual const Config& getConfig(const ModuleName& name) const override;
};

std::optional<std::string> parseModeString(Mode& mode, const std::string& modeString, bool compat = false);
std::optional<std::string> parseLintRuleString(
    LintOptions& enabledLints,
    LintOptions& fatalLints,
    const std::string& warningName,
    const std::string& value,
    bool compat = false
);

bool isValidAlias(const std::string& alias);

struct ConfigOptions
{
    bool compat = false;

    struct AliasOptions
    {
        std::string configLocation;
        bool overwriteAliases;
    };
    std::optional<AliasOptions> aliasOptions = std::nullopt;
};

std::optional<std::string> parseConfig(const std::string& contents, Config& config, const ConfigOptions& options = ConfigOptions{});

} // namespace Luau
