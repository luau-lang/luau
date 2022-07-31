// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lluz/Linter.h"
#include "lluz/ParseOptions.h"

#include <string>
#include <optional>
#include <vector>

namespace lluz
{

using ModuleName = std::string;

constexpr const char* kConfigName = ".lluzrc";

struct Config
{
    Config()
    {
        enabledLint.setDefaults();
    }

    Mode mode = Mode::NoCheck;

    ParseOptions parseOptions;

    LintOptions enabledLint;
    LintOptions fatalLint;

    bool lintErrors = false;
    bool typeErrors = true;

    std::vector<std::string> globals;
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
    LintOptions& enabledLints, LintOptions& fatalLints, const std::string& warningName, const std::string& value, bool compat = false);

std::optional<std::string> parseConfig(const std::string& contents, Config& config, bool compat = false);

} // namespace lluz
