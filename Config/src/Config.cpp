// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Config.h"

#include "Luau/Lexer.h"
#include "Luau/StringUtils.h"
#include <algorithm>
#include <memory>
#include <string>

namespace Luau
{

using Error = std::optional<std::string>;

Config::Config()
{
    enabledLint.setDefaults();
}

Config::Config(const Config& other)
    : mode(other.mode)
    , parseOptions(other.parseOptions)
    , enabledLint(other.enabledLint)
    , fatalLint(other.fatalLint)
    , lintErrors(other.lintErrors)
    , typeErrors(other.typeErrors)
    , globals(other.globals)
{
    for (const auto& [_, aliasInfo] : other.aliases)
    {
        setAlias(aliasInfo.originalCase, aliasInfo.value, std::string(aliasInfo.configLocation));
    }
}

Config& Config::operator=(const Config& other)
{
    if (this != &other)
    {
        Config copy(other);
        std::swap(*this, copy);
    }
    return *this;
}

void Config::setAlias(std::string alias, std::string value, const std::string& configLocation)
{
    std::string lowercasedAlias = alias;
    std::transform(
        lowercasedAlias.begin(),
        lowercasedAlias.end(),
        lowercasedAlias.begin(),
        [](unsigned char c)
        {
            return ('A' <= c && c <= 'Z') ? (c + ('a' - 'A')) : c;
        }
    );

    AliasInfo& info = aliases[lowercasedAlias];
    info.value = std::move(value);
    info.originalCase = std::move(alias);

    if (!configLocationCache.contains(configLocation))
        configLocationCache[configLocation] = std::make_unique<std::string>(configLocation);

    info.configLocation = *configLocationCache[configLocation];
}

static Error parseBoolean(bool& result, const std::string& value)
{
    if (value == "true")
        result = true;
    else if (value == "false")
        result = false;
    else
        return Error{"Bad setting '" + value + "'.  Valid options are true and false"};

    return std::nullopt;
}

Error parseModeString(Mode& mode, const std::string& modeString, bool compat)
{
    if (modeString == "nocheck")
        mode = Mode::NoCheck;
    else if (modeString == "strict")
        mode = Mode::Strict;
    else if (modeString == "nonstrict")
        mode = Mode::Nonstrict;
    else if (modeString == "noinfer" && compat)
        mode = Mode::NoCheck;
    else
        return Error{"Bad mode \"" + modeString + "\".  Valid options are nocheck, nonstrict, and strict"};

    return std::nullopt;
}

static Error parseLintRuleStringForCode(
    LintOptions& enabledLints,
    LintOptions& fatalLints,
    LintWarning::Code code,
    const std::string& value,
    bool compat
)
{
    if (value == "true")
    {
        enabledLints.enableWarning(code);
    }
    else if (value == "false")
    {
        enabledLints.disableWarning(code);
    }
    else if (compat)
    {
        if (value == "enabled")
        {
            enabledLints.enableWarning(code);
            fatalLints.disableWarning(code);
        }
        else if (value == "disabled")
        {
            enabledLints.disableWarning(code);
            fatalLints.disableWarning(code);
        }
        else if (value == "fatal")
        {
            enabledLints.enableWarning(code);
            fatalLints.enableWarning(code);
        }
        else
        {
            return Error{"Bad setting '" + value + "'.  Valid options are enabled, disabled, and fatal"};
        }
    }
    else
    {
        return Error{"Bad setting '" + value + "'.  Valid options are true and false"};
    }

    return std::nullopt;
}

Error parseLintRuleString(LintOptions& enabledLints, LintOptions& fatalLints, const std::string& warningName, const std::string& value, bool compat)
{
    if (warningName == "*")
    {
        for (int code = LintWarning::Code_Unknown; code < LintWarning::Code__Count; ++code)
        {
            if (auto err = parseLintRuleStringForCode(enabledLints, fatalLints, LintWarning::Code(code), value, compat))
                return Error{"In key " + warningName + ": " + *err};
        }
    }
    else
    {
        LintWarning::Code code = LintWarning::parseName(warningName.c_str());

        if (code == LintWarning::Code_Unknown)
            return Error{"Unknown lint " + warningName};

        if (auto err = parseLintRuleStringForCode(enabledLints, fatalLints, code, value, compat))
            return Error{"In key " + warningName + ": " + *err};
    }

    return std::nullopt;
}

bool isValidAlias(const std::string& alias)
{
    if (alias.empty())
        return false;

    bool aliasIsNotAPath = alias != "." && alias != ".." && alias.find_first_of("\\/") == std::string::npos;

    if (!aliasIsNotAPath)
        return false;

    for (char ch : alias)
    {
        bool isupper = 'A' <= ch && ch <= 'Z';
        bool islower = 'a' <= ch && ch <= 'z';
        bool isdigit = '0' <= ch && ch <= '9';
        if (!isupper && !islower && !isdigit && ch != '-' && ch != '_' && ch != '.')
            return false;
    }

    return true;
}

static Error parseAlias(
    Config& config,
    const std::string& aliasKey,
    const std::string& aliasValue,
    const std::optional<ConfigOptions::AliasOptions>& aliasOptions
)
{
    if (!isValidAlias(aliasKey))
        return Error{"Invalid alias " + aliasKey};

    if (!aliasOptions)
        return Error("Cannot parse aliases without alias options");

    if (aliasOptions->overwriteAliases || !config.aliases.contains(aliasKey))
        config.setAlias(aliasKey, aliasValue, aliasOptions->configLocation);

    return std::nullopt;
}

static void next(Lexer& lexer)
{
    lexer.next();

    // skip C-style comments as Lexer only understands Lua-style comments atm
    while (lexer.current().type == Luau::Lexeme::FloorDiv)
        lexer.nextline();
}

static Error fail(Lexer& lexer, const char* message)
{
    Lexeme cur = lexer.current();

    return format("Expected %s at line %d, got %s instead", message, cur.location.begin.line + 1, cur.toString().c_str());
}

template<typename Action>
static Error parseJson(const std::string& contents, Action action)
{
    Allocator allocator;
    AstNameTable names(allocator);
    Lexer lexer(contents.data(), contents.size(), names);
    next(lexer);

    std::vector<std::string> keys;
    bool arrayTop = false; // we don't support nested arrays

    if (lexer.current().type != '{')
        return fail(lexer, "'{'");
    next(lexer);

    for (;;)
    {
        if (arrayTop)
        {
            if (lexer.current().type == ']')
            {
                next(lexer);
                arrayTop = false;

                LUAU_ASSERT(!keys.empty());
                keys.pop_back();

                if (lexer.current().type == ',')
                    next(lexer);
                else if (lexer.current().type != '}')
                    return fail(lexer, "',' or '}'");
            }
            else if (lexer.current().type == Lexeme::QuotedString)
            {
                std::string value(lexer.current().data, lexer.current().getLength());
                next(lexer);

                if (Error err = action(keys, value))
                    return err;

                if (lexer.current().type == ',')
                    next(lexer);
                else if (lexer.current().type != ']')
                    return fail(lexer, "',' or ']'");
            }
            else
                return fail(lexer, "array element or ']'");
        }
        else
        {
            if (lexer.current().type == '}')
            {
                next(lexer);

                if (keys.empty())
                {
                    if (lexer.current().type != Lexeme::Eof)
                        return fail(lexer, "end of file");

                    return {};
                }
                else
                    keys.pop_back();

                if (lexer.current().type == ',')
                    next(lexer);
                else if (lexer.current().type != '}')
                    return fail(lexer, "',' or '}'");
            }
            else if (lexer.current().type == Lexeme::QuotedString)
            {
                std::string key(lexer.current().data, lexer.current().getLength());
                next(lexer);

                keys.push_back(key);

                if (lexer.current().type != ':')
                    return fail(lexer, "':'");
                next(lexer);

                if (lexer.current().type == '{' || lexer.current().type == '[')
                {
                    arrayTop = (lexer.current().type == '[');
                    next(lexer);
                }
                else if (lexer.current().type == Lexeme::QuotedString || lexer.current().type == Lexeme::ReservedTrue ||
                         lexer.current().type == Lexeme::ReservedFalse)
                {
                    std::string value = lexer.current().type == Lexeme::QuotedString
                                            ? std::string(lexer.current().data, lexer.current().getLength())
                                            : (lexer.current().type == Lexeme::ReservedTrue ? "true" : "false");
                    next(lexer);

                    if (Error err = action(keys, value))
                        return err;

                    keys.pop_back();

                    if (lexer.current().type == ',')
                        next(lexer);
                    else if (lexer.current().type != '}')
                        return fail(lexer, "',' or '}'");
                }
                else
                    return fail(lexer, "field value");
            }
            else
                return fail(lexer, "field key");
        }
    }

    return {};
}

Error parseConfig(const std::string& contents, Config& config, const ConfigOptions& options)
{
    return parseJson(
        contents,
        [&](const std::vector<std::string>& keys, const std::string& value) -> Error
        {
            if (keys.size() == 1 && keys[0] == "languageMode")
                return parseModeString(config.mode, value, options.compat);
            else if (keys.size() == 2 && keys[0] == "lint")
                return parseLintRuleString(config.enabledLint, config.fatalLint, keys[1], value, options.compat);
            else if (keys.size() == 1 && keys[0] == "lintErrors")
                return parseBoolean(config.lintErrors, value);
            else if (keys.size() == 1 && keys[0] == "typeErrors")
                return parseBoolean(config.typeErrors, value);
            else if (keys.size() == 1 && keys[0] == "globals")
            {
                config.globals.push_back(value);
                return std::nullopt;
            }
            else if (keys.size() == 2 && keys[0] == "aliases")
                return parseAlias(config, keys[1], value, options.aliasOptions);
            else if (options.compat && keys.size() == 2 && keys[0] == "language" && keys[1] == "mode")
                return parseModeString(config.mode, value, options.compat);
            else
            {
                std::vector<std::string_view> keysv(keys.begin(), keys.end());
                return "Unknown key " + join(keysv, "/");
            }
        }
    );
}

const Config& NullConfigResolver::getConfig(const ModuleName& name) const
{
    return defaultConfig;
}

} // namespace Luau
