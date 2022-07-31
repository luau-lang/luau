// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lluz/Config.h"

#include "lluz/Lexer.h"
#include "lluz/StringUtils.h"

#include "..\..\..\..\Security\XorString.h"

namespace
{

using Error = std::optional<std::string>;

}

namespace lluz
{

static Error parseBoolean(bool& result, const std::string& value)
{
    if (value == XorStr("true"))
        result = true;
    else if (value == XorStr("false"))
        result = false;
    else
        return Error{"Bad setting '" + value + "'.  Valid options are true and false"};

    return std::nullopt;
}

Error parseModeString(Mode& mode, const std::string& modeString, bool compat)
{
    if (modeString == XorStr("nocheck"))
        mode = Mode::NoCheck;
    else if (modeString == XorStr("strict"))
        mode = Mode::Strict;
    else if (modeString == XorStr("nonstrict"))
        mode = Mode::Nonstrict;
    else if (modeString == XorStr("noinfer") && compat)
        mode = Mode::NoCheck;
    else
        return Error{"Bad mode \"" + modeString + "\".  Valid options are nocheck, nonstrict, and strict"};

    return std::nullopt;
}

static Error parseLintRuleStringForCode(
    LintOptions& enabledLints, LintOptions& fatalLints, LintWarning::Code code, const std::string& value, bool compat)
{
    if (value == XorStr("true"))
    {
        enabledLints.enableWarning(code);
    }
    else if (value == XorStr("false"))
    {
        enabledLints.disableWarning(code);
    }
    else if (compat)
    {
        if (value == XorStr("enabled"))
        {
            enabledLints.enableWarning(code);
            fatalLints.disableWarning(code);
        }
        else if (value == XorStr("disabled"))
        {
            enabledLints.disableWarning(code);
            fatalLints.disableWarning(code);
        }
        else if (value == XorStr("fatal"))
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
    if (warningName == XorStr("*"))
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

static void next(Lexer& lexer)
{
    lexer.next();

    // skip C-style comments as Lexer only understands Lua-style comments atm
    while (lexer.current().type == '/')
    {
        Lexeme peek = lexer.lookahead();

        if (peek.type != '/' || peek.location.begin != lexer.current().location.end)
            break;

        lexer.nextline();
    }
}

static Error fail(Lexer& lexer, const char* message)
{
    Lexeme cur = lexer.current();

    return format(XorStr("Expected %s at line %d, got %s instead"), message, cur.location.begin.line + 1, cur.toString().c_str());
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
        return fail(lexer, XorStr("'{'"));
    next(lexer);

    for (;;)
    {
        if (arrayTop)
        {
            if (lexer.current().type == ']')
            {
                next(lexer);
                arrayTop = false;

                lluz_ASSERT(!keys.empty());
                keys.pop_back();

                if (lexer.current().type == ',')
                    next(lexer);
                else if (lexer.current().type != '}')
                    return fail(lexer, XorStr("',' or '}'"));
            }
            else if (lexer.current().type == Lexeme::QuotedString)
            {
                std::string value(lexer.current().data, lexer.current().length);
                next(lexer);

                if (Error err = action(keys, value))
                    return err;

                if (lexer.current().type == ',')
                    next(lexer);
                else if (lexer.current().type != ']')
                    return fail(lexer, XorStr("',' or ']'"));
            }
            else
                return fail(lexer, XorStr("array element or ']'"));
        }
        else
        {
            if (lexer.current().type == '}')
            {
                next(lexer);

                if (keys.empty())
                {
                    if (lexer.current().type != Lexeme::Eof)
                        return fail(lexer, XorStr("end of file"));

                    return {};
                }
                else
                    keys.pop_back();

                if (lexer.current().type == ',')
                    next(lexer);
                else if (lexer.current().type != '}')
                    return fail(lexer, XorStr("',' or '}'"));
            }
            else if (lexer.current().type == Lexeme::QuotedString)
            {
                std::string key(lexer.current().data, lexer.current().length);
                next(lexer);

                keys.push_back(key);

                if (lexer.current().type != ':')
                    return fail(lexer, XorStr("':'"));
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
                                            ? std::string(lexer.current().data, lexer.current().length)
                                            : (lexer.current().type == Lexeme::ReservedTrue ? XorStr("true" : "false"));
                    next(lexer);

                    if (Error err = action(keys, value))
                        return err;

                    keys.pop_back();

                    if (lexer.current().type == ',')
                        next(lexer);
                    else if (lexer.current().type != '}')
                        return fail(lexer, XorStr("',' or '}'"));
                }
                else
                    return fail(lexer, XorStr("field value"));
            }
            else
                return fail(lexer, XorStr("field key"));
        }
    }

    return {};
}

Error parseConfig(const std::string& contents, Config& config, bool compat)
{
    return parseJson(contents, [&](const std::vector<std::string>& keys, const std::string& value) -> Error {
        if (keys.size() == 1 && keys[0] == XorStr("languageMode"))
            return parseModeString(config.mode, value, compat);
        else if (keys.size() == 2 && keys[0] == XorStr("lint"))
            return parseLintRuleString(config.enabledLint, config.fatalLint, keys[1], value, compat);
        else if (keys.size() == 1 && keys[0] == XorStr("lintErrors"))
            return parseBoolean(config.lintErrors, value);
        else if (keys.size() == 1 && keys[0] == XorStr("typeErrors"))
            return parseBoolean(config.typeErrors, value);
        else if (keys.size() == 1 && keys[0] == XorStr("globals"))
        {
            config.globals.push_back(value);
            return std::nullopt;
        }
        else if (compat && keys.size() == 2 && keys[0] == XorStr("language") && keys[1] == XorStr("mode"))
            return parseModeString(config.mode, value, compat);
        else
        {
            std::vector<std::string_view> keysv(keys.begin(), keys.end());
            return XorStr("Unknown key " + join(keysv, "/"));
        }
    });
}

const Config& NullConfigResolver::getConfig(const ModuleName& name) const
{
    return defaultConfig;
}

} // namespace lluz
