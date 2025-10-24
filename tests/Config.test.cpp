// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Config.h"
#include "Luau/Frontend.h"
#include "Luau/LinterConfig.h"
#include "Luau/LuauConfig.h"

#include "Fixture.h"
#include "ScopedFlags.h"

#include "doctest.h"

#include <iostream>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using namespace Luau;

TEST_SUITE_BEGIN("ConfigTest");

TEST_CASE("language_mode")
{
    Config config;
    auto err = parseConfig(R"({"languageMode":"strict"})", config);
    REQUIRE(!err);

    CHECK_EQ(int(Luau::Mode::Strict), int(config.mode));
}

TEST_CASE("disable_a_lint_rule")
{
    Config config;
    auto err = parseConfig(
        R"(
        {"lint": {
            "UnknownGlobal": false,
        }}
    )",
        config
    );
    REQUIRE(!err);

    CHECK(!config.enabledLint.isEnabled(LintWarning::Code_UnknownGlobal));
    CHECK(config.enabledLint.isEnabled(LintWarning::Code_DeprecatedGlobal));
}

TEST_CASE("report_a_syntax_error")
{
    Config config;
    auto err = parseConfig(
        R"(
        {"lint": {
            "UnknownGlobal": "oops"
        }}
    )",
        config
    );

    REQUIRE(err);
    CHECK_EQ("In key UnknownGlobal: Bad setting 'oops'.  Valid options are true and false", *err);
}

TEST_CASE("noinfer_is_still_allowed")
{
    Config config;

    ConfigOptions opts;
    opts.compat = true;

    auto err = parseConfig(R"( {"language": {"mode": "noinfer"}} )", config, opts);
    REQUIRE(!err);

    CHECK_EQ(int(Luau::Mode::NoCheck), int(config.mode));
}

TEST_CASE("lint_warnings_are_ordered")
{
    Config root;
    auto err = parseConfig(R"({"lint": {"*": true, "LocalShadow": false}})", root);
    REQUIRE(!err);

    Config foo = root;
    err = parseConfig(R"({"lint": {"LocalShadow": true, "*": false}})", foo);
    REQUIRE(!err);

    CHECK(!root.enabledLint.isEnabled(LintWarning::Code_LocalShadow));
    CHECK(root.enabledLint.isEnabled(LintWarning::Code_LocalUnused));

    CHECK(!foo.enabledLint.isEnabled(LintWarning::Code_LocalShadow));
}

TEST_CASE("comments")
{
    Config config;
    auto err = parseConfig(
        R"(
{
    "lint": {
        "*": false,
        "SameLineStatement": true,
        "FunctionUnused": true,
        //"LocalShadow": true,
        //"LocalUnused": true,
        "ImportUnused": true,
        "ImplicitReturn": true
    }
}
)",
        config
    );
    REQUIRE(!err);

    CHECK(!config.enabledLint.isEnabled(LintWarning::Code_LocalShadow));
    CHECK(config.enabledLint.isEnabled(LintWarning::Code_ImportUnused));
}

TEST_CASE("issue_severity")
{
    Config config;
    CHECK(!config.lintErrors);
    CHECK(config.typeErrors);

    auto err = parseConfig(
        R"(
{
    "lintErrors": true,
    "typeErrors": false,
}
)",
        config
    );
    REQUIRE(!err);

    CHECK(config.lintErrors);
    CHECK(!config.typeErrors);
}

TEST_CASE("extra_globals")
{
    Config config;
    auto err = parseConfig(
        R"(
{
    "globals": ["it", "__DEV__"],
}
)",
        config
    );
    REQUIRE(!err);

    REQUIRE(config.globals.size() == 2);
    CHECK(config.globals[0] == "it");
    CHECK(config.globals[1] == "__DEV__");
}

TEST_CASE("lint_rules_compat")
{
    Config config;

    ConfigOptions opts;
    opts.compat = true;

    auto err = parseConfig(
        R"(
        {"lint": {
            "SameLineStatement": "enabled",
            "FunctionUnused": "disabled",
            "ImportUnused": "fatal",
        }}
    )",
        config,
        opts
    );
    REQUIRE(!err);

    CHECK(config.enabledLint.isEnabled(LintWarning::Code_SameLineStatement));
    CHECK(!config.enabledLint.isEnabled(LintWarning::Code_FunctionUnused));
    CHECK(config.enabledLint.isEnabled(LintWarning::Code_ImportUnused));
    CHECK(config.fatalLint.isEnabled(LintWarning::Code_ImportUnused));
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("ConfigLuauTest");

TEST_CASE("extract_configuration")
{
    std::string source = R"(
        local config = {}
        config.luau = {}

        config.luau.languagemode = "strict"
        config.luau.lint = {
            ["*"] = true,
            LocalUnused = false
        }
        config.luau.linterrors = true
        config.luau.typeerrors = true
        config.luau.globals = {"expect"}
        config.luau.aliases = {
            src = "./src"
        }

        return config
    )";

    std::string error;
    std::optional<ConfigTable> configTable = extractConfig(source, InterruptCallbacks{}, &error);
    REQUIRE(configTable);

    CHECK(configTable->size() == 1);
    REQUIRE(configTable->contains("luau"));
    ConfigTable* luau = (*configTable)["luau"].get_if<ConfigTable>();
    REQUIRE(luau);
    CHECK(luau->size() == 6);

    REQUIRE(luau->contains("languagemode"));
    std::string* languageMode = (*luau)["languagemode"].get_if<std::string>();
    REQUIRE(languageMode);
    CHECK(*languageMode == "strict");

    REQUIRE(luau->contains("lint"));
    ConfigTable* lint = (*luau)["lint"].get_if<ConfigTable>();
    REQUIRE(lint);
    CHECK(lint->size() == 2);
    REQUIRE(lint->contains("*"));
    bool* all = (*lint)["*"].get_if<bool>();
    REQUIRE(all);
    CHECK(*all);
    bool* localUnused = (*lint)["LocalUnused"].get_if<bool>();
    REQUIRE(localUnused);
    CHECK(!(*localUnused));

    REQUIRE(luau->contains("linterrors"));
    bool* lintErrors = (*luau)["linterrors"].get_if<bool>();
    REQUIRE(lintErrors);
    CHECK(*lintErrors);

    REQUIRE(luau->contains("typeerrors"));
    bool* typeErrors = (*luau)["typeerrors"].get_if<bool>();
    REQUIRE(typeErrors);
    CHECK(*typeErrors);

    REQUIRE(luau->contains("globals"));
    ConfigTable* globalsTable = (*luau)["globals"].get_if<ConfigTable>();
    REQUIRE(globalsTable);
    CHECK(globalsTable->size() == 1);
    REQUIRE(globalsTable->contains(1));
    std::string* global = (*globalsTable)[1].get_if<std::string>();
    REQUIRE(global);
    CHECK(*global == "expect");

    REQUIRE(luau->contains("aliases"));
    ConfigTable* aliases = (*luau)["aliases"].get_if<ConfigTable>();
    REQUIRE(aliases);
    CHECK(aliases->size() == 1);
    REQUIRE(aliases->contains("src"));
    std::string* alias = (*aliases)["src"].get_if<std::string>();
    REQUIRE(alias);
    CHECK(*alias == "./src");
}

TEST_CASE("extract_luau_configuration")
{
    std::string source = R"(
        local config = {}
        config.luau = {}

        config.luau.languagemode = "strict"
        config.luau.lint = {
            ["*"] = true,
            LocalUnused = false
        }
        config.luau.linterrors = true
        config.luau.typeerrors = true
        config.luau.globals = {"expect"}
        config.luau.aliases = {
            src = "./src"
        }

        return config
    )";

    ConfigOptions::AliasOptions aliasOptions;
    aliasOptions.configLocation = "/some/path";
    aliasOptions.overwriteAliases = true;

    Config config;
    std::optional<std::string> error = extractLuauConfig(source, config, std::move(aliasOptions), InterruptCallbacks{});
    REQUIRE(!error);

    CHECK_EQ(config.mode, Mode::Strict);

    for (LintWarning::Code code = static_cast<LintWarning::Code>(0); code <= LintWarning::Code::Code__Count; code = LintWarning::Code(int(code) + 1))
    {
        if (code == LintWarning::Code_LocalUnused)
            CHECK(!config.enabledLint.isEnabled(code));
        else
            CHECK(config.enabledLint.isEnabled(code));
    }

    CHECK_EQ(config.lintErrors, true);
    CHECK_EQ(config.typeErrors, true);

    CHECK(config.globals.size() == 1);
    CHECK(config.globals[0] == "expect");

    CHECK(config.aliases.size() == 1);
    REQUIRE(config.aliases.contains("src"));
    CHECK(config.aliases["src"].value == "./src");
}

TEST_CASE("yielded_configuration")
{
    std::string source = R"(
        coroutine.yield()
    )";

    std::string error;
    std::optional<ConfigTable> configTable = extractConfig(source, InterruptCallbacks{}, &error);
    REQUIRE(!configTable);
    CHECK(error == "configuration execution cannot yield");
}

TEST_CASE("interrupt_execution" * doctest::timeout(2))
{
    std::string source = R"(
        while true do end
    )";

    std::string error;
    std::optional<ConfigTable> configTable = extractConfig(
        source,
        {
            nullptr,
            [](lua_State* L, int gc)
            {
                throw std::runtime_error("interrupted");
            },
        },
        &error
    );
    REQUIRE(!configTable);
    CHECK(error.find("interrupted") != std::string_view::npos);
}

TEST_CASE("validate_return_value")
{
    std::vector<std::pair<std::string, std::string>> testCases;
    testCases.emplace_back("", "configuration must return exactly one value");
    testCases.emplace_back("return {}, {}", "configuration must return exactly one value");
    testCases.emplace_back("return 'a string'", "configuration did not return a table");

    for (const auto& [source, expectedError] : testCases)
    {
        std::string error;
        std::optional<ConfigTable> configTable = extractConfig(source, InterruptCallbacks{}, &error);
        REQUIRE(!configTable);
        CHECK(error == expectedError);
    }
}

TEST_SUITE_END();
