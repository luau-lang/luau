// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lluz/Config.h"
#include "lluz/Frontend.h"

#include "Fixture.h"
#include "ScopedFlags.h"

#include "doctest.h"

#include <iostream>

using namespace lluz;

TEST_SUITE_BEGIN(XorStr("ConfigTest"));

TEST_CASE("language_mode")
{
    Config config;
    auto err = parseConfig(R"({"languageMode":"strict"})", config);
    REQUIRE(!err);

    CHECK_EQ(int(lluz::Mode::Strict), int(config.mode));
}

TEST_CASE("disable_a_lint_rule")
{
    Config config;
    auto err = parseConfig(R"(
        {"lint": {
            "UnknownGlobal": false,
        }}
    )",
        config);
    REQUIRE(!err);

    CHECK(!config.enabledLint.isEnabled(LintWarning::Code_UnknownGlobal));
    CHECK(config.enabledLint.isEnabled(LintWarning::Code_DeprecatedGlobal));
}

TEST_CASE("report_a_syntax_error")
{
    Config config;
    auto err = parseConfig(R"(
        {"lint": {
            "UnknownGlobal": "oops"
        }}
    )",
        config);

    REQUIRE(err);
    CHECK_EQ("In key UnknownGlobal: Bad setting 'oops'.  Valid options are true and false", *err);
}

TEST_CASE("noinfer_is_still_allowed")
{
    Config config;
    auto err = parseConfig(R"( {"language": {"mode": "noinfer"}} )", config, true);
    REQUIRE(!err);

    CHECK_EQ(int(lluz::Mode::NoCheck), int(config.mode));
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
    auto err = parseConfig(R"(
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
        config);
    REQUIRE(!err);

    CHECK(!config.enabledLint.isEnabled(LintWarning::Code_LocalShadow));
    CHECK(config.enabledLint.isEnabled(LintWarning::Code_ImportUnused));
}

TEST_CASE("issue_severity")
{
    Config config;
    CHECK(!config.lintErrors);
    CHECK(config.typeErrors);

    auto err = parseConfig(R"(
{
    "lintErrors": true,
    "typeErrors": false,
}
)",
        config);
    REQUIRE(!err);

    CHECK(config.lintErrors);
    CHECK(!config.typeErrors);
}

TEST_CASE("extra_globals")
{
    Config config;
    auto err = parseConfig(R"(
{
    "globals": ["it", "__DEV__"],
}
)",
        config);
    REQUIRE(!err);

    REQUIRE(config.globals.size() == 2);
    CHECK(config.globals[0] == XorStr("it"));
    CHECK(config.globals[1] == XorStr("__DEV__"));
}

TEST_CASE("lint_rules_compat")
{
    Config config;
    auto err = parseConfig(R"(
        {"lint": {
            "SameLineStatement": "enabled",
            "FunctionUnused": "disabled",
            "ImportUnused": "fatal",
        }}
    )",
        config, true);
    REQUIRE(!err);

    CHECK(config.enabledLint.isEnabled(LintWarning::Code_SameLineStatement));
    CHECK(!config.enabledLint.isEnabled(LintWarning::Code_FunctionUnused));
    CHECK(config.enabledLint.isEnabled(LintWarning::Code_ImportUnused));
    CHECK(config.fatalLint.isEnabled(LintWarning::Code_ImportUnused));
}

TEST_SUITE_END();
