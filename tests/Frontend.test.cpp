// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Frontend.h"
#include "Luau/Parser.h"
#include "Luau/RequireTracer.h"

#include "Fixture.h"

#include "doctest.h"

#include <algorithm>

using namespace Luau;

namespace
{

struct NaiveModuleResolver : ModuleResolver
{
    std::optional<ModuleInfo> resolveModuleInfo(const ModuleName& currentModuleName, const AstExpr& pathExpr) override
    {
        if (auto name = pathExprToModuleName(currentModuleName, pathExpr))
            return {{*name, false}};

        return std::nullopt;
    }

    const ModulePtr getModule(const ModuleName& moduleName) const override
    {
        return nullptr;
    }

    bool moduleExists(const ModuleName& moduleName) const override
    {
        return false;
    }

    std::string getHumanReadableModuleName(const ModuleName& moduleName) const override
    {
        return moduleName;
    }
};

NaiveModuleResolver naiveModuleResolver;

struct NaiveFileResolver : NullFileResolver
{
    std::optional<ModuleName> fromAstFragment(AstExpr* expr) const override
    {
        AstExprGlobal* g = expr->as<AstExprGlobal>();
        if (g && g->name == "Modules")
            return "Modules";

        if (g && g->name == "game")
            return "game";

        return std::nullopt;
    }

    ModuleName concat(const ModuleName& lhs, std::string_view rhs) const override
    {
        return lhs + "/" + ModuleName(rhs);
    }
};

} // namespace

struct FrontendFixture : Fixture
{
    FrontendFixture()
    {
        addGlobalBinding(typeChecker, "game", frontend.typeChecker.anyType, "@test");
        addGlobalBinding(typeChecker, "script", frontend.typeChecker.anyType, "@test");
    }
};

TEST_SUITE_BEGIN("FrontendTest");

TEST_CASE_FIXTURE(FrontendFixture, "find_a_require")
{
    AstStatBlock* program = parse(R"(
        local M = require(Modules.Foo.Bar)
    )");

    NaiveFileResolver naiveFileResolver;

    auto res = traceRequires(&naiveFileResolver, program, "");
    CHECK_EQ(1, res.requires.size());
    CHECK_EQ(res.requires[0].first, "Modules/Foo/Bar");
}

// It could be argued that this should not work.
TEST_CASE_FIXTURE(FrontendFixture, "find_a_require_inside_a_function")
{
    AstStatBlock* program = parse(R"(
        function foo()
            local M = require(Modules.Foo.Bar)
        end
    )");

    NaiveFileResolver naiveFileResolver;

    auto res = traceRequires(&naiveFileResolver, program, "");
    CHECK_EQ(1, res.requires.size());
}

TEST_CASE_FIXTURE(FrontendFixture, "real_source")
{
    AstStatBlock* program = parse(R"(
        return function()
            local Modules = game:GetService("CoreGui").Gui.Modules

            local Roact = require(Modules.Common.Roact)
            local Rodux = require(Modules.Common.Rodux)

            local AppReducer = require(Modules.LuaApp.AppReducer)
            local AEAppReducer = require(Modules.LuaApp.Reducers.AEReducers.AEAppReducer)
            local AETabList = require(Modules.LuaApp.Components.Avatar.UI.Views.Portrait.AETabList)
            local mockServices = require(Modules.LuaApp.TestHelpers.mockServices)
            local DeviceOrientationMode = require(Modules.LuaApp.DeviceOrientationMode)
            local MockAvatarEditorTheme = require(Modules.LuaApp.TestHelpers.MockAvatarEditorTheming)
            local FFlagAvatarEditorEnableThemes = settings():GetFFlag("AvatarEditorEnableThemes2")
        end
    )");

    NaiveFileResolver naiveFileResolver;

    auto res = traceRequires(&naiveFileResolver, program, "");
    CHECK_EQ(8, res.requires.size());
}

TEST_CASE_FIXTURE(FrontendFixture, "automatically_check_dependent_scripts")
{
    fileResolver.source["game/Gui/Modules/A"] = "return {hello=5, world=true}";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A)
        return {b_value = A.hello}
    )";

    frontend.check("game/Gui/Modules/B");

    ModulePtr bModule = frontend.moduleResolver.modules["game/Gui/Modules/B"];
    REQUIRE(bModule != nullptr);
    CHECK(bModule->errors.empty());
    Luau::dumpErrors(bModule);

    auto bExports = first(bModule->getModuleScope()->returnType);
    REQUIRE(!!bExports);

    CHECK_EQ("{| b_value: number |}", toString(*bExports));
}

TEST_CASE_FIXTURE(FrontendFixture, "automatically_check_cyclically_dependent_scripts")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
        local Modules = game:GetService('Gui').Modules
        local B = require(Modules.B)
        return {}
    )";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A)
        require(Modules.C)
        return {}
    )";
    fileResolver.source["game/Gui/Modules/C"] = R"(
        local Modules = game:GetService('Gui').Modules
        do local A = require(Modules.A) end
        return {}
    )";

    fileResolver.source["game/Gui/Modules/D"] = R"(
        local Modules = game:GetService('Gui').Modules
        do local A = require(Modules.A) end
        return {}
    )";

    CheckResult result1 = frontend.check("game/Gui/Modules/B");
    LUAU_REQUIRE_ERROR_COUNT(4, result1);

    CHECK_MESSAGE(get<ModuleHasCyclicDependency>(result1.errors[0]), "Should have been a ModuleHasCyclicDependency: " << toString(result1.errors[0]));

    CHECK_MESSAGE(get<ModuleHasCyclicDependency>(result1.errors[1]), "Should have been a ModuleHasCyclicDependency: " << toString(result1.errors[1]));

    CHECK_MESSAGE(get<ModuleHasCyclicDependency>(result1.errors[2]), "Should have been a ModuleHasCyclicDependency: " << toString(result1.errors[2]));

    CheckResult result2 = frontend.check("game/Gui/Modules/D");
    LUAU_REQUIRE_ERROR_COUNT(0, result2);
}

TEST_CASE_FIXTURE(FrontendFixture, "any_annotation_breaks_cycle")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
        local Modules = game:GetService('Gui').Modules
        local B = require(Modules.B)
        return {hello = B.hello}
    )";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A) :: any
        return {hello = A.hello}
    )";

    CheckResult result = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(FrontendFixture, "nocheck_modules_are_typed")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
        --!nocheck
        export type Foo = number
        return {hello = "hi"}
    )";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        --!nonstrict
        export type Foo = number
        return {hello = "hi"}
    )";
    fileResolver.source["game/Gui/Modules/C"] = R"(
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A)
        local B = require(Modules.B)
        local five : A.Foo = 5
    )";

    CheckResult result = frontend.check("game/Gui/Modules/C");
    LUAU_REQUIRE_NO_ERRORS(result);

    ModulePtr aModule = frontend.moduleResolver.modules["game/Gui/Modules/A"];
    REQUIRE(bool(aModule));

    std::optional<TypeId> aExports = first(aModule->getModuleScope()->returnType);
    REQUIRE(bool(aExports));

    ModulePtr bModule = frontend.moduleResolver.modules["game/Gui/Modules/B"];
    REQUIRE(bool(bModule));

    std::optional<TypeId> bExports = first(bModule->getModuleScope()->returnType);
    REQUIRE(bool(bExports));

    CHECK_EQ(toString(*aExports), toString(*bExports));
}

TEST_CASE_FIXTURE(FrontendFixture, "cycle_detection_between_check_and_nocheck")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
        local Modules = game:GetService('Gui').Modules
        local B = require(Modules.B)
        return {hello = B.hello}
    )";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        --!nocheck
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A)
        return {hello = A.hello}
    )";

    CheckResult result = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(FrontendFixture, "nocheck_cycle_used_by_checked")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
        --!nocheck
        local Modules = game:GetService('Gui').Modules
        local B = require(Modules.B)
        return {hello = B.hello}
    )";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        --!nocheck
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A)
        return {hello = A.hello}
    )";
    fileResolver.source["game/Gui/Modules/C"] = R"(
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A)
        local B = require(Modules.B)
        return {a=A, b=B}
    )";

    CheckResult result = frontend.check("game/Gui/Modules/C");
    LUAU_REQUIRE_NO_ERRORS(result);

    ModulePtr cModule = frontend.moduleResolver.modules["game/Gui/Modules/C"];
    REQUIRE(bool(cModule));

    std::optional<TypeId> cExports = first(cModule->getModuleScope()->returnType);
    REQUIRE(bool(cExports));
    CHECK_EQ("{| a: any, b: any |}", toString(*cExports));
}

TEST_CASE_FIXTURE(FrontendFixture, "cycle_detection_disabled_in_nocheck")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
        --!nocheck
        local Modules = game:GetService('Gui').Modules
        local B = require(Modules.B)
        return {hello = B.hello}
    )";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        --!nocheck
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A)
        return {hello = A.hello}
    )";

    CheckResult result = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(FrontendFixture, "cycle_errors_can_be_fixed")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
        local Modules = game:GetService('Gui').Modules
        local B = require(Modules.B)
        return {hello = B.hello}
    )";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A)
        return {hello = A.hello}
    )";

    CheckResult result1 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(2, result1);

    CHECK_MESSAGE(get<ModuleHasCyclicDependency>(result1.errors[0]), "Should have been a ModuleHasCyclicDependency: " << toString(result1.errors[0]));

    CHECK_MESSAGE(get<ModuleHasCyclicDependency>(result1.errors[1]), "Should have been a ModuleHasCyclicDependency: " << toString(result1.errors[1]));

    fileResolver.source["game/Gui/Modules/B"] = R"(
        return {hello = 42}
    )";
    frontend.markDirty("game/Gui/Modules/B");

    CheckResult result2 = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result2);
}

TEST_CASE_FIXTURE(FrontendFixture, "cycle_error_paths")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
        local Modules = game:GetService('Gui').Modules
        local B = require(Modules.B)
        return {hello = B.hello}
    )";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A)
        return {hello = A.hello}
    )";

    CheckResult result = frontend.check("game/Gui/Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(2, result);

    auto ce1 = get<ModuleHasCyclicDependency>(result.errors[0]);
    REQUIRE(ce1);
    CHECK_EQ(result.errors[0].moduleName, "game/Gui/Modules/B");
    REQUIRE_EQ(ce1->cycle.size(), 2);
    CHECK_EQ(ce1->cycle[0], "game/Gui/Modules/A");
    CHECK_EQ(ce1->cycle[1], "game/Gui/Modules/B");

    auto ce2 = get<ModuleHasCyclicDependency>(result.errors[1]);
    REQUIRE(ce2);
    CHECK_EQ(result.errors[1].moduleName, "game/Gui/Modules/A");
    REQUIRE_EQ(ce2->cycle.size(), 2);
    CHECK_EQ(ce2->cycle[0], "game/Gui/Modules/B");
    CHECK_EQ(ce2->cycle[1], "game/Gui/Modules/A");
}

TEST_CASE_FIXTURE(FrontendFixture, "dont_reparse_clean_file_when_linting")
{
    fileResolver.source["Modules/A"] = R"(
        local t = {}

        for i=#t,1 do
        end

        for i=#t,1,-1 do
        end
    )";

    frontend.check("Modules/A");

    fileResolver.source["Modules/A"] = R"(
        -- We have fixed the lint error, but we did not tell the Frontend that the file is changed!
        -- Therefore, we expect Frontend to reuse the parse tree.
    )";

    configResolver.defaultConfig.enabledLint.enableWarning(LintWarning::Code_ForRange);

    LintResult lintResult = frontend.lint("Modules/A");

    CHECK_EQ(1, lintResult.warnings.size());
}

TEST_CASE_FIXTURE(FrontendFixture, "dont_recheck_script_that_hasnt_been_marked_dirty")
{
    fileResolver.source["game/Gui/Modules/A"] = "return {hello=5, world=true}";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A)
        return {b_value = A.hello}
    )";

    frontend.check("game/Gui/Modules/B");

    fileResolver.source["game/Gui/Modules/A"] =
        "Massively incorrect syntax haha oops!  However!  The frontend doesn't know that this file needs reparsing!";

    frontend.check("game/Gui/Modules/B");

    ModulePtr bModule = frontend.moduleResolver.modules["game/Gui/Modules/B"];
    CHECK(bModule->errors.empty());
    Luau::dumpErrors(bModule);
}

TEST_CASE_FIXTURE(FrontendFixture, "recheck_if_dependent_script_is_dirty")
{
    fileResolver.source["game/Gui/Modules/A"] = "return {hello=5, world=true}";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A)
        return {b_value = A.hello}
    )";

    frontend.check("game/Gui/Modules/B");

    fileResolver.source["game/Gui/Modules/A"] = "return {hello='hi!'}";
    frontend.markDirty("game/Gui/Modules/A");

    frontend.check("game/Gui/Modules/B");

    ModulePtr bModule = frontend.moduleResolver.modules["game/Gui/Modules/B"];
    CHECK(bModule->errors.empty());
    Luau::dumpErrors(bModule);

    auto bExports = first(bModule->getModuleScope()->returnType);
    REQUIRE(!!bExports);

    CHECK_EQ("{| b_value: string |}", toString(*bExports));
}

#if 0
// Does not work yet. :(
TEST_CASE_FIXTURE(FrontendFixture, "recheck_if_dependent_script_has_a_parse_error")
{
    fileResolver.source["Modules/A"] = "oh no a syntax error";
    fileResolver.source["Modules/B"] = R"(
        local Modules = {}
        local A = require(Modules.A)
        return {}
    )";

    CheckResult result = frontend.check("Modules/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Modules/A", result.errors[0].moduleName);

    CheckResult result2 = frontend.check("Modules/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result2);
    CHECK_EQ(result2.errors[0], result.errors[0]);
}
#endif

TEST_CASE_FIXTURE(FrontendFixture, "produce_errors_for_unchanged_file_with_a_syntax_error")
{
    fileResolver.source["Modules/A"] = "oh no a blatant syntax error!!";

    CheckResult one = frontend.check("Modules/A");
    CheckResult two = frontend.check("Modules/A");

    CHECK(!one.errors.empty());
    CHECK(!two.errors.empty());
}

TEST_CASE_FIXTURE(FrontendFixture, "produce_errors_for_unchanged_file_with_errors")
{
    fileResolver.source["Modules/A"] = "local p: number = 'oh no a type error'";

    frontend.check("Modules/A");

    fileResolver.source["Modules/A"] = "local p = 4 -- We have fixed the problem, but we didn't tell the frontend, so it will not recheck this file!";
    CheckResult secondResult = frontend.check("Modules/A");

    CHECK_EQ(1, secondResult.errors.size());
}

TEST_CASE_FIXTURE(FrontendFixture, "reports_errors_from_multiple_sources")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
        local a: number = 'oh no a type error'
        return {a=a}
    )";

    fileResolver.source["game/Gui/Modules/B"] = R"(
        local Modules = script.Parent
        local A = require(Modules.A)
        local b: number = 'another one!  This is quite distressing!'
    )";

    CheckResult result = frontend.check("game/Gui/Modules/B");
    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK_EQ("game/Gui/Modules/A", result.errors[0].moduleName);
    CHECK_EQ("game/Gui/Modules/B", result.errors[1].moduleName);
}

TEST_CASE_FIXTURE(FrontendFixture, "report_require_to_nonexistent_file")
{
    fileResolver.source["Modules/A"] = R"(
        local Modules = script
        local B = require(Modules.B)
    )";

    CheckResult result = frontend.check("Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    std::string s = toString(result.errors[0]);
    CHECK_MESSAGE(get<UnknownRequire>(result.errors[0]), "Should have been an UnknownRequire: " << toString(result.errors[0]));
}

TEST_CASE_FIXTURE(FrontendFixture, "ignore_require_to_nonexistent_file")
{
    fileResolver.source["Modules/A"] = R"(
        local Modules = script
        local B = require(Modules.B :: any)
    )";

    CheckResult result = frontend.check("Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(FrontendFixture, "report_syntax_error_in_required_file")
{
    fileResolver.source["Modules/A"] = "oh no a gross breach of syntax";
    fileResolver.source["Modules/B"] = R"(
        local Modules = script.Parent
        local A = require(Modules.A)
    )";

    CheckResult result = frontend.check("Modules/B");
    LUAU_REQUIRE_ERRORS(result);

    CHECK_EQ("Modules/A", result.errors[0].moduleName);

    bool b = std::any_of(begin(result.errors), end(result.errors), [](auto&& e) -> bool {
        return get<SyntaxError>(e);
    });
    if (!b)
    {
        CHECK_MESSAGE(false, "Expected a syntax error!");
        dumpErrors(result);
    }
}

TEST_CASE_FIXTURE(FrontendFixture, "re_report_type_error_in_required_file")
{
    fileResolver.source["Modules/A"] = R"(
        local n: number = 'five'
        return {n=n}
    )";

    fileResolver.source["Modules/B"] = R"(
        local Modules = script.Parent
        local A = require(Modules.A)
        print(A.n)
    )";

    CheckResult result = frontend.check("Modules/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CheckResult result2 = frontend.check("Modules/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result2);

    CHECK_EQ("Modules/A", result.errors[0].moduleName);
}

TEST_CASE_FIXTURE(FrontendFixture, "accumulate_cached_errors")
{
    fileResolver.source["Modules/A"] = R"(
        local n: number = 'five'
        return {n=n}
    )";

    fileResolver.source["Modules/B"] = R"(
        local Modules = script.Parent
        local A = require(Modules.A)
        local b: number = 'seven'
        print(A, b)
    )";

    CheckResult result1 = frontend.check("Modules/B");

    LUAU_REQUIRE_ERROR_COUNT(2, result1);

    CHECK_EQ("Modules/A", result1.errors[0].moduleName);
    CHECK_EQ("Modules/B", result1.errors[1].moduleName);

    CheckResult result2 = frontend.check("Modules/B");

    LUAU_REQUIRE_ERROR_COUNT(2, result2);

    CHECK_EQ("Modules/A", result2.errors[0].moduleName);
    CHECK_EQ("Modules/B", result2.errors[1].moduleName);
}

TEST_CASE_FIXTURE(FrontendFixture, "accumulate_cached_errors_in_consistent_order")
{
    fileResolver.source["Modules/A"] = R"(
        a = 1
        b = 2
        local Modules = script.Parent
        local A = require(Modules.B)
    )";

    fileResolver.source["Modules/B"] = R"(
        d = 3
        e = 4
        return {}
    )";

    CheckResult result1 = frontend.check("Modules/A");

    LUAU_REQUIRE_ERROR_COUNT(4, result1);

    CHECK_EQ("Modules/A", result1.errors[2].moduleName);
    CHECK_EQ("Modules/A", result1.errors[3].moduleName);

    CHECK_EQ("Modules/B", result1.errors[0].moduleName);
    CHECK_EQ("Modules/B", result1.errors[1].moduleName);

    CheckResult result2 = frontend.check("Modules/A");
    CHECK_EQ(4, result2.errors.size());

    for (size_t i = 0; i < result1.errors.size(); ++i)
        CHECK_EQ(result1.errors[i], result2.errors[i]);
}

TEST_CASE_FIXTURE(FrontendFixture, "test_pruneParentSegments")
{
    CHECK_EQ(std::optional<std::string>{"Modules/Enum/ButtonState"},
        pathExprToModuleName("", {"Modules", "LuaApp", "DeprecatedDarkTheme", "Parent", "Parent", "Enum", "ButtonState"}));
    CHECK_EQ(std::optional<std::string>{"workspace/Foo/Bar/Baz"}, pathExprToModuleName("workspace/Foo/Quux", {"script", "Parent", "Bar", "Baz"}));
    CHECK_EQ(std::nullopt, pathExprToModuleName("", {}));
    CHECK_EQ(std::optional<std::string>{"script"}, pathExprToModuleName("", {"script"}));
    CHECK_EQ(std::optional<std::string>{"script/Parent"}, pathExprToModuleName("", {"script", "Parent"}));
    CHECK_EQ(std::optional<std::string>{"script"}, pathExprToModuleName("", {"script", "Parent", "Parent"}));
    CHECK_EQ(std::optional<std::string>{"script"}, pathExprToModuleName("", {"script", "Test", "Parent"}));
    CHECK_EQ(std::optional<std::string>{"script/Parent"}, pathExprToModuleName("", {"script", "Test", "Parent", "Parent"}));
    CHECK_EQ(std::optional<std::string>{"script/Parent"}, pathExprToModuleName("", {"script", "Test", "Parent", "Test", "Parent", "Parent"}));
}

TEST_CASE_FIXTURE(FrontendFixture, "test_lint_uses_correct_config")
{
    fileResolver.source["Module/A"] = R"(
        local t = {}

        for i=#t,1 do
        end
    )";

    configResolver.configFiles["Module/A"].enabledLint.enableWarning(LintWarning::Code_ForRange);

    auto result = frontend.lint("Module/A");
    CHECK_EQ(1, result.warnings.size());

    configResolver.configFiles["Module/A"].enabledLint.disableWarning(LintWarning::Code_ForRange);

    auto result2 = frontend.lint("Module/A");
    CHECK_EQ(0, result2.warnings.size());

    LintOptions overrideOptions;

    overrideOptions.enableWarning(LintWarning::Code_ForRange);
    auto result3 = frontend.lint("Module/A", overrideOptions);
    CHECK_EQ(1, result3.warnings.size());

    overrideOptions.disableWarning(LintWarning::Code_ForRange);
    auto result4 = frontend.lint("Module/A", overrideOptions);
    CHECK_EQ(0, result4.warnings.size());
}

TEST_CASE_FIXTURE(FrontendFixture, "lintFragment")
{
    LintOptions lintOptions;
    lintOptions.enableWarning(LintWarning::Code_ForRange);

    auto [_sourceModule, result] = frontend.lintFragment(R"(
        local t = {}

        for i=#t,1 do
        end

        for i=#t,1,-1 do
        end
    )",
        lintOptions);

    CHECK_EQ(1, result.warnings.size());
    CHECK_EQ(0, result.errors.size());
}

TEST_CASE_FIXTURE(FrontendFixture, "discard_type_graphs")
{
    Frontend fe{&fileResolver, &configResolver, {false}};

    fileResolver.source["Module/A"] = R"(
        local a = {1,2,3,4,5}
    )";

    CheckResult result = fe.check("Module/A");

    ModulePtr module = fe.moduleResolver.getModule("Module/A");

    CHECK_EQ(0, module->internalTypes.typeVars.size());
    CHECK_EQ(0, module->internalTypes.typePacks.size());
    CHECK_EQ(0, module->astTypes.size());
}

TEST_CASE_FIXTURE(FrontendFixture, "it_should_be_safe_to_stringify_errors_when_full_type_graph_is_discarded")
{
    Frontend fe{&fileResolver, &configResolver, {false}};

    fileResolver.source["Module/A"] = R"(
        --!strict
        local a: {Count: number} = {count='five'}
    )";

    CheckResult result = fe.check("Module/A");

    REQUIRE_EQ(1, result.errors.size());

    // When this test fails, it is because the TypeIds needed by the error have been deallocated.
    // It is thus basically impossible to predict what will happen when this assert is evaluated.
    // It could segfault, or you could see weird type names like the empty string or <VALUELESS BY EXCEPTION>
    REQUIRE_EQ(
        "Table type 'a' not compatible with type '{| Count: number |}' because the former is missing field 'Count'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(FrontendFixture, "trace_requires_in_nonstrict_mode")
{
    fileResolver.source["Module/A"] = R"(
        --!nonstrict
        local module = {}

        function module.f(arg: number)
            print('f', arg)
        end

        return module
    )";

    fileResolver.source["Module/B"] = R"(
        --!nonstrict
        local A = require(script.Parent.A)

        print(A.g(5))       -- Key 'g' not found
        print(A.f('five'))  -- Type mismatch number and string
        print(A.f(5))       -- OK
    )";

    CheckResult result = frontend.check("Module/B");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK_EQ(4, result.errors[0].location.begin.line);
    CHECK_EQ(5, result.errors[1].location.begin.line);
}

TEST_CASE_FIXTURE(FrontendFixture, "environments")
{
    ScopePtr testScope = frontend.addEnvironment("test");

    unfreeze(typeChecker.globalTypes);
    loadDefinitionFile(typeChecker, testScope, R"(
        export type Foo = number | string
    )",
        "@test");
    freeze(typeChecker.globalTypes);

    fileResolver.source["A"] = R"(
        --!nonstrict
        local foo: Foo = 1
    )";

    fileResolver.source["B"] = R"(
        --!nonstrict
        local foo: Foo = 1
    )";

    fileResolver.environments["A"] = "test";

    CheckResult resultA = frontend.check("A");
    LUAU_REQUIRE_NO_ERRORS(resultA);

    CheckResult resultB = frontend.check("B");
    LUAU_REQUIRE_ERROR_COUNT(1, resultB);
}

TEST_CASE_FIXTURE(FrontendFixture, "ast_node_at_position")
{
    check(R"(
        local t = {}

        function t:aa() end

        t:
    )");

    SourceModule* module = getMainSourceModule();
    Position pos = module->root->location.end;
    AstNode* node = findNodeAtPosition(*module, pos);

    REQUIRE(node);
    REQUIRE(bool(node->asExpr()));

    ++pos.column;
    AstNode* node2 = findNodeAtPosition(*module, pos);
    CHECK_EQ(node, node2);
}

TEST_CASE_FIXTURE(FrontendFixture, "stats_are_not_reset_between_checks")
{
    fileResolver.source["Module/A"] = R"(
        --!strict
        local B = require(script.Parent.B)
        local foo = B.foo + 1
    )";

    fileResolver.source["Module/B"] = R"(
        --!strict
        return {foo = 1}
    )";

    CheckResult r1 = frontend.check("Module/A");
    LUAU_REQUIRE_NO_ERRORS(r1);

    Frontend::Stats stats1 = frontend.stats;
    CHECK_EQ(2, stats1.files);

    frontend.markDirty("Module/A");
    frontend.markDirty("Module/B");

    CheckResult r2 = frontend.check("Module/A");
    LUAU_REQUIRE_NO_ERRORS(r2);
    Frontend::Stats stats2 = frontend.stats;

    CHECK_EQ(4, stats2.files);
}

TEST_CASE_FIXTURE(FrontendFixture, "clearStats")
{
    fileResolver.source["Module/A"] = R"(
        --!strict
        local B = require(script.Parent.B)
        local foo = B.foo + 1
    )";

    fileResolver.source["Module/B"] = R"(
        --!strict
        return {foo = 1}
    )";

    CheckResult r1 = frontend.check("Module/A");
    LUAU_REQUIRE_NO_ERRORS(r1);

    Frontend::Stats stats1 = frontend.stats;
    CHECK_EQ(2, stats1.files);

    frontend.markDirty("Module/A");
    frontend.markDirty("Module/B");

    frontend.clearStats();
    CheckResult r2 = frontend.check("Module/A");
    LUAU_REQUIRE_NO_ERRORS(r2);
    Frontend::Stats stats2 = frontend.stats;

    CHECK_EQ(2, stats2.files);
}

TEST_CASE_FIXTURE(FrontendFixture, "typecheck_twice_for_ast_types")
{
    ScopedFastFlag sffs("LuauTypeCheckTwice", true);

    fileResolver.source["Module/A"] = R"(
        local a = 1
    )";

    CheckResult result = frontend.check("Module/A");

    ModulePtr module = frontend.moduleResolver.getModule("Module/A");

    REQUIRE_EQ(module->astTypes.size(), 1);
    auto it = module->astTypes.begin();
    CHECK_EQ(toString(it->second), "number");
}

TEST_CASE_FIXTURE(FrontendFixture, "imported_table_modification_2")
{
    frontend.options.retainFullTypeGraphs = false;

    fileResolver.source["Module/A"] = R"(
--!nonstrict
local a = {}
a.x = 1
return a;
    )";

    fileResolver.source["Module/B"] = R"(
--!nonstrict
local a = require(script.Parent.A)
local b = {}
function a:b() end -- this should error, but doesn't
return b
    )";

    fileResolver.source["Module/C"] = R"(
--!nonstrict
local a = require(script.Parent.A)
local b = require(script.Parent.B)
a:b() -- this should error, since A doesn't define a:b()
    )";

    CheckResult resultA = frontend.check("Module/A");
    LUAU_REQUIRE_NO_ERRORS(resultA);

    CheckResult resultB = frontend.check("Module/B");
    // TODO (CLI-45592): this should error, since we shouldn't be adding properties to objects from other modules
    LUAU_REQUIRE_NO_ERRORS(resultB);

    CheckResult resultC = frontend.check("Module/C");
    LUAU_REQUIRE_ERRORS(resultC);
}

// This test does not use TEST_CASE_FIXTURE because we need to set a flag before
// the fixture is constructed.
TEST_CASE("no_use_after_free_with_type_fun_instantiation")
{
    // This flag forces this test to crash if there's a UAF in this code.
    ScopedFastFlag sff_DebugLuauFreezeArena("DebugLuauFreezeArena", true);
    ScopedFastFlag sff_LuauCloneCorrectlyBeforeMutatingTableType("LuauCloneCorrectlyBeforeMutatingTableType", true);

    FrontendFixture fix;

    fix.fileResolver.source["Module/A"] = R"(
export type Foo<V> = typeof(setmetatable({}, {}))
return false;
)";

    fix.fileResolver.source["Module/B"] = R"(
local A = require(script.Parent.A)
export type Foo<V> = A.Foo<V>
return false;
)";

    // We don't care about the result. That we haven't crashed is enough.
    fix.frontend.check("Module/B");
}

TEST_SUITE_END();
