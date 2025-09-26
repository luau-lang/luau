// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/DenseHash.h"
#include "Luau/Frontend.h"
#include "Luau/RequireTracer.h"

#include "Fixture.h"

#include "doctest.h"

#include <algorithm>

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2);
LUAU_FASTFLAG(DebugLuauFreezeArena)
LUAU_FASTFLAG(DebugLuauMagicTypes)
LUAU_FASTFLAG(LuauBatchedExecuteTask)

namespace
{
struct NaiveFileResolver : NullFileResolver
{
    std::optional<ModuleInfo> resolveModule(const ModuleInfo* context, AstExpr* expr) override
    {
        if (AstExprGlobal* g = expr->as<AstExprGlobal>())
        {
            if (g->name == "Modules")
                return ModuleInfo{"Modules"};

            if (g->name == "game")
                return ModuleInfo{"game"};
        }
        else if (AstExprIndexName* i = expr->as<AstExprIndexName>())
        {
            if (context)
                return ModuleInfo{context->name + '/' + i->index.value, context->optional};
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
};

} // namespace

struct FrontendFixture : BuiltinsFixture
{
    FrontendFixture() = default;

    Frontend& getFrontend() override
    {
        if (frontend)
            return *frontend;

        Frontend& f = BuiltinsFixture::getFrontend();
        addGlobalBinding(f.globals, "game", f.builtinTypes->anyType, "@test");
        addGlobalBinding(f.globals, "script", f.builtinTypes->anyType, "@test");
        return *frontend;
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
    CHECK_EQ(1, res.requireList.size());
    CHECK_EQ(res.requireList[0].first, "Modules/Foo/Bar");
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
    CHECK_EQ(1, res.requireList.size());
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
    CHECK_EQ(8, res.requireList.size());
}

TEST_CASE_FIXTURE(FrontendFixture, "automatically_check_dependent_scripts")
{
    fileResolver.source["game/Gui/Modules/A"] = "return {hello=5, world=true}";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A)
        return {b_value = A.hello}
    )";

    getFrontend().check("game/Gui/Modules/B");

    ModulePtr bModule = getFrontend().moduleResolver.getModule("game/Gui/Modules/B");
    REQUIRE(bModule != nullptr);
    CHECK(bModule->errors.empty());
    Luau::dumpErrors(bModule);

    auto bExports = first(bModule->returnType);
    REQUIRE(!!bExports);

    CHECK_EQ("{ b_value: number }", toString(*bExports));
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

    CheckResult result1 = getFrontend().check("game/Gui/Modules/B");
    LUAU_REQUIRE_ERROR_COUNT(4, result1);

    CHECK_MESSAGE(get<ModuleHasCyclicDependency>(result1.errors[0]), "Should have been a ModuleHasCyclicDependency: " << toString(result1.errors[0]));

    CHECK_MESSAGE(get<ModuleHasCyclicDependency>(result1.errors[1]), "Should have been a ModuleHasCyclicDependency: " << toString(result1.errors[1]));

    CHECK_MESSAGE(get<ModuleHasCyclicDependency>(result1.errors[2]), "Should have been a ModuleHasCyclicDependency: " << toString(result1.errors[2]));

    CheckResult result2 = getFrontend().check("game/Gui/Modules/D");
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

    CheckResult result = getFrontend().check("game/Gui/Modules/A");
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

    CheckResult result = getFrontend().check("game/Gui/Modules/C");
    LUAU_REQUIRE_NO_ERRORS(result);

    ModulePtr aModule = getFrontend().moduleResolver.getModule("game/Gui/Modules/A");
    REQUIRE(bool(aModule));

    std::optional<TypeId> aExports = first(aModule->returnType);
    REQUIRE(bool(aExports));

    ModulePtr bModule = getFrontend().moduleResolver.getModule("game/Gui/Modules/B");
    REQUIRE(bool(bModule));

    std::optional<TypeId> bExports = first(bModule->returnType);
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

    CheckResult result = getFrontend().check("game/Gui/Modules/A");
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
        --!strict
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A)
        local B = require(Modules.B)
        return {a=A, b=B}
    )";

    CheckResult result = getFrontend().check("game/Gui/Modules/C");
    LUAU_REQUIRE_NO_ERRORS(result);

    ModulePtr cModule = getFrontend().moduleResolver.getModule("game/Gui/Modules/C");
    REQUIRE(bool(cModule));

    std::optional<TypeId> cExports = first(cModule->returnType);
    REQUIRE(bool(cExports));

    CHECK("{ a: { hello: any }, b: { hello: any } }" == toString(*cExports));
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

    CheckResult result = getFrontend().check("game/Gui/Modules/A");
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

    CheckResult result1 = getFrontend().check("game/Gui/Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(2, result1);

    CHECK_MESSAGE(get<ModuleHasCyclicDependency>(result1.errors[0]), "Should have been a ModuleHasCyclicDependency: " << toString(result1.errors[0]));

    CHECK_MESSAGE(get<ModuleHasCyclicDependency>(result1.errors[1]), "Should have been a ModuleHasCyclicDependency: " << toString(result1.errors[1]));

    fileResolver.source["game/Gui/Modules/B"] = R"(
        return {hello = 42}
    )";
    getFrontend().markDirty("game/Gui/Modules/B");

    CheckResult result2 = getFrontend().check("game/Gui/Modules/A");
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

    CheckResult result = getFrontend().check("game/Gui/Modules/A");
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

TEST_CASE_FIXTURE(FrontendFixture, "cycle_incremental_type_surface")
{
    fileResolver.source["game/A"] = R"(
        return {hello = 2}
    )";

    CheckResult result = getFrontend().check("game/A");
    LUAU_REQUIRE_NO_ERRORS(result);

    fileResolver.source["game/A"] = R"(
        local me = require(game.A)
        return {hello = 2}
    )";
    getFrontend().markDirty("game/A");

    result = getFrontend().check("game/A");
    LUAU_REQUIRE_ERRORS(result);

    auto ty = requireType("game/A", "me");
    CHECK_EQ(toString(ty), "any");
}

TEST_CASE_FIXTURE(FrontendFixture, "cycle_incremental_type_surface_longer")
{
    fileResolver.source["game/A"] = R"(
        return {mod_a = 2}
    )";

    CheckResult result = getFrontend().check("game/A");
    LUAU_REQUIRE_NO_ERRORS(result);

    fileResolver.source["game/B"] = R"(
        local me = require(game.A)
        return {mod_b = 4}
    )";

    result = getFrontend().check("game/B");
    LUAU_REQUIRE_NO_ERRORS(result);

    fileResolver.source["game/A"] = R"(
        local me = require(game.B)
        return {mod_a_prime = 3}
    )";

    getFrontend().markDirty("game/A");
    getFrontend().markDirty("game/B");

    result = getFrontend().check("game/A");
    LUAU_REQUIRE_ERRORS(result);

    TypeId tyA = requireType("game/A", "me");
    CHECK_EQ(toString(tyA), "any");

    result = getFrontend().check("game/B");
    LUAU_REQUIRE_ERRORS(result);

    TypeId tyB = requireType("game/B", "me");
    CHECK_EQ(toString(tyB), "any");
}

TEST_CASE_FIXTURE(FrontendFixture, "cycle_incremental_type_surface_exports")
{
    fileResolver.source["game/A"] = R"(
local b = require(game.B)
export type atype = { x: b.btype }
return {mod_a = 1}
    )";

    fileResolver.source["game/B"] = R"(
export type btype = { x: number }

local function bf()
    local a = require(game.A)
    local bfl : a.atype = nil
    return {bfl.x}
end
return {mod_b = 2}
    )";

    ToStringOptions opts;
    opts.exhaustive = true;

    CheckResult resultA = getFrontend().check("game/A");
    LUAU_REQUIRE_ERRORS(resultA);

    CheckResult resultB = getFrontend().check("game/B");
    LUAU_REQUIRE_ERRORS(resultB);

    TypeId tyB = requireExportedType("game/B", "btype");
    CHECK_EQ(toString(tyB, opts), "{ x: number }");

    TypeId tyA = requireExportedType("game/A", "atype");
    CHECK_EQ(toString(tyA, opts), "{ x: any }");

    getFrontend().markDirty("game/B");
    resultB = getFrontend().check("game/B");
    LUAU_REQUIRE_ERRORS(resultB);

    tyB = requireExportedType("game/B", "btype");
    CHECK_EQ(toString(tyB, opts), "{ x: number }");

    tyA = requireExportedType("game/A", "atype");
    CHECK_EQ(toString(tyA, opts), "{ x: any }");
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

    configResolver.defaultConfig.enabledLint.enableWarning(LintWarning::Code_ForRange);

    lintModule("Modules/A");

    fileResolver.source["Modules/A"] = R"(
        -- We have fixed the lint error, but we did not tell the Frontend that the file is changed!
        -- Therefore, we expect Frontend to reuse the results from previous lint.
    )";

    LintResult lintResult = lintModule("Modules/A");

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

    getFrontend().check("game/Gui/Modules/B");

    fileResolver.source["game/Gui/Modules/A"] =
        "Massively incorrect syntax haha oops!  However!  The getFrontend().doesn't know that this file needs reparsing!";

    getFrontend().check("game/Gui/Modules/B");

    ModulePtr bModule = getFrontend().moduleResolver.getModule("game/Gui/Modules/B");
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

    getFrontend().check("game/Gui/Modules/B");

    fileResolver.source["game/Gui/Modules/A"] = "return {hello='hi!'}";
    getFrontend().markDirty("game/Gui/Modules/A");

    getFrontend().check("game/Gui/Modules/B");

    ModulePtr bModule = getFrontend().moduleResolver.getModule("game/Gui/Modules/B");
    CHECK(bModule->errors.empty());
    Luau::dumpErrors(bModule);

    auto bExports = first(bModule->returnType);
    REQUIRE(!!bExports);

    CHECK_EQ("{ b_value: string }", toString(*bExports));
}

TEST_CASE_FIXTURE(FrontendFixture, "mark_non_immediate_reverse_deps_as_dirty")
{
    fileResolver.source["game/Gui/Modules/A"] = "return {hello=5, world=true}";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        return require(game:GetService('Gui').Modules.A)
    )";
    fileResolver.source["game/Gui/Modules/C"] = R"(
        local Modules = game:GetService('Gui').Modules
        local B = require(Modules.B)
        return {c_value = B.hello}
    )";

    getFrontend().check("game/Gui/Modules/C");

    std::vector<Luau::ModuleName> markedDirty;
    getFrontend().markDirty("game/Gui/Modules/A", &markedDirty);

    REQUIRE(markedDirty.size() == 3);
    CHECK(std::find(markedDirty.begin(), markedDirty.end(), "game/Gui/Modules/A") != markedDirty.end());
    CHECK(std::find(markedDirty.begin(), markedDirty.end(), "game/Gui/Modules/B") != markedDirty.end());
    CHECK(std::find(markedDirty.begin(), markedDirty.end(), "game/Gui/Modules/C") != markedDirty.end());
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

    CheckResult result = getFrontend().check("Modules/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Modules/A", result.errors[0].moduleName);

    CheckResult result2 = getFrontend().check("Modules/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result2);
    CHECK_EQ(result2.errors[0], result.errors[0]);
}
#endif

TEST_CASE_FIXTURE(FrontendFixture, "produce_errors_for_unchanged_file_with_a_syntax_error")
{
    fileResolver.source["Modules/A"] = "oh no a blatant syntax error!!";

    CheckResult one = getFrontend().check("Modules/A");
    CheckResult two = getFrontend().check("Modules/A");

    CHECK(!one.errors.empty());
    CHECK(!two.errors.empty());
}

TEST_CASE_FIXTURE(FrontendFixture, "produce_errors_for_unchanged_file_with_errors")
{
    fileResolver.source["Modules/A"] = "local p: number = 'oh no a type error'";

    getFrontend().check("Modules/A");

    fileResolver.source["Modules/A"] =
        "local p = 4 -- We have fixed the problem, but we didn't tell the getFrontend(). so it will not recheck this file!";
    CheckResult secondResult = getFrontend().check("Modules/A");

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

    CheckResult result = getFrontend().check("game/Gui/Modules/B");
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

    CheckResult result = getFrontend().check("Modules/A");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    std::string s = toString(result.errors[0]);
    CHECK_MESSAGE(get<UnknownRequire>(result.errors[0]), "Should have been an UnknownRequire: " << toString(result.errors[0]));
}

TEST_CASE_FIXTURE(FrontendFixture, "ignore_require_to_nonexistent_file")
{
    fileResolver.source["Modules/A"] = R"(
        local Modules = script
        local B = require(Modules.B) :: any
    )";

    CheckResult result = getFrontend().check("Modules/A");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(FrontendFixture, "report_syntax_error_in_required_file")
{
    fileResolver.source["Modules/A"] = "oh no a gross breach of syntax";
    fileResolver.source["Modules/B"] = R"(
        local Modules = script.Parent
        local A = require(Modules.A)
    )";

    CheckResult result = getFrontend().check("Modules/B");
    LUAU_REQUIRE_ERRORS(result);

    CHECK_EQ("Modules/A", result.errors[0].moduleName);

    bool b = std::any_of(
        begin(result.errors),
        end(result.errors),
        [](auto&& e) -> bool
        {
            return get<SyntaxError>(e);
        }
    );
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

    CheckResult result = getFrontend().check("Modules/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CheckResult result2 = getFrontend().check("Modules/B");
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

    CheckResult result1 = getFrontend().check("Modules/B");

    LUAU_REQUIRE_ERROR_COUNT(2, result1);

    CHECK_EQ("Modules/A", result1.errors[0].moduleName);
    CHECK_EQ("Modules/B", result1.errors[1].moduleName);

    CheckResult result2 = getFrontend().check("Modules/B");

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

    CheckResult result1 = getFrontend().check("Modules/A");

    LUAU_REQUIRE_ERROR_COUNT(4, result1);

    CHECK_EQ("Modules/A", result1.errors[2].moduleName);
    CHECK_EQ("Modules/A", result1.errors[3].moduleName);

    CHECK_EQ("Modules/B", result1.errors[0].moduleName);
    CHECK_EQ("Modules/B", result1.errors[1].moduleName);

    CheckResult result2 = getFrontend().check("Modules/A");
    CHECK_EQ(4, result2.errors.size());

    for (size_t i = 0; i < result1.errors.size(); ++i)
        CHECK_EQ(result1.errors[i], result2.errors[i]);
}

TEST_CASE_FIXTURE(FrontendFixture, "test_pruneParentSegments")
{
    CHECK_EQ(
        std::optional<std::string>{"Modules/Enum/ButtonState"},
        pathExprToModuleName("", {"Modules", "LuaApp", "DeprecatedDarkTheme", "Parent", "Parent", "Enum", "ButtonState"})
    );
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

    auto result = lintModule("Module/A");
    CHECK_EQ(1, result.warnings.size());

    configResolver.configFiles["Module/A"].enabledLint.disableWarning(LintWarning::Code_ForRange);
    getFrontend().markDirty("Module/A");

    auto result2 = lintModule("Module/A");
    CHECK_EQ(0, result2.warnings.size());

    LintOptions overrideOptions;

    overrideOptions.enableWarning(LintWarning::Code_ForRange);
    getFrontend().markDirty("Module/A");

    auto result3 = lintModule("Module/A", overrideOptions);
    CHECK_EQ(1, result3.warnings.size());

    overrideOptions.disableWarning(LintWarning::Code_ForRange);
    getFrontend().markDirty("Module/A");

    auto result4 = lintModule("Module/A", overrideOptions);
    CHECK_EQ(0, result4.warnings.size());
}

TEST_CASE_FIXTURE(FrontendFixture, "lint_results_are_only_for_checked_module")
{
    fileResolver.source["Module/A"] = R"(
local _ = 0b10000000000000000000000000000000000000000000000000000000000000000
    )";

    fileResolver.source["Module/B"] = R"(
require(script.Parent.A)
local _ = 0x10000000000000000
    )";

    LintResult lintResult = lintModule("Module/B");
    CHECK_EQ(1, lintResult.warnings.size());

    // Check cached result
    lintResult = lintModule("Module/B");
    CHECK_EQ(1, lintResult.warnings.size());
}

TEST_CASE_FIXTURE(FrontendFixture, "discard_type_graphs")
{
    Frontend fe{&fileResolver, &configResolver, {false}};

    fileResolver.source["Module/A"] = R"(
        local a = {1,2,3,4,5}
    )";

    CheckResult result = fe.check("Module/A");

    ModulePtr module = fe.moduleResolver.getModule("Module/A");

    CHECK_EQ(0, module->internalTypes.types.size());
    CHECK_EQ(0, module->internalTypes.typePacks.size());
    CHECK_EQ(0, module->astTypes.size());
    CHECK_EQ(0, module->astResolvedTypes.size());
    CHECK_EQ(0, module->astResolvedTypePacks.size());
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
    if (FFlag::LuauSolverV2)
    {
        CHECK_EQ(
            "Table type '{ count: string }' not compatible with type '{ Count: number }' because the former is missing field 'Count'",
            toString(result.errors[0])
        );
    }
    else
        REQUIRE_EQ(
            "Table type 'a' not compatible with type '{ Count: number }' because the former is missing field 'Count'", toString(result.errors[0])
        );
}

TEST_CASE_FIXTURE(FrontendFixture, "trace_requires_in_nonstrict_mode")
{
    // The new non-strict mode is not currently expected to signal any errors here.
    if (FFlag::LuauSolverV2)
        return;

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

    CheckResult result = getFrontend().check("Module/B");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK_EQ(4, result.errors[0].location.begin.line);
    CHECK_EQ(5, result.errors[1].location.begin.line);
}

TEST_CASE_FIXTURE(FrontendFixture, "environments")
{
    ScopePtr testScope = getFrontend().addEnvironment("test");

    unfreeze(getFrontend().globals.globalTypes);
    getFrontend().loadDefinitionFile(
        getFrontend().globals,
        testScope,
        R"(
        export type Foo = number | string
    )",
        "@test",
        /* captureComments */ false
    );
    freeze(getFrontend().globals.globalTypes);

    fileResolver.source["A"] = R"(
        --!nonstrict
        local foo: Foo = 1
    )";

    fileResolver.source["B"] = R"(
        --!nonstrict
        local foo: Foo = 1
    )";

    fileResolver.source["C"] = R"(
        --!strict
        local foo: Foo = 1
    )";

    fileResolver.environments["A"] = "test";

    CheckResult resultA = getFrontend().check("A");
    LUAU_REQUIRE_NO_ERRORS(resultA);

    CheckResult resultB = getFrontend().check("B");
    LUAU_REQUIRE_ERROR_COUNT(1, resultB);

    CheckResult resultC = getFrontend().check("C");
    LUAU_REQUIRE_ERROR_COUNT(1, resultC);
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

    CheckResult r1 = getFrontend().check("Module/A");
    LUAU_REQUIRE_NO_ERRORS(r1);

    Frontend::Stats stats1 = getFrontend().stats;
    CHECK_EQ(2, stats1.files);

    getFrontend().markDirty("Module/A");
    getFrontend().markDirty("Module/B");

    CheckResult r2 = getFrontend().check("Module/A");
    LUAU_REQUIRE_NO_ERRORS(r2);
    Frontend::Stats stats2 = getFrontend().stats;

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

    CheckResult r1 = getFrontend().check("Module/A");
    LUAU_REQUIRE_NO_ERRORS(r1);

    Frontend::Stats stats1 = getFrontend().stats;
    CHECK_EQ(2, stats1.files);

    getFrontend().markDirty("Module/A");
    getFrontend().markDirty("Module/B");

    getFrontend().clearStats();
    CheckResult r2 = getFrontend().check("Module/A");
    LUAU_REQUIRE_NO_ERRORS(r2);
    Frontend::Stats stats2 = getFrontend().stats;

    CHECK_EQ(2, stats2.files);
}

TEST_CASE_FIXTURE(FrontendFixture, "typecheck_twice_for_ast_types")
{
    fileResolver.source["Module/A"] = R"(
        local a = 1
    )";

    CheckResult result = getFrontend().check("Module/A");

    ModulePtr module = getFrontend().moduleResolver.getModule("Module/A");

    REQUIRE_EQ(module->astTypes.size(), 1);
    auto it = module->astTypes.begin();
    CHECK_EQ(toString(it->second), "number");
}

TEST_CASE_FIXTURE(FrontendFixture, "imported_table_modification_2")
{
    // This test describes non-strict mode behavior that is just not currently present in the new non-strict mode.
    if (FFlag::LuauSolverV2)
        return;

    getFrontend().options.retainFullTypeGraphs = false;

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
function a:b() end -- this should error, since A doesn't define a:b()
return b
    )";

    fileResolver.source["Module/C"] = R"(
--!nonstrict
local a = require(script.Parent.A)
local b = require(script.Parent.B)
a:b() -- this should error, since A doesn't define a:b()
    )";

    CheckResult resultA = getFrontend().check("Module/A");
    LUAU_REQUIRE_NO_ERRORS(resultA);

    CheckResult resultB = getFrontend().check("Module/B");
    LUAU_REQUIRE_ERRORS(resultB);

    CheckResult resultC = getFrontend().check("Module/C");
    LUAU_REQUIRE_ERRORS(resultC);
}

// This test does not use TEST_CASE_FIXTURE because we need to set a flag before
// the fixture is constructed.
TEST_CASE("no_use_after_free_with_type_fun_instantiation")
{
    // This flag forces this test to crash if there's a UAF in this code.
    ScopedFastFlag sff_DebugLuauFreezeArena(FFlag::DebugLuauFreezeArena, true);

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
    fix.getFrontend().check("Module/B");
}

TEST_CASE("check_without_builtin_next")
{
    TestFileResolver fileResolver;
    TestConfigResolver configResolver;
    Frontend frontend(&fileResolver, &configResolver);
    fileResolver.source["Module/A"] = "for k,v in 2 do end";
    fileResolver.source["Module/B"] = "return next";

    // We don't care about the result. That we haven't crashed is enough.
    frontend.check("Module/A");
    frontend.check("Module/B");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "reexport_cyclic_type")
{
    fileResolver.source["Module/A"] = R"(
        type F<T> = (set: G<T>) -> ()

        export type G<T> = {
            forEach: (a: F<T>) -> (),
        }

        function X<T>(a: F<T>): ()
        end

        return X
    )";

    fileResolver.source["Module/B"] = R"(
        --!strict
        local A = require(script.Parent.A)

        export type G<T> = A.G<T>

        return {
            A = A,
        }
    )";

    CheckResult result = getFrontend().check("Module/B");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "reexport_type_alias")
{
    fileResolver.source["Module/A"] = R"(
        type KeyOfTestEvents = "test-file-start" | "test-file-success" | "test-file-failure" | "test-case-result"
        type MyAny = any

        export type TestFileEvent<T = KeyOfTestEvents> = (
            eventName: T,
            args: any --[[ ROBLOX TODO: Unhandled node for type: TSIndexedAccessType ]] --[[ TestEvents[T] ]]
        ) -> MyAny

        return {}
    )";

    fileResolver.source["Module/B"] = R"(
        --!strict
        local A = require(script.Parent.A)

        export type TestFileEvent = A.TestFileEvent
    )";

    CheckResult result = getFrontend().check("Module/B");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "module_scope_check")
{
    getFrontend().prepareModuleScope = [this](const ModuleName& name, const ScopePtr& scope, bool forAutocomplete)
    {
        scope->bindings[Luau::AstName{"x"}] = Luau::Binding{getFrontend().globals.builtinTypes->numberType};
    };

    fileResolver.source["game/A"] = R"(
        local a = x
    )";

    CheckResult result = getFrontend().check("game/A");
    LUAU_REQUIRE_NO_ERRORS(result);

    auto ty = requireType("game/A", "a");
    CHECK_EQ(toString(ty), "number");
}

TEST_CASE_FIXTURE(FrontendFixture, "parse_only")
{
    fileResolver.source["game/Gui/Modules/A"] = R"(
        local a: number = 'oh no a type error'
        return {a=a}
    )";

    fileResolver.source["game/Gui/Modules/B"] = R"(
        local Modules = script.Parent
        local A = require(Modules.A)
        local b: number = 2
    )";

    getFrontend().parse("game/Gui/Modules/B");

    REQUIRE(getFrontend().sourceNodes.count("game/Gui/Modules/A"));
    REQUIRE(getFrontend().sourceNodes.count("game/Gui/Modules/B"));

    auto node = getFrontend().sourceNodes["game/Gui/Modules/B"];
    CHECK(node->requireSet.contains("game/Gui/Modules/A"));
    REQUIRE_EQ(node->requireLocations.size(), 1);
    CHECK_EQ(node->requireLocations[0].second, Luau::Location(Position(2, 18), Position(2, 36)));

    // Early parse doesn't cause typechecking to be skipped
    CheckResult result = getFrontend().check("game/Gui/Modules/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("game/Gui/Modules/A", result.errors[0].moduleName);
    CHECK_EQ("Type 'string' could not be converted into 'number'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(FrontendFixture, "markdirty_early_return")
{
    constexpr char moduleName[] = "game/Gui/Modules/A";
    fileResolver.source[moduleName] = R"(
        return 1
    )";

    {
        std::vector<ModuleName> markedDirty;
        getFrontend().markDirty(moduleName, &markedDirty);
        CHECK(markedDirty.empty());
    }

    getFrontend().parse(moduleName);

    {
        std::vector<ModuleName> markedDirty;
        getFrontend().markDirty(moduleName, &markedDirty);
        CHECK(!markedDirty.empty());
    }
}

TEST_CASE_FIXTURE(FrontendFixture, "attribute_ices_to_the_correct_module")
{
    ScopedFastFlag sff{FFlag::DebugLuauMagicTypes, true};

    fileResolver.source["game/one"] = R"(
        require(game.two)
    )";

    fileResolver.source["game/two"] = R"(
        local a: _luau_ice
    )";

    try
    {
        getFrontend().check("game/one");
    }
    catch (InternalCompilerError& err)
    {
        CHECK("game/two" == err.moduleName);
        return;
    }

    FAIL("Expected an InternalCompilerError!");
}

TEST_CASE_FIXTURE(FrontendFixture, "checked_modules_have_the_correct_mode")
{
    fileResolver.source["game/A"] = R"(
        --!nocheck
        local a: number = "five"
    )";

    fileResolver.source["game/B"] = R"(
        --!nonstrict
        local a = math.abs("five")
    )";

    fileResolver.source["game/C"] = R"(
        --!strict
        local a = 10
    )";

    getFrontend().check("game/A");
    getFrontend().check("game/B");
    getFrontend().check("game/C");

    ModulePtr moduleA = getFrontend().moduleResolver.getModule("game/A");
    REQUIRE(moduleA);
    CHECK(moduleA->mode == Mode::NoCheck);

    ModulePtr moduleB = getFrontend().moduleResolver.getModule("game/B");
    REQUIRE(moduleB);
    CHECK(moduleB->mode == Mode::Nonstrict);

    ModulePtr moduleC = getFrontend().moduleResolver.getModule("game/C");
    REQUIRE(moduleC);
    CHECK(moduleC->mode == Mode::Strict);
}

TEST_CASE_FIXTURE(FrontendFixture, "separate_caches_for_autocomplete")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    fileResolver.source["game/A"] = R"(
        --!nonstrict
        local exports = {}
        function exports.hello() end
        return exports
    )";

    FrontendOptions opts;
    opts.forAutocomplete = true;
    getFrontend().setLuauSolverMode(FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old);
    getFrontend().check("game/A", opts);

    CHECK(nullptr == getFrontend().moduleResolver.getModule("game/A"));

    ModulePtr acModule = getFrontend().moduleResolverForAutocomplete.getModule("game/A");
    REQUIRE(acModule != nullptr);
    CHECK(acModule->mode == Mode::Strict);

    getFrontend().check("game/A");

    ModulePtr module = getFrontend().moduleResolver.getModule("game/A");

    REQUIRE(module != nullptr);
    CHECK(module->mode == Mode::Nonstrict);
}

TEST_CASE_FIXTURE(FrontendFixture, "no_separate_caches_with_the_new_solver")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    fileResolver.source["game/A"] = R"(
        --!nonstrict
        local exports = {}
        function exports.hello() end
        return exports
    )";

    FrontendOptions opts;
    opts.forAutocomplete = true;

    getFrontend().check("game/A", opts);

    CHECK(nullptr == getFrontend().moduleResolverForAutocomplete.getModule("game/A"));

    ModulePtr module = getFrontend().moduleResolver.getModule("game/A");

    REQUIRE(module != nullptr);
    CHECK(module->mode == Mode::Nonstrict);
}

TEST_CASE_FIXTURE(Fixture, "exported_tables_have_position_metadata")
{
    CheckResult result = check(R"(
        return { abc = 22 }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    ModulePtr mm = getMainModule();

    TypePackId retTp = mm->getModuleScope()->returnType;
    auto retHead = flatten(retTp).first;
    REQUIRE(1 == retHead.size());

    const TableType* tt = get<TableType>(retHead[0]);
    REQUIRE(tt);

    CHECK("MainModule" == tt->definitionModuleName);

    CHECK(1 == tt->props.size());
    CHECK(tt->props.count("abc"));

    const Property& prop = tt->props.find("abc")->second;

    CHECK(Location{Position{1, 17}, Position{1, 20}} == prop.location);
}

TEST_CASE_FIXTURE(FrontendFixture, "get_required_scripts")
{
    fileResolver.source["game/workspace/MyScript"] = R"(
        local MyModuleScript = require(game.workspace.MyModuleScript)
        local MyModuleScript2 = require(game.workspace.MyModuleScript2)
        MyModuleScript.myPrint()
    )";

    fileResolver.source["game/workspace/MyModuleScript"] = R"(
        local module = {}
        function module.myPrint()
            print("Hello World")
        end
        return module
    )";

    fileResolver.source["game/workspace/MyModuleScript2"] = R"(
        local module = {}
        return module
    )";

    // isDirty(name) is true, getRequiredScripts should not hit the cache.
    getFrontend().markDirty("game/workspace/MyScript");
    std::vector<ModuleName> requiredScripts = getFrontend().getRequiredScripts("game/workspace/MyScript");
    REQUIRE(requiredScripts.size() == 2);
    CHECK(requiredScripts[0] == "game/workspace/MyModuleScript");
    CHECK(requiredScripts[1] == "game/workspace/MyModuleScript2");

    // Call getFrontend().check first, then getRequiredScripts should hit the cache because isDirty(name) is false.
    getFrontend().check("game/workspace/MyScript");
    requiredScripts = getFrontend().getRequiredScripts("game/workspace/MyScript");
    REQUIRE(requiredScripts.size() == 2);
    CHECK(requiredScripts[0] == "game/workspace/MyModuleScript");
    CHECK(requiredScripts[1] == "game/workspace/MyModuleScript2");
}

TEST_CASE_FIXTURE(FrontendFixture, "get_required_scripts_dirty")
{
    fileResolver.source["game/workspace/MyScript"] = R"(
        print("Hello World")
    )";

    fileResolver.source["game/workspace/MyModuleScript"] = R"(
        local module = {}
        function module.myPrint()
            print("Hello World")
        end
        return module
    )";

    getFrontend().check("game/workspace/MyScript");
    std::vector<ModuleName> requiredScripts = getFrontend().getRequiredScripts("game/workspace/MyScript");
    REQUIRE(requiredScripts.size() == 0);

    fileResolver.source["game/workspace/MyScript"] = R"(
        local MyModuleScript = require(game.workspace.MyModuleScript)
        MyModuleScript.myPrint()
    )";

    requiredScripts = getFrontend().getRequiredScripts("game/workspace/MyScript");
    REQUIRE(requiredScripts.size() == 0);

    getFrontend().markDirty("game/workspace/MyScript");
    requiredScripts = getFrontend().getRequiredScripts("game/workspace/MyScript");
    REQUIRE(requiredScripts.size() == 1);
    CHECK(requiredScripts[0] == "game/workspace/MyModuleScript");
}

TEST_CASE_FIXTURE(FrontendFixture, "check_module_references_allocator")
{
    fileResolver.source["game/workspace/MyScript"] = R"(
        print("Hello World")
    )";

    getFrontend().check("game/workspace/MyScript");

    ModulePtr module = getFrontend().moduleResolver.getModule("game/workspace/MyScript");
    SourceModule* source = getFrontend().getSourceModule("game/workspace/MyScript");
    CHECK(module);
    CHECK(source);

    CHECK_EQ(module->allocator.get(), source->allocator.get());
    CHECK_EQ(module->names.get(), source->names.get());
}

TEST_CASE_FIXTURE(FrontendFixture, "check_module_references_correct_ast_root")
{
    fileResolver.source["game/workspace/MyScript"] = R"(
        print("Hello World")
    )";

    getFrontend().check("game/workspace/MyScript");

    ModulePtr module = getFrontend().moduleResolver.getModule("game/workspace/MyScript");
    SourceModule* source = getFrontend().getSourceModule("game/workspace/MyScript");
    CHECK(module);
    CHECK(source);

    CHECK_EQ(module->root, source->root);
}

TEST_CASE_FIXTURE(FrontendFixture, "dfg_data_cleared_on_retain_type_graphs_unset")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
    fileResolver.source["game/A"] = R"(
local a = 1
local b = 2
local c = 3
return {x = a, y = b, z = c}
)";

    getFrontend().options.retainFullTypeGraphs = true;
    getFrontend().check("game/A");

    auto mod = getFrontend().moduleResolver.getModule("game/A");
    CHECK(!mod->defArena.allocator.empty());
    CHECK(!mod->keyArena.allocator.empty());

    // We should check that the dfg arena is empty once retainFullTypeGraphs is unset
    getFrontend().options.retainFullTypeGraphs = false;
    getFrontend().markDirty("game/A");
    getFrontend().check("game/A");

    mod = getFrontend().moduleResolver.getModule("game/A");
    CHECK(mod->defArena.allocator.empty());
    CHECK(mod->keyArena.allocator.empty());
}

TEST_CASE_FIXTURE(FrontendFixture, "test_traverse_dependents")
{
    fileResolver.source["game/Gui/Modules/A"] = "return {hello=5, world=true}";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        return require(game:GetService('Gui').Modules.A)
    )";
    fileResolver.source["game/Gui/Modules/C"] = R"(
        local Modules = game:GetService('Gui').Modules
        local B = require(Modules.B)
        return {c_value = B.hello}
    )";
    fileResolver.source["game/Gui/Modules/D"] = R"(
        local Modules = game:GetService('Gui').Modules
        local C = require(Modules.C)
        return {d_value = C.c_value}
    )";

    getFrontend().check("game/Gui/Modules/D");

    std::vector<ModuleName> visited;
    getFrontend().traverseDependents(
        "game/Gui/Modules/B",
        [&visited](SourceNode& node)
        {
            visited.push_back(node.name);
            return true;
        }
    );

    CHECK_EQ(std::vector<ModuleName>{"game/Gui/Modules/B", "game/Gui/Modules/C", "game/Gui/Modules/D"}, visited);
}

TEST_CASE_FIXTURE(FrontendFixture, "test_traverse_dependents_early_exit")
{
    fileResolver.source["game/Gui/Modules/A"] = "return {hello=5, world=true}";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        return require(game:GetService('Gui').Modules.A)
    )";
    fileResolver.source["game/Gui/Modules/C"] = R"(
        local Modules = game:GetService('Gui').Modules
        local B = require(Modules.B)
        return {c_value = B.hello}
    )";

    getFrontend().check("game/Gui/Modules/C");

    std::vector<ModuleName> visited;
    getFrontend().traverseDependents(
        "game/Gui/Modules/A",
        [&visited](SourceNode& node)
        {
            visited.push_back(node.name);
            return node.name != "game/Gui/Modules/B";
        }
    );

    CHECK_EQ(std::vector<ModuleName>{"game/Gui/Modules/A", "game/Gui/Modules/B"}, visited);
}

TEST_CASE_FIXTURE(FrontendFixture, "test_dependents_stored_on_node_as_graph_updates")
{
    auto updateSource = [&](const std::string& name, const std::string& source)
    {
        fileResolver.source[name] = source;
        getFrontend().markDirty(name);
    };

    auto validateMatchesRequireLists = [&](const std::string& message)
    {
        DenseHashMap<ModuleName, std::vector<ModuleName>> dependents{{}};
        for (const auto& module : getFrontend().sourceNodes)
        {
            for (const auto& dep : module.second->requireSet)
                dependents[dep].push_back(module.first);
        }

        for (const auto& module : getFrontend().sourceNodes)
        {
            Set<ModuleName>& dependentsForModule = module.second->dependents;
            for (const auto& dep : dependents[module.first])
                CHECK_MESSAGE(1 == dependentsForModule.count(dep), "Mismatch in dependents for " << module.first << ": " << message);
        }
    };

    auto validateSecondDependsOnFirst = [&](const std::string& from, const std::string& to, bool expected)
    {
        SourceNode& fromNode = *getFrontend().sourceNodes[from];
        CHECK_MESSAGE(
            fromNode.dependents.count(to) == int(expected),
            "Expected " << from << " to " << (expected ? std::string() : std::string("not ")) << "have a reverse dependency on " << to
        );
    };

    // C -> B -> A
    {
        updateSource("game/Gui/Modules/A", "return {hello=5, world=true}");
        updateSource("game/Gui/Modules/B", R"(
            return require(game:GetService('Gui').Modules.A)
        )");
        updateSource("game/Gui/Modules/C", R"(
            local Modules = game:GetService('Gui').Modules
            local B = require(Modules.B)
            return {c_value = B}
        )");
        getFrontend().check("game/Gui/Modules/C");

        validateMatchesRequireLists("Initial check");

        validateSecondDependsOnFirst("game/Gui/Modules/A", "game/Gui/Modules/B", true);
        validateSecondDependsOnFirst("game/Gui/Modules/B", "game/Gui/Modules/C", true);
        validateSecondDependsOnFirst("game/Gui/Modules/C", "game/Gui/Modules/A", false);
    }

    // C -> B, A
    {
        updateSource("game/Gui/Modules/B", R"(
            return 1
        )");
        getFrontend().check("game/Gui/Modules/C");

        validateMatchesRequireLists("Removing dependency B->A");
        validateSecondDependsOnFirst("game/Gui/Modules/A", "game/Gui/Modules/B", false);
    }

    // C -> B -> A
    {
        updateSource("game/Gui/Modules/B", R"(
            return require(game:GetService('Gui').Modules.A)
        )");
        getFrontend().check("game/Gui/Modules/C");

        validateMatchesRequireLists("Adding back B->A");
        validateSecondDependsOnFirst("game/Gui/Modules/A", "game/Gui/Modules/B", true);
    }

    // C -> B -> A, D -> (C,B,A)
    {
        updateSource("game/Gui/Modules/D", R"(
            local C = require(game:GetService('Gui').Modules.C)
            local B = require(game:GetService('Gui').Modules.B)
            local A = require(game:GetService('Gui').Modules.A)
            return {d_value = C.c_value}
        )");
        getFrontend().check("game/Gui/Modules/D");

        validateMatchesRequireLists("Adding D->C, D->B, D->A");
        validateSecondDependsOnFirst("game/Gui/Modules/A", "game/Gui/Modules/D", true);
        validateSecondDependsOnFirst("game/Gui/Modules/B", "game/Gui/Modules/D", true);
        validateSecondDependsOnFirst("game/Gui/Modules/C", "game/Gui/Modules/D", true);
    }

    // B -> A, C <-> D
    {
        updateSource("game/Gui/Modules/D", "return require(game:GetService('Gui').Modules.C)");
        updateSource("game/Gui/Modules/C", "return require(game:GetService('Gui').Modules.D)");
        getFrontend().check("game/Gui/Modules/D");

        validateMatchesRequireLists("Adding cycle D->C, C->D");
        validateSecondDependsOnFirst("game/Gui/Modules/C", "game/Gui/Modules/D", true);
        validateSecondDependsOnFirst("game/Gui/Modules/D", "game/Gui/Modules/C", true);
    }

    // B -> A, C -> D, D -> error
    {
        updateSource("game/Gui/Modules/D", "return require(game:GetService('Gui').Modules.C.)");
        getFrontend().check("game/Gui/Modules/D");

        validateMatchesRequireLists("Adding error dependency D->C.");
        validateSecondDependsOnFirst("game/Gui/Modules/D", "game/Gui/Modules/C", true);
        validateSecondDependsOnFirst("game/Gui/Modules/C", "game/Gui/Modules/D", false);
    }
}

TEST_CASE_FIXTURE(FrontendFixture, "test_invalid_dependency_tracking_per_module_resolver")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, false};
    getFrontend().setLuauSolverMode(FFlag::LuauSolverV2 ? SolverMode::New : SolverMode::Old);

    fileResolver.source["game/Gui/Modules/A"] = "return {hello=5, world=true}";
    fileResolver.source["game/Gui/Modules/B"] = "return require(game:GetService('Gui').Modules.A)";

    FrontendOptions opts;
    opts.forAutocomplete = false;

    getFrontend().check("game/Gui/Modules/B", opts);
    CHECK(getFrontend().allModuleDependenciesValid("game/Gui/Modules/B", opts.forAutocomplete));
    CHECK(!getFrontend().allModuleDependenciesValid("game/Gui/Modules/B", !opts.forAutocomplete));

    opts.forAutocomplete = true;
    getFrontend().check("game/Gui/Modules/A", opts);

    CHECK(!getFrontend().allModuleDependenciesValid("game/Gui/Modules/B", opts.forAutocomplete));
    CHECK(getFrontend().allModuleDependenciesValid("game/Gui/Modules/B", !opts.forAutocomplete));
    CHECK(getFrontend().allModuleDependenciesValid("game/Gui/Modules/A", !opts.forAutocomplete));
    CHECK(getFrontend().allModuleDependenciesValid("game/Gui/Modules/A", opts.forAutocomplete));
}

TEST_CASE_FIXTURE(FrontendFixture, "queue_check_simple")
{
    ScopedFastFlag luauBatchedExecuteTask{FFlag::LuauBatchedExecuteTask, true};

    fileResolver.source["game/Gui/Modules/A"] = R"(
        --!strict
        return {hello=5, world=true}
    )";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        --!strict
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A)
        return {b_value = A.hello}
    )";

    getFrontend().queueModuleCheck("game/Gui/Modules/B");
    getFrontend().checkQueuedModules();

    auto result = getFrontend().getCheckResult("game/Gui/Modules/B", true);
    REQUIRE(result);
    LUAU_REQUIRE_NO_ERRORS(*result);
}

TEST_CASE_FIXTURE(FrontendFixture, "queue_check_cycle_instant")
{
    ScopedFastFlag luauBatchedExecuteTask{FFlag::LuauBatchedExecuteTask, true};

    fileResolver.source["game/Gui/Modules/A"] = R"(
        --!strict
        local Modules = game:GetService('Gui').Modules
        local B = require(Modules.B)
        return {a_value = B.hello}
    )";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        --!strict
        local Modules = game:GetService('Gui').Modules
        local A = require(Modules.A)
        return {b_value = A.hello}
    )";

    getFrontend().queueModuleCheck("game/Gui/Modules/B");
    getFrontend().checkQueuedModules();

    auto result = getFrontend().getCheckResult("game/Gui/Modules/B", true);
    REQUIRE(result);
    LUAU_REQUIRE_ERROR_COUNT(2, *result);
    CHECK(toString(result->errors[0]) == "Cyclic module dependency: game/Gui/Modules/B -> game/Gui/Modules/A");
    CHECK(toString(result->errors[1]) == "Cyclic module dependency: game/Gui/Modules/A -> game/Gui/Modules/B");
}

TEST_CASE_FIXTURE(FrontendFixture, "queue_check_cycle_delayed")
{
    ScopedFastFlag luauBatchedExecuteTask{FFlag::LuauBatchedExecuteTask, true};

    fileResolver.source["game/Gui/Modules/C"] = R"(
        --!strict
        return {c_value = 5}
    )";
    fileResolver.source["game/Gui/Modules/A"] = R"(
        --!strict
        local Modules = game:GetService('Gui').Modules
        local C = require(Modules.C)
        local B = require(Modules.B)
        return {a_value = B.hello + C.c_value}
    )";
    fileResolver.source["game/Gui/Modules/B"] = R"(
        --!strict
        local Modules = game:GetService('Gui').Modules
        local C = require(Modules.C)
        local A = require(Modules.A)
        return {b_value = A.hello + C.c_value}
    )";

    getFrontend().queueModuleCheck("game/Gui/Modules/B");
    getFrontend().checkQueuedModules();

    auto result = getFrontend().getCheckResult("game/Gui/Modules/B", true);
    REQUIRE(result);
    LUAU_REQUIRE_ERROR_COUNT(2, *result);
    CHECK(toString(result->errors[0]) == "Cyclic module dependency: game/Gui/Modules/B -> game/Gui/Modules/A");
    CHECK(toString(result->errors[1]) == "Cyclic module dependency: game/Gui/Modules/A -> game/Gui/Modules/B");
}

TEST_CASE_FIXTURE(FrontendFixture, "queue_check_propagates_ice")
{
    ScopedFastFlag luauBatchedExecuteTask{FFlag::LuauBatchedExecuteTask, true};
    ScopedFastFlag sffs{FFlag::DebugLuauMagicTypes, true};

    ModuleName mm = fromString("MainModule");
    fileResolver.source[mm] = R"(
        --!strict
        local a: _luau_ice = 55
    )";
    getFrontend().markDirty(mm);
    getFrontend().queueModuleCheck("MainModule");

    CHECK_THROWS_AS(getFrontend().checkQueuedModules(), InternalCompilerError);
}

TEST_SUITE_END();
