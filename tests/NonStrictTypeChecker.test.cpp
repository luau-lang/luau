// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/NonStrictTypeChecker.h"

#include "Fixture.h"

#include "Luau/Common.h"
#include "Luau/Ast.h"
#include "Luau/ModuleResolver.h"
#include "ScopedFlags.h"
#include "doctest.h"
#include <iostream>

using namespace Luau;

struct NonStrictTypeCheckerFixture : Fixture
{

    ParseResult parse(std::string source)
    {
        ParseOptions opts;
        opts.allowDeclarationSyntax = true;
        ScopedFastFlag sff{"LuauCheckedFunctionSyntax", true};
        return tryParse(source, opts);
    }
};


TEST_SUITE_BEGIN("NonStrictTypeCheckerTest");

TEST_CASE_FIXTURE(Fixture, "basic")
{
    Luau::checkNonStrict(builtinTypes, nullptr);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "parse_top_level_checked_fn")
{
    std::string src = R"BUILTIN_SRC(
declare function @checked abs(n: number): number
)BUILTIN_SRC";

    ParseResult pr = parse(src);
    LUAU_ASSERT(pr.errors.size() == 0);

    LUAU_ASSERT(pr.root->body.size == 1);
    AstStat* root = *(pr.root->body.data);
    auto func = root->as<AstStatDeclareFunction>();
    LUAU_ASSERT(func);
    LUAU_ASSERT(func->checkedFunction);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "parse_declared_table_checked_member")
{
    std::string src = R"BUILTIN_SRC(
    declare math : {
        abs : @checked (number) -> number
}
)BUILTIN_SRC";

    ParseResult pr = parse(src);
    LUAU_ASSERT(pr.errors.size() == 0);

    LUAU_ASSERT(pr.root->body.size == 1);
    AstStat* root = *(pr.root->body.data);
    auto glob = root->as<AstStatDeclareGlobal>();
    LUAU_ASSERT(glob);
    auto tbl = glob->type->as<AstTypeTable>();
    LUAU_ASSERT(tbl);
    LUAU_ASSERT(tbl->props.size == 1);
    auto prop = *tbl->props.data;
    auto func = prop.type->as<AstTypeFunction>();
    LUAU_ASSERT(func);
    LUAU_ASSERT(func->checkedFunction);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "parse_checked_outside_decl_fails")
{
    auto src = R"(
    local @checked = 3
)";

    ParseResult pr = parse(src);
    LUAU_ASSERT(pr.errors.size() > 0);
    auto ts = pr.errors[1].getMessage();
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "parse_checked_in_and_out_of_decl_fails")
{
    auto src = R"(
    local @checked = 3
    declare function @checked abs(n: number): number
)";
    auto pr = parse(src);
    LUAU_ASSERT(pr.errors.size() == 2);
    LUAU_ASSERT(pr.errors[0].getLocation().begin.line == 1);
    LUAU_ASSERT(pr.errors[1].getLocation().begin.line == 1);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "parse_checked_as_function_name_fails")
{
    auto pr = parse(R"(
    function @checked(x: number) : number
    end
)");
    LUAU_ASSERT(pr.errors.size() > 0);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "cannot_use_@_as_variable_name")
{
    auto pr = parse(R"(
    local @blah = 3
)");

    LUAU_ASSERT(pr.errors.size() > 0);
}

TEST_SUITE_END();
