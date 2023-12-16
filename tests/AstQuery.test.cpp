// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstQuery.h"

#include "AstQueryDsl.h"
#include "doctest.h"
#include "Fixture.h"

using namespace Luau;

struct DocumentationSymbolFixture : BuiltinsFixture
{
    std::optional<DocumentationSymbol> getDocSymbol(const std::string& source, Position position)
    {
        check(source);

        SourceModule* sourceModule = getMainSourceModule();
        ModulePtr module = getMainModule();

        return getDocumentationSymbolAtPosition(*sourceModule, *module, position);
    }
};

TEST_SUITE_BEGIN("AstQuery::getDocumentationSymbolAtPosition");

TEST_CASE_FIXTURE(DocumentationSymbolFixture, "binding")
{
    std::optional<DocumentationSymbol> global = getDocSymbol(R"(
        local a = string.sub()
    )",
        Position(1, 21));

    CHECK_EQ(global, "@luau/global/string");
}

TEST_CASE_FIXTURE(DocumentationSymbolFixture, "prop")
{
    std::optional<DocumentationSymbol> substring = getDocSymbol(R"(
        local a = string.sub()
    )",
        Position(1, 27));

    CHECK_EQ(substring, "@luau/global/string.sub");
}

TEST_CASE_FIXTURE(DocumentationSymbolFixture, "event_callback_arg")
{
    loadDefinition(R"(
        declare function Connect(fn: (string) -> ())
    )");

    std::optional<DocumentationSymbol> substring = getDocSymbol(R"(
        Connect(function(abc)
        end)
    )",
        Position(1, 27));

    CHECK_EQ(substring, "@test/global/Connect/param/0/param/0");
}

TEST_CASE_FIXTURE(DocumentationSymbolFixture, "overloaded_fn")
{
    loadDefinition(R"(
        declare foo: ((string) -> number) & ((number) -> string)
    )");

    std::optional<DocumentationSymbol> symbol = getDocSymbol(R"(
        foo("asdf")
    )",
        Position(1, 10));

    CHECK_EQ(symbol, "@test/global/foo/overload/(string) -> number");
}

TEST_CASE_FIXTURE(DocumentationSymbolFixture, "class_method")
{
    loadDefinition(R"(
        declare class Foo
            function bar(self, x: string): number
        end

        declare Foo: {
            new: () -> Foo
        }
    )");

    std::optional<DocumentationSymbol> symbol = getDocSymbol(R"(
        local x: Foo = Foo.new()
        x:bar("asdf")
    )",
        Position(2, 11));

    CHECK_EQ(symbol, "@test/globaltype/Foo.bar");
}

TEST_CASE_FIXTURE(DocumentationSymbolFixture, "overloaded_class_method")
{
    loadDefinition(R"(
        declare class Foo
            function bar(self, x: string): number
            function bar(self, x: number): string
        end

        declare Foo: {
            new: () -> Foo
        }
    )");

    std::optional<DocumentationSymbol> symbol = getDocSymbol(R"(
        local x: Foo = Foo.new()
        x:bar("asdf")
    )",
        Position(2, 11));

    CHECK_EQ(symbol, "@test/globaltype/Foo.bar/overload/(Foo, string) -> number");
}

TEST_CASE_FIXTURE(DocumentationSymbolFixture, "table_function_prop")
{
    loadDefinition(R"(
        declare Foo: {
            new: (number) -> string
        }
    )");

    std::optional<DocumentationSymbol> symbol = getDocSymbol(R"(
        Foo.new("asdf")
    )",
        Position(1, 13));

    CHECK_EQ(symbol, "@test/global/Foo.new");
}

TEST_CASE_FIXTURE(DocumentationSymbolFixture, "table_overloaded_function_prop")
{
    loadDefinition(R"(
        declare Foo: {
            new: ((number) -> string) & ((string) -> number)
        }
    )");

    std::optional<DocumentationSymbol> symbol = getDocSymbol(R"(
        Foo.new("asdf")
    )",
        Position(1, 13));

    CHECK_EQ(symbol, "@test/global/Foo.new/overload/(string) -> number");
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("AstQuery");

TEST_CASE_FIXTURE(Fixture, "last_argument_function_call_type")
{
    check(R"(
local function foo() return 2 end
local function bar(a: number) return -a end
bar(foo())
    )");

    auto oty = findTypeAtPosition(Position(3, 7));
    REQUIRE(oty);
    CHECK_EQ("number", toString(*oty));

    auto expectedOty = findExpectedTypeAtPosition(Position(3, 7));
    REQUIRE(expectedOty);
    CHECK_EQ("number", toString(*expectedOty));
}

TEST_CASE_FIXTURE(Fixture, "ast_ancestry_at_eof")
{
    check(R"(
if true then
    )");

    std::vector<AstNode*> ancestry = findAstAncestryOfPosition(*getMainSourceModule(), Position(2, 4));
    REQUIRE_GE(ancestry.size(), 2);
    AstStat* parentStat = ancestry[ancestry.size() - 2]->asStat();
    REQUIRE(bool(parentStat));
    REQUIRE(parentStat->is<AstStatIf>());
}

TEST_CASE_FIXTURE(Fixture, "ac_ast_ancestry_at_number_const")
{
    check(R"(
print(3.)
    )");

    std::vector<AstNode*> ancestry = findAncestryAtPositionForAutocomplete(*getMainSourceModule(), Position(1, 8));
    REQUIRE_GE(ancestry.size(), 2);
    REQUIRE(ancestry.back()->is<AstExprConstantNumber>());
}

TEST_CASE_FIXTURE(Fixture, "ac_ast_ancestry_in_workspace_dot")
{
    check(R"(
print(workspace.)
    )");

    std::vector<AstNode*> ancestry = findAncestryAtPositionForAutocomplete(*getMainSourceModule(), Position(1, 16));
    REQUIRE_GE(ancestry.size(), 2);
    REQUIRE(ancestry.back()->is<AstExprIndexName>());
}

TEST_CASE_FIXTURE(Fixture, "ac_ast_ancestry_in_workspace_colon")
{
    check(R"(
print(workspace:)
    )");

    std::vector<AstNode*> ancestry = findAncestryAtPositionForAutocomplete(*getMainSourceModule(), Position(1, 16));
    REQUIRE_GE(ancestry.size(), 2);
    REQUIRE(ancestry.back()->is<AstExprIndexName>());
}

TEST_CASE_FIXTURE(Fixture, "Luau_query")
{
    AstStatBlock* block = parse(R"(
        if true then
        end
    )");

    AstStatIf* if_ = Luau::query<AstStatIf>(block);
    CHECK(if_);
}

TEST_CASE_FIXTURE(Fixture, "Luau_query_for_2nd_if_stat_which_doesnt_exist")
{
    AstStatBlock* block = parse(R"(
        if true then
        end
    )");

    AstStatIf* if_ = Luau::query<AstStatIf, 2>(block);
    CHECK(!if_);
}

TEST_CASE_FIXTURE(Fixture, "Luau_nested_query")
{
    AstStatBlock* block = parse(R"(
        if true then
        end
    )");

    AstStatIf* if_ = Luau::query<AstStatIf>(block);
    REQUIRE(if_);
    AstExprConstantBool* bool_ = Luau::query<AstExprConstantBool>(if_);
    REQUIRE(bool_);
}

TEST_CASE_FIXTURE(Fixture, "Luau_nested_query_but_first_query_failed")
{
    AstStatBlock* block = parse(R"(
        if true then
        end
    )");

    AstStatIf* if_ = Luau::query<AstStatIf, 2>(block);
    REQUIRE(!if_);
    AstExprConstantBool* bool_ = Luau::query<AstExprConstantBool>(if_); // ensure it doesn't crash
    REQUIRE(!bool_);
}

TEST_CASE_FIXTURE(Fixture, "Luau_selectively_query_for_a_different_boolean")
{
    AstStatBlock* block = parse(R"(
        local x = false and true
        local y = true and false
    )");

    AstExprConstantBool* fst = Luau::query<AstExprConstantBool>(block, {nth<AstStatLocal>(), nth<AstExprConstantBool>(2)});
    REQUIRE(fst);
    REQUIRE(fst->value == true);

    AstExprConstantBool* snd = Luau::query<AstExprConstantBool>(block, {nth<AstStatLocal>(2), nth<AstExprConstantBool>(2)});
    REQUIRE(snd);
    REQUIRE(snd->value == false);
}

TEST_CASE_FIXTURE(Fixture, "Luau_selectively_query_for_a_different_boolean_2")
{
    AstStatBlock* block = parse(R"(
        local x = false and true
        local y = true and false
    )");

    AstExprConstantBool* snd = Luau::query<AstExprConstantBool>(block, {nth<AstStatLocal>(2), nth<AstExprConstantBool>()});
    REQUIRE(snd);
    REQUIRE(snd->value == true);
}

TEST_CASE_FIXTURE(Fixture, "include_types_ancestry")
{
    check("local x: number = 4;");
    const Position pos(0, 10);

    std::vector<AstNode*> ancestryNoTypes = findAstAncestryOfPosition(*getMainSourceModule(), pos);
    std::vector<AstNode*> ancestryTypes = findAstAncestryOfPosition(*getMainSourceModule(), pos, true);

    CHECK(ancestryTypes.size() > ancestryNoTypes.size());
    CHECK(!ancestryNoTypes.back()->asType());
    CHECK(ancestryTypes.back()->asType());
}

TEST_CASE_FIXTURE(Fixture, "find_name_ancestry")
{
    check(R"(
        local tbl = {}
        function tbl:abc() end
    )");
    const Position pos(2, 18);

    std::vector<AstNode*> ancestry = findAstAncestryOfPosition(*getMainSourceModule(), pos);

    REQUIRE(!ancestry.empty());
    CHECK(ancestry.back()->is<AstExprLocal>());
}

TEST_CASE_FIXTURE(Fixture, "find_expr_ancestry")
{
    check(R"(
        local tbl = {}
        function tbl:abc() end
    )");
    const Position pos(2, 29);

    std::vector<AstNode*> ancestry = findAstAncestryOfPosition(*getMainSourceModule(), pos);

    REQUIRE(!ancestry.empty());
    CHECK(ancestry.back()->is<AstExprFunction>());
}

TEST_SUITE_END();
