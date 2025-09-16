// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Parser.h"

#include "AstQueryDsl.h"
#include "Fixture.h"
#include "Luau/Ast.h"
#include "Luau/Common.h"
#include "ScopedFlags.h"

#include "doctest.h"

#include <limits.h>

using namespace Luau;

LUAU_FASTINT(LuauRecursionLimit)
LUAU_FASTINT(LuauTypeLengthLimit)
LUAU_FASTINT(LuauParseErrorLimit)
LUAU_FASTFLAG(LuauSolverV2)
LUAU_DYNAMIC_FASTFLAG(DebugLuauReportReturnTypeVariadicWithTypeSuffix)
LUAU_FASTFLAG(LuauParseIncompleteInterpStringsWithLocation)
LUAU_FASTFLAG(LuauParametrizedAttributeSyntax)

// Clip with DebugLuauReportReturnTypeVariadicWithTypeSuffix
extern bool luau_telemetry_parsed_return_type_variadic_with_type_suffix;

namespace
{

struct Counter
{
    static int instanceCount;

    int id;

    Counter()
    {
        ++instanceCount;
        id = instanceCount;
    }
};

int Counter::instanceCount = 0;

} // namespace

TEST_SUITE_BEGIN("AllocatorTests");

TEST_CASE("allocator_can_be_moved")
{
    Counter* c = nullptr;
    auto inner = [&]()
    {
        Luau::Allocator allocator;
        c = allocator.alloc<Counter>();
        Luau::Allocator moved{std::move(allocator)};
        return moved;
    };

    Counter::instanceCount = 0;
    Luau::Allocator a{inner()};

    CHECK_EQ(1, c->id);
}

TEST_CASE("moved_out_Allocator_can_still_be_used")
{
    Luau::Allocator outer;
    Luau::Allocator inner{std::move(outer)};

    int* i = outer.alloc<int>();
    REQUIRE(i != nullptr);
    *i = 55;
    REQUIRE_EQ(*i, 55);
}

TEST_CASE("aligns_things")
{
    Luau::Allocator alloc;

    char* one = alloc.alloc<char>();
    double* two = alloc.alloc<double>();
    (void)one;
    CHECK_EQ(0, reinterpret_cast<intptr_t>(two) & (alignof(double) - 1));
}

TEST_CASE("initial_double_is_aligned")
{
    Luau::Allocator alloc;

    double* one = alloc.alloc<double>();
    CHECK_EQ(0, reinterpret_cast<intptr_t>(one) & (alignof(double) - 1));
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("ParserTests");

TEST_CASE_FIXTURE(Fixture, "basic_parse")
{
    AstStat* stat = parse("print(\"Hello World!\")");
    REQUIRE(stat != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "can_haz_annotations")
{
    AstStatBlock* block = parse("local foo: string = \"Hello Types!\"");
    REQUIRE(block != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "local_with_annotation")
{
    AstStatBlock* block = parse(R"(
        local foo: string = "Hello Types!"
    )");

    REQUIRE(block != nullptr);

    REQUIRE(block->body.size > 0);

    AstStatLocal* local = block->body.data[0]->as<AstStatLocal>();
    REQUIRE(local != nullptr);

    REQUIRE_EQ(1, local->vars.size);

    AstLocal* l = local->vars.data[0];
    REQUIRE(l->annotation != nullptr);

    REQUIRE_EQ(1, local->values.size);
}

TEST_CASE_FIXTURE(Fixture, "type_names_can_contain_dots")
{
    AstStatBlock* block = parse(R"(
        local foo: SomeModule.CoolType
    )");

    REQUIRE(block != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "functions_can_have_return_annotations")
{
    AstStatBlock* block = parse(R"(
        function foo(): number return 55 end
    )");

    REQUIRE(block != nullptr);
    REQUIRE(block->body.size > 0);

    AstStatFunction* statFunction = block->body.data[0]->as<AstStatFunction>();
    REQUIRE(statFunction != nullptr);

    REQUIRE(statFunction->func->returnAnnotation);
    auto typePack = statFunction->func->returnAnnotation->as<AstTypePackExplicit>();
    REQUIRE(typePack);
    CHECK_EQ(typePack->typeList.types.size, 1);
    CHECK(typePack->typeList.tailType == nullptr);
}

TEST_CASE_FIXTURE(Fixture, "functions_can_have_a_function_type_annotation")
{
    AstStatBlock* block = parse(R"(
        function f(): (number) -> nil return nil end
    )");

    REQUIRE(block != nullptr);
    REQUIRE(block->body.size > 0);

    AstStatFunction* statFunc = block->body.data[0]->as<AstStatFunction>();
    REQUIRE(statFunc != nullptr);

    REQUIRE(statFunc->func->returnAnnotation);
    auto typePack = statFunc->func->returnAnnotation->as<AstTypePackExplicit>();
    REQUIRE(typePack);
    CHECK(typePack->typeList.tailType == nullptr);
    AstArray<AstType*>& retTypes = typePack->typeList.types;
    REQUIRE(retTypes.size == 1);

    AstTypeFunction* funTy = retTypes.data[0]->as<AstTypeFunction>();
    REQUIRE(funTy != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "function_return_type_should_disambiguate_from_function_type_and_multiple_returns")
{
    AstStatBlock* block = parse(R"(
        function f(): (number, string) return 1, "foo" end
    )");

    REQUIRE(block != nullptr);
    REQUIRE(block->body.size > 0);

    AstStatFunction* statFunc = block->body.data[0]->as<AstStatFunction>();
    REQUIRE(statFunc != nullptr);

    REQUIRE(statFunc->func->returnAnnotation);
    auto typePack = statFunc->func->returnAnnotation->as<AstTypePackExplicit>();
    REQUIRE(typePack);
    CHECK(typePack->typeList.tailType == nullptr);
    AstArray<AstType*>& retTypes = typePack->typeList.types;
    REQUIRE(retTypes.size == 2);

    AstTypeReference* ty0 = retTypes.data[0]->as<AstTypeReference>();
    REQUIRE(ty0 != nullptr);
    REQUIRE(ty0->name == "number");

    AstTypeReference* ty1 = retTypes.data[1]->as<AstTypeReference>();
    REQUIRE(ty1 != nullptr);
    REQUIRE(ty1->name == "string");
}

TEST_CASE_FIXTURE(Fixture, "function_return_type_should_parse_as_function_type_annotation_with_no_args")
{
    AstStatBlock* block = parse(R"(
        function f(): () -> nil return nil end
    )");

    REQUIRE(block != nullptr);
    REQUIRE(block->body.size > 0);

    AstStatFunction* statFunc = block->body.data[0]->as<AstStatFunction>();
    REQUIRE(statFunc != nullptr);

    REQUIRE(statFunc->func->returnAnnotation);
    auto typePack = statFunc->func->returnAnnotation->as<AstTypePackExplicit>();
    REQUIRE(typePack);
    CHECK(typePack->typeList.tailType == nullptr);
    AstArray<AstType*>& retTypes = typePack->typeList.types;
    REQUIRE(retTypes.size == 1);

    AstTypeFunction* funTy = retTypes.data[0]->as<AstTypeFunction>();
    REQUIRE(funTy != nullptr);
    REQUIRE(funTy->argTypes.types.size == 0);
    CHECK(funTy->argTypes.tailType == nullptr);

    auto funReturnPack = funTy->returnTypes->as<AstTypePackExplicit>();
    REQUIRE(funReturnPack);
    CHECK(funReturnPack->typeList.tailType == nullptr);

    AstTypeReference* ty = funReturnPack->typeList.types.data[0]->as<AstTypeReference>();
    REQUIRE(ty != nullptr);
    REQUIRE(ty->name == "nil");
}

TEST_CASE_FIXTURE(Fixture, "annotations_can_be_tables")
{
    AstStatBlock* stat = parse(R"(
        local zero: number
        local one: {x: number, y: string}
    )");

    REQUIRE(stat != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "tables_should_have_an_indexer_and_keys")
{
    AstStatBlock* stat = parse(R"(
        local t: {
            [string]: number,
            f: () -> nil
        }
    )");

    REQUIRE(stat != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "tables_can_have_trailing_separator")
{
    AstStatBlock* stat = parse(R"(
        local zero: number
        local one: {x: number, y: string, }
    )");

    REQUIRE(stat != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "tables_can_use_semicolons")
{
    AstStatBlock* stat = parse(R"(
        local zero: number
        local one: {x: number; y: string; }
    )");

    REQUIRE(stat != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "other_places_where_type_annotations_are_allowed")
{
    AstStatBlock* stat = parse(R"(
        for i: number = 0, 50 do end
        for i: number, s: string in expr() do end
    )");

    REQUIRE(stat != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "nil_is_a_valid_type_name")
{
    AstStatBlock* stat = parse(R"(
        local n: nil
    )");

    REQUIRE(stat != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "function_type_annotation")
{
    AstStatBlock* stat = parse(R"(
        local f: (number, string) -> nil
    )");

    REQUIRE(stat != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "functions_can_return_multiple_values")
{
    AstStatBlock* stat = parse(R"(
        local f: (number) -> (number, number)
    )");

    REQUIRE(stat != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "functions_can_have_0_arguments")
{
    AstStatBlock* stat = parse(R"(
        local f: () -> number
    )");

    REQUIRE(stat != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "functions_can_return_0_values")
{
    AstStatBlock* block = parse(R"(
        local f: (number) -> ()
    )");

    REQUIRE(block != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "intersection_of_two_function_types_if_no_returns")
{
    AstStatBlock* block = parse(R"(
        local f: (string) -> () & (number) -> ()
    )");

    REQUIRE(block != nullptr);

    AstStatLocal* local = block->body.data[0]->as<AstStatLocal>();
    AstTypeIntersection* annotation = local->vars.data[0]->annotation->as<AstTypeIntersection>();
    REQUIRE(annotation != nullptr);
    CHECK(annotation->types.data[0]->as<AstTypeFunction>());
    CHECK(annotation->types.data[1]->as<AstTypeFunction>());
}

TEST_CASE_FIXTURE(Fixture, "intersection_of_two_function_types_if_two_or_more_returns")
{
    AstStatBlock* block = parse(R"(
        local f: (string) -> (string, number) & (number) -> (number, string)
    )");

    REQUIRE(block != nullptr);

    AstStatLocal* local = block->body.data[0]->as<AstStatLocal>();
    AstTypeIntersection* annotation = local->vars.data[0]->annotation->as<AstTypeIntersection>();
    REQUIRE(annotation != nullptr);
    CHECK(annotation->types.data[0]->as<AstTypeFunction>());
    CHECK(annotation->types.data[1]->as<AstTypeFunction>());
}

TEST_CASE_FIXTURE(Fixture, "return_type_is_an_intersection_type_if_led_with_one_parenthesized_type")
{
    AstStatBlock* block = parse(R"(
        local f: (string) -> (string) & (number) -> (number)
    )");

    REQUIRE(block != nullptr);

    AstStatLocal* local = block->body.data[0]->as<AstStatLocal>();
    AstTypeFunction* annotation = local->vars.data[0]->annotation->as<AstTypeFunction>();
    REQUIRE(annotation != nullptr);

    auto returnTypePack = annotation->returnTypes->as<AstTypePackExplicit>();
    REQUIRE(returnTypePack);
    AstTypeIntersection* returnAnnotation = returnTypePack->typeList.types.data[0]->as<AstTypeIntersection>();
    REQUIRE(returnAnnotation != nullptr);
    CHECK(returnAnnotation->types.data[0]->as<AstTypeGroup>());
    CHECK(returnAnnotation->types.data[1]->as<AstTypeFunction>());
}

TEST_CASE_FIXTURE(Fixture, "type_alias_to_a_typeof")
{
    AstStatBlock* block = parse(R"(
        type A = typeof(1)
    )");

    REQUIRE(block != nullptr);
    REQUIRE(block->body.size > 0);

    auto typeAliasStat = block->body.data[0]->as<AstStatTypeAlias>();
    REQUIRE(typeAliasStat != nullptr);
    CHECK_EQ(typeAliasStat->location, (Location{{1, 8}, {1, 26}}));
}

TEST_CASE_FIXTURE(Fixture, "type_alias_should_point_to_string")
{
    AstStatBlock* block = parse(R"(
        type A = string
    )");

    REQUIRE(block != nullptr);
    REQUIRE(block->body.size > 0);
    REQUIRE(block->body.data[0]->is<AstStatTypeAlias>());
}

TEST_CASE_FIXTURE(Fixture, "type_alias_should_not_interfere_with_type_function_call_or_assignment")
{
    AstStatBlock* block = parse(R"(
        type("a")
        type = nil
    )");

    REQUIRE(block != nullptr);
    REQUIRE(block->body.size > 0);

    AstStatExpr* stat = block->body.data[0]->as<AstStatExpr>();
    REQUIRE(stat != nullptr);
    REQUIRE(stat->expr->as<AstExprCall>());

    REQUIRE(block->body.data[1]->is<AstStatAssign>());
}

TEST_CASE_FIXTURE(Fixture, "type_alias_should_work_when_name_is_also_local")
{
    AstStatBlock* block = parse(R"(
        local A = nil
        type A = string
    )");

    REQUIRE(block != nullptr);
    REQUIRE(block->body.size == 2);
    REQUIRE(block->body.data[0]->is<AstStatLocal>());
    REQUIRE(block->body.data[1]->is<AstStatTypeAlias>());
}

TEST_CASE_FIXTURE(Fixture, "type_alias_span_is_correct")
{
    AstStatBlock* block = parse(R"(
        type Packed1<T...> = (T...) -> (T...)
        type Packed2<T...> = (Packed1<T...>, T...) -> (Packed1<T...>, T...)
    )");

    REQUIRE(block != nullptr);
    REQUIRE(2 == block->body.size);
    AstStatTypeAlias* t1 = block->body.data[0]->as<AstStatTypeAlias>();
    REQUIRE(t1);
    REQUIRE(Location{Position{1, 8}, Position{1, 45}} == t1->location);

    AstStatTypeAlias* t2 = block->body.data[1]->as<AstStatTypeAlias>();
    REQUIRE(t2);
    REQUIRE(Location{Position{2, 8}, Position{2, 75}} == t2->location);
}

TEST_CASE_FIXTURE(Fixture, "parse_error_messages")
{
    matchParseError(
        R"(
        local a: (number, number) -> (string
    )",
        "Expected ')' (to close '(' at line 2), got <eof>"
    );

    matchParseError(
        R"(
        local a: (number, number) -> (
            string
    )",
        "Expected ')' (to close '(' at line 2), got <eof>"
    );

    matchParseError(
        R"(
        local a: (number, number)
    )",
        "Expected '->' when parsing function type, got <eof>"
    );

    matchParseError(
        R"(
        local a: (number, number
    )",
        "Expected ')' (to close '(' at line 2), got <eof>"
    );

    matchParseError(
        R"(
        local a: {foo: string,
    )",
        "Expected identifier when parsing table field, got <eof>"
    );

    matchParseError(
        R"(
        local a: {foo: string
    )",
        "Expected '}' (to close '{' at line 2), got <eof>"
    );

    matchParseError(
        R"(
        local a: { [string]: number, [number]: string }
    )",
        "Cannot have more than one table indexer"
    );

    matchParseError(
        R"(
        type T = <a>foo
    )",
        "Expected '(' when parsing function parameters, got 'foo'"
    );
}

TEST_CASE_FIXTURE(Fixture, "mixed_intersection_and_union_not_allowed")
{
    matchParseError("type A = number & string | boolean", "Mixing union and intersection types is not allowed; consider wrapping in parentheses.");
}

TEST_CASE_FIXTURE(Fixture, "mixed_intersection_and_union_allowed_when_parenthesized")
{
    try
    {
        parse("type A = (number & string) | boolean");
    }
    catch (const ParseErrors& e)
    {
        FAIL(e.what());
    }
}

TEST_CASE_FIXTURE(Fixture, "cannot_write_multiple_values_in_type_groups")
{
    matchParseError("type F = ((string, number))", "Expected '->' when parsing function type, got ')'");
    matchParseError("type F = () -> ((string, number))", "Expected '->' when parsing function type, got ')'");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_error_messages")
{
    matchParseError("type 5 = number", "Expected identifier when parsing type name, got '5'");
    matchParseError("type A", "Expected '=' when parsing type alias, got <eof>");
    matchParseError("type A<", "Expected identifier, got <eof>");
    matchParseError("type A<B", "Expected '>' (to close '<' at column 7), got <eof>");
}

TEST_CASE_FIXTURE(Fixture, "type_assertion_expression")
{
    (void)parse(R"(
        local a = something() :: any
    )");
}

// The bug that motivated this test was an infinite loop.
// TODO: Set a timer and crash if the timeout is exceeded.
TEST_CASE_FIXTURE(Fixture, "last_line_does_not_have_to_be_blank")
{
    (void)parse("-- print('hello')");
}

TEST_CASE_FIXTURE(Fixture, "type_assertion_expression_binds_tightly")
{
    AstStatBlock* stat = parse(R"(
        local a = one :: any + two :: any
    )");

    REQUIRE(stat != nullptr);

    AstStatBlock* block = stat->as<AstStatBlock>();
    REQUIRE(block != nullptr);
    REQUIRE_EQ(1, block->body.size);

    AstStatLocal* local = block->body.data[0]->as<AstStatLocal>();
    REQUIRE(local != nullptr);
    REQUIRE_EQ(1, local->values.size);

    AstExprBinary* bin = local->values.data[0]->as<AstExprBinary>();
    REQUIRE(bin != nullptr);

    CHECK(nullptr != bin->left->as<AstExprTypeAssertion>());
    CHECK(nullptr != bin->right->as<AstExprTypeAssertion>());
}

TEST_CASE_FIXTURE(Fixture, "mode_is_unset_if_no_hot_comment")
{
    ParseResult result = parseEx("print('Hello World!')");
    CHECK(result.hotcomments.empty());
}

TEST_CASE_FIXTURE(Fixture, "sense_hot_comment_on_first_line")
{
    ParseOptions options;
    options.captureComments = true;

    ParseResult result = parseEx("   --!strict ", options);
    std::optional<Mode> mode = parseMode(result.hotcomments);
    REQUIRE(bool(mode));
    CHECK_EQ(int(*mode), int(Mode::Strict));
}

TEST_CASE_FIXTURE(Fixture, "non_header_hot_comments")
{
    ParseOptions options;
    options.captureComments = true;

    ParseResult result = parseEx("do end --!strict", options);
    std::optional<Mode> mode = parseMode(result.hotcomments);
    REQUIRE(!mode);
}

TEST_CASE_FIXTURE(Fixture, "stop_if_line_ends_with_hyphen")
{
    CHECK_THROWS_AS(parse("   -"), std::exception);
}

TEST_CASE_FIXTURE(Fixture, "nonstrict_mode")
{
    ParseOptions options;
    options.captureComments = true;

    ParseResult result = parseEx("--!nonstrict", options);
    CHECK(result.errors.empty());
    std::optional<Mode> mode = parseMode(result.hotcomments);
    REQUIRE(bool(mode));
    CHECK_EQ(int(*mode), int(Mode::Nonstrict));
}

TEST_CASE_FIXTURE(Fixture, "nocheck_mode")
{
    ParseOptions options;
    options.captureComments = true;

    ParseResult result = parseEx("--!nocheck", options);
    CHECK(result.errors.empty());
    std::optional<Mode> mode = parseMode(result.hotcomments);
    REQUIRE(bool(mode));
    CHECK_EQ(int(*mode), int(Mode::NoCheck));
}

TEST_CASE_FIXTURE(Fixture, "vertical_space")
{
    ParseResult result = parseEx("a()\vb()");
    CHECK(result.errors.empty());
}

TEST_CASE_FIXTURE(Fixture, "parse_error_type_name")
{
    matchParseError(
        R"(
        local a: Foo.=
    )",
        "Expected identifier when parsing field name, got '='"
    );
}

TEST_CASE_FIXTURE(Fixture, "parse_numbers_decimal")
{
    AstStat* stat = parse("return 1, .5, 1.5, 1e-5, 1.5e-5, 12_345.1_25");
    REQUIRE(stat != nullptr);

    AstStatReturn* str = stat->as<AstStatBlock>()->body.data[0]->as<AstStatReturn>();
    CHECK(str->list.size == 6);
    CHECK_EQ(str->list.data[0]->as<AstExprConstantNumber>()->value, 1.0);
    CHECK_EQ(str->list.data[1]->as<AstExprConstantNumber>()->value, 0.5);
    CHECK_EQ(str->list.data[2]->as<AstExprConstantNumber>()->value, 1.5);
    CHECK_EQ(str->list.data[3]->as<AstExprConstantNumber>()->value, 1.0e-5);
    CHECK_EQ(str->list.data[4]->as<AstExprConstantNumber>()->value, 1.5e-5);
    CHECK_EQ(str->list.data[5]->as<AstExprConstantNumber>()->value, 12345.125);
}

TEST_CASE_FIXTURE(Fixture, "parse_numbers_hexadecimal")
{
    AstStat* stat = parse("return 0xab, 0XAB05, 0xff_ff, 0xffffffffffffffff");
    REQUIRE(stat != nullptr);

    AstStatReturn* str = stat->as<AstStatBlock>()->body.data[0]->as<AstStatReturn>();
    CHECK(str->list.size == 4);
    CHECK_EQ(str->list.data[0]->as<AstExprConstantNumber>()->value, 0xab);
    CHECK_EQ(str->list.data[1]->as<AstExprConstantNumber>()->value, 0xAB05);
    CHECK_EQ(str->list.data[2]->as<AstExprConstantNumber>()->value, 0xFFFF);
    CHECK_EQ(str->list.data[3]->as<AstExprConstantNumber>()->value, double(ULLONG_MAX));
}

TEST_CASE_FIXTURE(Fixture, "parse_numbers_binary")
{
    AstStat* stat = parse("return 0b1, 0b0, 0b101010, 0b1111111111111111111111111111111111111111111111111111111111111111");
    REQUIRE(stat != nullptr);

    AstStatReturn* str = stat->as<AstStatBlock>()->body.data[0]->as<AstStatReturn>();
    CHECK(str->list.size == 4);
    CHECK_EQ(str->list.data[0]->as<AstExprConstantNumber>()->value, 1);
    CHECK_EQ(str->list.data[1]->as<AstExprConstantNumber>()->value, 0);
    CHECK_EQ(str->list.data[2]->as<AstExprConstantNumber>()->value, 42);
    CHECK_EQ(str->list.data[3]->as<AstExprConstantNumber>()->value, double(ULLONG_MAX));
}

TEST_CASE_FIXTURE(Fixture, "parse_numbers_error")
{
    matchParseError("return 0b123", "Malformed number");
    matchParseError("return 123x", "Malformed number");
    matchParseError("return 0xg", "Malformed number");
    matchParseError("return 0x0x123", "Malformed number");
    matchParseError("return 0xffffffffffffffffffffllllllg", "Malformed number");
    matchParseError("return 0x0xffffffffffffffffffffffffffff", "Malformed number");
}

TEST_CASE_FIXTURE(Fixture, "break_return_not_last_error")
{
    matchParseError("return 0 print(5)", "Expected <eof>, got 'print'");
    matchParseError("while true do break print(5) end", "Expected 'end' (to close 'do' at column 12), got 'print'");
}

TEST_CASE_FIXTURE(Fixture, "error_on_unicode")
{
    matchParseError(
        R"(
            local ☃ = 10
        )",
        "Expected identifier when parsing variable name, got Unicode character U+2603"
    );
}

TEST_CASE_FIXTURE(Fixture, "allow_unicode_in_string")
{
    ParseResult result = parseEx("local snowman = \"☃\"");
    CHECK(result.errors.empty());
}

TEST_CASE_FIXTURE(Fixture, "error_on_confusable")
{
    matchParseError(
        R"(
        local pi = 3․13
    )",
        "Expected identifier when parsing expression, got Unicode character U+2024 (did you mean '.'?)"
    );
}

TEST_CASE_FIXTURE(Fixture, "error_on_non_utf8_sequence")
{
    const char* expected = "Expected identifier when parsing expression, got invalid UTF-8 sequence";

    matchParseError("local pi = \xFF!", expected);
    matchParseError("local pi = \xE2!", expected);
}

TEST_CASE_FIXTURE(Fixture, "lex_broken_unicode")
{
    const std::string testInput = std::string("\xFF\xFE☃․");

    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);
    Lexeme lexeme = lexer.current();

    lexeme = lexer.next();
    CHECK_EQ(lexeme.type, Lexeme::BrokenUnicode);
    CHECK_EQ(lexeme.codepoint, 0);
    CHECK_EQ(lexeme.location, Luau::Location(Luau::Position(0, 0), Luau::Position(0, 1)));

    lexeme = lexer.next();
    CHECK_EQ(lexeme.type, Lexeme::BrokenUnicode);
    CHECK_EQ(lexeme.codepoint, 0);
    CHECK_EQ(lexeme.location, Luau::Location(Luau::Position(0, 1), Luau::Position(0, 2)));

    lexeme = lexer.next();
    CHECK_EQ(lexeme.type, Lexeme::BrokenUnicode);
    CHECK_EQ(lexeme.codepoint, 0x2603);
    CHECK_EQ(lexeme.location, Luau::Location(Luau::Position(0, 2), Luau::Position(0, 5)));

    lexeme = lexer.next();
    CHECK_EQ(lexeme.type, Lexeme::BrokenUnicode);
    CHECK_EQ(lexeme.codepoint, 0x2024);
    CHECK_EQ(lexeme.location, Luau::Location(Luau::Position(0, 5), Luau::Position(0, 8)));

    lexeme = lexer.next();
    CHECK_EQ(lexeme.type, Lexeme::Eof);
}

TEST_CASE_FIXTURE(Fixture, "parse_continue")
{
    AstStatBlock* stat = parse(R"(
        while true do
            continue()
            continue = 5
            continue, continue = continue
            continue
        end
    )");

    REQUIRE(stat != nullptr);

    AstStatBlock* block = stat->as<AstStatBlock>();
    REQUIRE(block != nullptr);
    REQUIRE_EQ(1, block->body.size);

    AstStatWhile* wb = block->body.data[0]->as<AstStatWhile>();
    REQUIRE(wb != nullptr);

    AstStatBlock* wblock = wb->body->as<AstStatBlock>();
    REQUIRE(wblock != nullptr);
    REQUIRE_EQ(4, wblock->body.size);

    REQUIRE(wblock->body.data[0]->is<AstStatExpr>());
    REQUIRE(wblock->body.data[1]->is<AstStatAssign>());
    REQUIRE(wblock->body.data[2]->is<AstStatAssign>());
    REQUIRE(wblock->body.data[3]->is<AstStatContinue>());
}

TEST_CASE_FIXTURE(Fixture, "continue_not_last_error")
{
    matchParseError("while true do continue print(5) end", "Expected 'end' (to close 'do' at column 12), got 'print'");
}

TEST_CASE_FIXTURE(Fixture, "parse_export_type")
{
    AstStatBlock* stat = parse(R"(
        export()
        export = 5
        export, export = export
        export type A = number
        type A = number
    )");

    REQUIRE(stat != nullptr);

    AstStatBlock* block = stat->as<AstStatBlock>();
    REQUIRE(block != nullptr);
    REQUIRE_EQ(5, block->body.size);

    REQUIRE(block->body.data[0]->is<AstStatExpr>());
    REQUIRE(block->body.data[1]->is<AstStatAssign>());
    REQUIRE(block->body.data[2]->is<AstStatAssign>());
    REQUIRE(block->body.data[3]->is<AstStatTypeAlias>());
    REQUIRE(block->body.data[4]->is<AstStatTypeAlias>());
}

TEST_CASE_FIXTURE(Fixture, "export_is_an_identifier_only_when_followed_by_type")
{
    try
    {
        parse(R"(
            export function a() end
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ("Incomplete statement: expected assignment or a function call", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "incomplete_statement_error")
{
    matchParseError("fiddlesticks", "Incomplete statement: expected assignment or a function call");
}

TEST_CASE_FIXTURE(Fixture, "parse_compound_assignment")
{
    AstStatBlock* block = parse(R"(
        a += 5
    )");

    REQUIRE(block != nullptr);
    REQUIRE(block->body.size == 1);
    REQUIRE(block->body.data[0]->is<AstStatCompoundAssign>());
    REQUIRE(block->body.data[0]->as<AstStatCompoundAssign>()->op == AstExprBinary::Add);
}

TEST_CASE_FIXTURE(Fixture, "parse_compound_assignment_error_call")
{
    try
    {
        parse(R"(
            a() += 5
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ("Expected identifier when parsing expression, got '+='", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_compound_assignment_error_not_lvalue")
{
    try
    {
        parse(R"(
            (a) += 5
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ("Assigned expression must be a variable or a field", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_compound_assignment_error_multiple")
{
    try
    {
        parse(R"(
            a, b += 5
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ("Expected '=' when parsing assignment, got '+='", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_interpolated_string_double_brace_begin")
{
    try
    {
        parse(R"(
            _ = `{{oops}}`
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ("Double braces are not permitted within interpolated strings; did you mean '\\{'?", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_interpolated_string_double_brace_mid")
{
    try
    {
        parse(R"(
            _ = `{nice} {{oops}}`
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ("Double braces are not permitted within interpolated strings; did you mean '\\{'?", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_interpolated_string_without_end_brace")
{
    ScopedFastFlag sff{FFlag::LuauParseIncompleteInterpStringsWithLocation, true};
    auto columnOfEndBraceError = [this](const char* code)
    {
        try
        {
            parse(code);
            FAIL("Expected ParseErrors to be thrown");
            return UINT_MAX;
        }
        catch (const ParseErrors& e)
        {
            CHECK_EQ(e.getErrors().size(), 1);

            auto error = e.getErrors().front();
            CHECK_EQ("Malformed interpolated string; did you forget to add a '}'?", error.getMessage());
            return error.getLocation().begin.column;
        }
    };

    // This makes sure that the error is coming from the closing brace itself
    CHECK_EQ(columnOfEndBraceError("_ = `{a`"), 7);
    CHECK_EQ(columnOfEndBraceError("_ = `{abcdefg`"), 13);
    CHECK_EQ(columnOfEndBraceError("_ =       `{a`"), columnOfEndBraceError("_ = `{abcdefg`"));
}

TEST_CASE_FIXTURE(Fixture, "parse_interpolated_string_without_end_brace_in_table")
{
    try
    {
        parse(R"(
            _ = { `{a` }
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ(e.getErrors().size(), 2);

        CHECK_EQ("Malformed interpolated string; did you forget to add a '}'?", e.getErrors().front().getMessage());
        CHECK_EQ("Expected '}' (to close '{' at line 2), got <eof>", e.getErrors().back().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_interpolated_string_mid_without_end_brace_in_table")
{
    try
    {
        parse(R"(
            _ = { `x {"y"} {z` }
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ(e.getErrors().size(), 2);

        CHECK_EQ("Malformed interpolated string; did you forget to add a '}'?", e.getErrors().front().getMessage());
        CHECK_EQ("Expected '}' (to close '{' at line 2), got <eof>", e.getErrors().back().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_interpolated_string_as_type_fail")
{
    try
    {
        parse(R"(
            local a: `what` = `???`
            local b: `what {"the"}` = `???`
            local c: `what {"the"} heck` = `???`
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& parseErrors)
    {
        CHECK_EQ(parseErrors.getErrors().size(), 3);

        for (ParseError error : parseErrors.getErrors())
            CHECK_EQ(error.getMessage(), "Interpolated string literals cannot be used as types");
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_interpolated_string_call_without_parens")
{
    try
    {
        parse(R"(
            _ = print `{42}`
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ("Expected identifier when parsing expression, got `{", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_interpolated_string_without_expression")
{
    try
    {
        parse(R"(
            print(`{}`)
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ("Malformed interpolated string, expected expression inside '{}'", e.getErrors().front().getMessage());
    }

    try
    {
        parse(R"(
            print(`{}{1}`)
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ("Malformed interpolated string, expected expression inside '{}'", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_interpolated_string_malformed_escape")
{
    try
    {
        parse(R"(
            local a = `???\xQQ {1}`
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ("Interpolated string literal contains malformed escape sequence", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_interpolated_string_weird_token")
{
    try
    {
        parse(R"(
            local a = `??? {42 !!}`
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ("Malformed interpolated string, got '!'", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_nesting_based_end_detection")
{
    try
    {
        parse(R"(-- i am line 1
function BottomUpTree(item, depth)
  if depth > 0 then
    local i = item + item
    depth = depth - 1
    local left, right = BottomUpTree(i-1, depth), BottomUpTree(i, depth)
    return { item, left, right }
  else
    return { item }
end

function ItemCheck(tree)
  if tree[2] then
    return tree[1] + ItemCheck(tree[2]) - ItemCheck(tree[3])
  else
    return tree[1]
  end
end
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ(
            "Expected 'end' (to close 'function' at line 2), got <eof>; did you forget to close 'else' at line 8?", e.getErrors().front().getMessage()
        );
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_nesting_based_end_detection_single_line")
{
    try
    {
        parse(R"(-- i am line 1
function ItemCheck(tree)
  if tree[2] then return tree[1] + ItemCheck(tree[2]) - ItemCheck(tree[3]) else return tree[1]
end

function BottomUpTree(item, depth)
  if depth > 0 then
    local i = item + item
    depth = depth - 1
    local left, right = BottomUpTree(i-1, depth), BottomUpTree(i, depth)
    return { item, left, right }
  else
    return { item }
  end
end
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ(
            "Expected 'end' (to close 'function' at line 2), got <eof>; did you forget to close 'else' at line 3?", e.getErrors().front().getMessage()
        );
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_nesting_based_end_detection_local_repeat")
{
    try
    {
        parse(R"(-- i am line 1
repeat
  print(1)
  repeat
    print(2)
  print(3)
until false
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ(
            "Expected 'until' (to close 'repeat' at line 2), got <eof>; did you forget to close 'repeat' at line 4?",
            e.getErrors().front().getMessage()
        );
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_nesting_based_end_detection_local_function")
{
    try
    {
        parse(R"(-- i am line 1
local function BottomUpTree(item, depth)
  if depth > 0 then
    local i = item + item
    depth = depth - 1
    local left, right = BottomUpTree(i-1, depth), BottomUpTree(i, depth)
    return { item, left, right }
  else
    return { item }
end

local function ItemCheck(tree)
  if tree[2] then
    return tree[1] + ItemCheck(tree[2]) - ItemCheck(tree[3])
  else
    return tree[1]
  end
end
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ(
            "Expected 'end' (to close 'function' at line 2), got <eof>; did you forget to close 'else' at line 8?", e.getErrors().front().getMessage()
        );
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_nesting_based_end_detection_failsafe_earlier")
{
    try
    {
        parse(R"(-- i am line 1
local function ItemCheck(tree)
  if tree[2] then
    return tree[1] + ItemCheck(tree[2]) - ItemCheck(tree[3])
  else
    return tree[1]
      end
end

local function BottomUpTree(item, depth)
  if depth > 0 then
    local i = item + item
    depth = depth - 1
    local left, right = BottomUpTree(i-1, depth), BottomUpTree(i, depth)
    return { item, left, right }
  else
    return { item }
  end
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ("Expected 'end' (to close 'function' at line 10), got <eof>", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_nesting_based_end_detection_nested")
{
    try
    {
        parse(R"(-- i am line 1
function stringifyTable(t)
    local entries = {}
    for k, v in pairs(t) do
        -- if we find a nested table, convert that recursively
        if type(v) == "table" then
            v = stringifyTable(v)
        else
            v = tostring(v)
        k = tostring(k)

        -- add another entry to our stringified table
        entries[#entries + 1] = ("s = s"):format(k, v)
    end

    -- the memory location of the table
    local id = tostring(t):sub(8)

    return ("{s}@s"):format(table.concat(entries, ", "), id)
end
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ(
            "Expected 'end' (to close 'function' at line 2), got <eof>; did you forget to close 'else' at line 8?", e.getErrors().front().getMessage()
        );
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_error_table_literal")
{
    try
    {
        parse(R"(
function stringifyTable(t)
    local foo = (name = t)
    return foo
end
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ(
            "Expected ')' (to close '(' at column 17), got '='; did you mean to use '{' when defining a table?", e.getErrors().front().getMessage()
        );
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_error_function_call")
{
    try
    {
        parse(R"(
function stringifyTable(t)
    local foo = t:Parse 2
    return foo
end
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ(e.getErrors().front().getLocation().begin.line, 2);
        CHECK_EQ("Expected '(', '{' or <string> when parsing function call, got '2'", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_error_function_call_newline")
{
    try
    {
        parse(R"(
function stringifyTable(t)
    local foo = t:Parse
    return foo
end
        )");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const ParseErrors& e)
    {
        CHECK_EQ(e.getErrors().front().getLocation().begin.line, 2);
        CHECK_EQ("Expected function call arguments after '('", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_error_with_too_many_nested_type_group")
{
    ScopedFastInt sfis{FInt::LuauRecursionLimit, 10};

    matchParseError(
        "function f(): ((((((((((Fail)))))))))) end", "Exceeded allowed recursion depth; simplify your type annotation to make the code compile"
    );

    matchParseError(
        "function f(): () -> () -> () -> () -> () -> () -> () -> () -> () -> () -> () end",
        "Exceeded allowed recursion depth; simplify your type annotation to make the code compile"
    );

    matchParseError(
        "local t: {a: {b: {c: {d: {e: {f: {g: {h: {i: {j: {}}}}}}}}}}}",
        "Exceeded allowed recursion depth; simplify your type annotation to make the code compile"
    );

    matchParseError("local f: ((((((((((Fail))))))))))", "Exceeded allowed recursion depth; simplify your type annotation to make the code compile");

    matchParseError(
        "local t: a & (b & (c & (d & (e & (f & (g & (h & (i & (j & nil)))))))))",
        "Exceeded allowed recursion depth; simplify your type annotation to make the code compile"
    );
}

TEST_CASE_FIXTURE(Fixture, "can_parse_complex_unions_successfully")
{
    ScopedFastInt sfis[] = {{FInt::LuauRecursionLimit, 10}, {FInt::LuauTypeLengthLimit, 10}};

    parse(R"(
local f:
() -> ()
|
() -> ()
|
{a: number}
|
{b: number}
|
((number))
|
((number))
|
(a & (b & nil))
|
(a & (b & nil))
)");

    parse(R"(
local f: a? | b? | c? | d? | e? | f? | g? | h?
)");

    matchParseError(
        "local t: a & b & c & d & e & f & g & h & i & j & nil", "Exceeded allowed type length; simplify your type annotation to make the code compile"
    );
}

TEST_CASE_FIXTURE(Fixture, "parse_error_with_too_many_nested_if_statements")
{
    ScopedFastInt sfis{FInt::LuauRecursionLimit, 10};

    matchParseErrorPrefix(
        "function f() if true then if true then if true then if true then if true then if true then if true then if true then if true "
        "then if true then if true then end end end end end end end end end end end end",
        "Exceeded allowed recursion depth;"
    );
}

TEST_CASE_FIXTURE(Fixture, "parse_error_with_too_many_changed_elseif_statements")
{
    ScopedFastInt sfis{FInt::LuauRecursionLimit, 10};

    matchParseErrorPrefix(
        "function f() if false then elseif false then elseif false then elseif false then elseif false then elseif false then elseif "
        "false then elseif false then elseif false then elseif false then elseif false then end end",
        "Exceeded allowed recursion depth;"
    );
}

TEST_CASE_FIXTURE(Fixture, "parse_error_with_too_many_nested_ifelse_expressions1")
{
    ScopedFastInt sfis{FInt::LuauRecursionLimit, 10};

    matchParseError(
        "function f() return if true then 1 elseif true then 2 elseif true then 3 elseif true then 4 elseif true then 5 elseif true then "
        "6 elseif true then 7 elseif true then 8 elseif true then 9 elseif true then 10 else 11 end",
        "Exceeded allowed recursion depth; simplify your expression to make the code compile"
    );
}

TEST_CASE_FIXTURE(Fixture, "parse_error_with_too_many_nested_ifelse_expressions2")
{
    ScopedFastInt sfis{FInt::LuauRecursionLimit, 10};

    matchParseError(
        "function f() return if if if if if if if if if if true then false else true then false else true then false else true then false else true "
        "then false else true then false else true then false else true then false else true then false else true then 1 else 2 end",
        "Exceeded allowed recursion depth; simplify your expression to make the code compile"
    );
}

TEST_CASE_FIXTURE(Fixture, "unparenthesized_function_return_type_list")
{
    matchParseError(
        "function foo(): string, number end", "Expected a statement, got ','; did you forget to wrap the list of return types in parentheses?"
    );

    matchParseError(
        "function foo(): (number) -> string, string", "Expected a statement, got ','; did you forget to wrap the list of return types in parentheses?"
    );

    // Will throw if the parse fails
    parse(R"(
        type Vector3MT = {
            __add: (Vector3MT, Vector3MT) -> Vector3MT,
            __mul: (Vector3MT, Vector3MT|number) -> Vector3MT
        }
    )");
}

TEST_CASE_FIXTURE(Fixture, "short_array_types")
{
    AstStatBlock* stat = parse(R"(
        local n: {string}
    )");

    REQUIRE(stat != nullptr);
    AstStatLocal* local = stat->body.data[0]->as<AstStatLocal>();
    AstTypeTable* annotation = local->vars.data[0]->annotation->as<AstTypeTable>();
    REQUIRE(annotation != nullptr);
    CHECK(annotation->props.size == 0);
    REQUIRE(annotation->indexer);
    REQUIRE(annotation->indexer->indexType->is<AstTypeReference>());
    CHECK(annotation->indexer->indexType->as<AstTypeReference>()->name == "number");
    REQUIRE(annotation->indexer->resultType->is<AstTypeReference>());
    CHECK(annotation->indexer->resultType->as<AstTypeReference>()->name == "string");
}

TEST_CASE_FIXTURE(Fixture, "short_array_types_must_be_alone")
{
    matchParseError("local n: {string, number}", "Expected '}' (to close '{' at column 10), got ','");
    matchParseError("local n: {[number]: string, number}", "Expected ':' when parsing table field, got '}'");
    matchParseError("local n: {x: string, number}", "Expected ':' when parsing table field, got '}'");
    matchParseError("local n: {x: string, nil}", "Expected identifier when parsing table field, got 'nil'");
}

TEST_CASE_FIXTURE(Fixture, "short_array_types_do_not_break_field_names")
{
    AstStatBlock* stat = parse(R"(
        local n: {string: number}
    )");

    REQUIRE(stat != nullptr);
    AstStatLocal* local = stat->body.data[0]->as<AstStatLocal>();
    AstTypeTable* annotation = local->vars.data[0]->annotation->as<AstTypeTable>();
    REQUIRE(annotation != nullptr);
    REQUIRE(annotation->props.size == 1);
    CHECK(!annotation->indexer);
    REQUIRE(annotation->props.data[0].name == "string");
    REQUIRE(annotation->props.data[0].type->is<AstTypeReference>());
    REQUIRE(annotation->props.data[0].type->as<AstTypeReference>()->name == "number");
}

TEST_CASE_FIXTURE(Fixture, "short_array_types_are_not_field_names_when_complex")
{
    matchParseError("local n: {string | number: number}", "Expected '}' (to close '{' at column 10), got ':'");
}

TEST_CASE_FIXTURE(Fixture, "nil_can_not_be_a_field_name")
{
    matchParseError("local n: {nil: number}", "Expected '}' (to close '{' at column 10), got ':'");
}

TEST_CASE_FIXTURE(Fixture, "string_literal_call")
{
    AstStatBlock* stat = parse("do foo 'bar' end");
    REQUIRE(stat != nullptr);
    AstStatBlock* dob = stat->body.data[0]->as<AstStatBlock>();
    AstStatExpr* stc = dob->body.data[0]->as<AstStatExpr>();
    REQUIRE(stc != nullptr);
    AstExprCall* ec = stc->expr->as<AstExprCall>();
    CHECK(ec->args.size == 1);
    AstExprConstantString* arg = ec->args.data[0]->as<AstExprConstantString>();
    REQUIRE(arg != nullptr);
    CHECK(std::string(arg->value.data, arg->value.size) == "bar");
}

TEST_CASE_FIXTURE(Fixture, "multiline_strings_newlines")
{
    AstStatBlock* stat = parse("return [=[\nfoo\r\nbar\n\nbaz\n]=]");
    REQUIRE(stat != nullptr);

    AstStatReturn* ret = stat->body.data[0]->as<AstStatReturn>();
    REQUIRE(ret != nullptr);

    AstExprConstantString* str = ret->list.data[0]->as<AstExprConstantString>();
    REQUIRE(str != nullptr);
    CHECK(std::string(str->value.data, str->value.size) == "foo\nbar\n\nbaz\n");
}

TEST_CASE_FIXTURE(Fixture, "string_literals_escape")
{
    AstStatBlock* stat = parse(R"(
return
"foo\n\r",
"foo\0324",
"foo\x204",
"foo\u{20}",
"foo\u{0451}"
)");

    REQUIRE(stat != nullptr);

    AstStatReturn* ret = stat->body.data[0]->as<AstStatReturn>();
    REQUIRE(ret != nullptr);
    CHECK(ret->list.size == 5);

    AstExprConstantString* str;

    str = ret->list.data[0]->as<AstExprConstantString>();
    REQUIRE(str != nullptr);
    CHECK_EQ(std::string(str->value.data, str->value.size), "foo\n\r");

    str = ret->list.data[1]->as<AstExprConstantString>();
    REQUIRE(str != nullptr);
    CHECK_EQ(std::string(str->value.data, str->value.size), "foo 4");

    str = ret->list.data[2]->as<AstExprConstantString>();
    REQUIRE(str != nullptr);
    CHECK_EQ(std::string(str->value.data, str->value.size), "foo 4");

    str = ret->list.data[3]->as<AstExprConstantString>();
    REQUIRE(str != nullptr);
    CHECK_EQ(std::string(str->value.data, str->value.size), "foo ");

    str = ret->list.data[4]->as<AstExprConstantString>();
    REQUIRE(str != nullptr);
    CHECK_EQ(std::string(str->value.data, str->value.size), "foo\xd1\x91");
}

TEST_CASE_FIXTURE(Fixture, "string_literals_escape_newline")
{
    AstStatBlock* stat = parse("return \"foo\\z\n   bar\", \"foo\\\n    bar\", \"foo\\\r\nbar\"");

    REQUIRE(stat != nullptr);

    AstStatReturn* ret = stat->body.data[0]->as<AstStatReturn>();
    REQUIRE(ret != nullptr);
    CHECK(ret->list.size == 3);

    AstExprConstantString* str;

    str = ret->list.data[0]->as<AstExprConstantString>();
    REQUIRE(str != nullptr);
    CHECK_EQ(std::string(str->value.data, str->value.size), "foobar");

    str = ret->list.data[1]->as<AstExprConstantString>();
    REQUIRE(str != nullptr);
    CHECK_EQ(std::string(str->value.data, str->value.size), "foo\n    bar");

    str = ret->list.data[2]->as<AstExprConstantString>();
    REQUIRE(str != nullptr);
    CHECK_EQ(std::string(str->value.data, str->value.size), "foo\nbar");
}

TEST_CASE_FIXTURE(Fixture, "string_literals_escapes")
{
    AstStatBlock* stat = parse(R"(
return
"\xAB",
"\u{2024}",
"\121",
"\1x",
"\t",
"\n"
)");

    REQUIRE(stat != nullptr);

    AstStatReturn* ret = stat->body.data[0]->as<AstStatReturn>();
    REQUIRE(ret != nullptr);
    CHECK(ret->list.size == 6);

    AstExprConstantString* str;

    str = ret->list.data[0]->as<AstExprConstantString>();
    REQUIRE(str != nullptr);
    CHECK_EQ(std::string(str->value.data, str->value.size), "\xAB");

    str = ret->list.data[1]->as<AstExprConstantString>();
    REQUIRE(str != nullptr);
    CHECK_EQ(std::string(str->value.data, str->value.size), "\xE2\x80\xA4");

    str = ret->list.data[2]->as<AstExprConstantString>();
    REQUIRE(str != nullptr);
    CHECK_EQ(std::string(str->value.data, str->value.size), "\x79");

    str = ret->list.data[3]->as<AstExprConstantString>();
    REQUIRE(str != nullptr);
    CHECK_EQ(std::string(str->value.data, str->value.size), "\x01x");

    str = ret->list.data[4]->as<AstExprConstantString>();
    REQUIRE(str != nullptr);
    CHECK_EQ(std::string(str->value.data, str->value.size), "\t");

    str = ret->list.data[5]->as<AstExprConstantString>();
    REQUIRE(str != nullptr);
    CHECK_EQ(std::string(str->value.data, str->value.size), "\n");
}

TEST_CASE_FIXTURE(Fixture, "parse_error_broken_comment")
{
    const char* expected = "Expected identifier when parsing expression, got unfinished comment";

    matchParseError("--[[unfinished work", expected);
    matchParseError("--!strict\n--[[unfinished work", expected);
    matchParseError("local x = 1 --[[unfinished work", expected);
}

TEST_CASE_FIXTURE(Fixture, "string_literals_escapes_broken")
{
    const char* expected = "String literal contains malformed escape sequence";

    matchParseError("return \"\\u{\"", expected);
    matchParseError("return \"\\u{FO}\"", expected);
    matchParseError("return \"\\u{123456789}\"", expected);
    matchParseError("return \"\\359\"", expected);
    matchParseError("return \"\\xFO\"", expected);
    matchParseError("return \"\\xF\"", expected);
    matchParseError("return \"\\x\"", expected);
}

TEST_CASE_FIXTURE(Fixture, "string_literals_broken")
{
    matchParseError("return \"", "Malformed string; did you forget to finish it?");
    matchParseError("return \"\\", "Malformed string; did you forget to finish it?");
    matchParseError("return \"\r\r", "Malformed string; did you forget to finish it?");
}

TEST_CASE_FIXTURE(Fixture, "number_literals")
{
    AstStatBlock* stat = parse(R"(
return
1,
1.5,
.5,
12_34_56,
0x1234,
 0b010101
)");

    REQUIRE(stat != nullptr);

    AstStatReturn* ret = stat->body.data[0]->as<AstStatReturn>();
    REQUIRE(ret != nullptr);
    CHECK(ret->list.size == 6);

    AstExprConstantNumber* num;

    num = ret->list.data[0]->as<AstExprConstantNumber>();
    REQUIRE(num != nullptr);
    CHECK_EQ(num->value, 1.0);

    num = ret->list.data[1]->as<AstExprConstantNumber>();
    REQUIRE(num != nullptr);
    CHECK_EQ(num->value, 1.5);

    num = ret->list.data[2]->as<AstExprConstantNumber>();
    REQUIRE(num != nullptr);
    CHECK_EQ(num->value, 0.5);

    num = ret->list.data[3]->as<AstExprConstantNumber>();
    REQUIRE(num != nullptr);
    CHECK_EQ(num->value, 123456);

    num = ret->list.data[4]->as<AstExprConstantNumber>();
    REQUIRE(num != nullptr);
    CHECK_EQ(num->value, 0x1234);

    num = ret->list.data[5]->as<AstExprConstantNumber>();
    REQUIRE(num != nullptr);
    CHECK_EQ(num->value, 0x15);
}

TEST_CASE_FIXTURE(Fixture, "end_extent_of_functions_unions_and_intersections")
{
    AstStatBlock* block = parse(R"(
        type F = (string) -> string
        type G = string | number | boolean
        type H = string & number & boolean
        print('hello')
    )");

    REQUIRE_EQ(4, block->body.size);
    CHECK_EQ((Position{1, 35}), block->body.data[0]->location.end);
    CHECK_EQ((Position{2, 42}), block->body.data[1]->location.end);
    CHECK_EQ((Position{3, 42}), block->body.data[2]->location.end);
}

TEST_CASE_FIXTURE(Fixture, "end_extent_doesnt_consume_comments")
{
    AstStatBlock* block = parse(R"(
        type F = number
        --comment
        print('hello')
    )");

    REQUIRE_EQ(2, block->body.size);
    CHECK_EQ((Position{1, 23}), block->body.data[0]->location.end);
}

TEST_CASE_FIXTURE(Fixture, "end_extent_doesnt_consume_comments_even_with_capture")
{
    // Same should hold when comments are captured
    ParseOptions opts;
    opts.captureComments = true;

    AstStatBlock* block = parse(
        R"(
        type F = number
        --comment
        print('hello')
    )",
        opts
    );

    REQUIRE_EQ(2, block->body.size);
    CHECK_EQ((Position{1, 23}), block->body.data[0]->location.end);
}

TEST_CASE_FIXTURE(Fixture, "parse_error_loop_control")
{
    matchParseError("break", "break statement must be inside a loop");
    matchParseError("repeat local function a() break end until false", "break statement must be inside a loop");
    matchParseError("continue", "continue statement must be inside a loop");
    matchParseError("repeat local function a() continue end until false", "continue statement must be inside a loop");
}

TEST_CASE_FIXTURE(Fixture, "parse_error_confusing_function_call")
{
    auto result1 = matchParseError(
        R"(
        function add(x, y) return x + y end
        add
        (4, 7)
    )",
        "Ambiguous syntax: this looks like an argument list for a function call, but could also be a start of new statement; use ';' to separate "
        "statements"
    );

    CHECK(result1.errors.size() == 1);

    auto result2 = matchParseError(
        R"(
        function add(x, y) return x + y end
        local f = add
        (f :: any)['x'] = 2
    )",
        "Ambiguous syntax: this looks like an argument list for a function call, but could also be a start of new statement; use ';' to separate "
        "statements"
    );

    CHECK(result2.errors.size() == 1);

    auto result3 = matchParseError(
        R"(
        local x = {}
        function x:add(a, b) return a + b end
        x:add
        (1, 2)
    )",
        "Ambiguous syntax: this looks like an argument list for a function call, but could also be a start of new statement; use ';' to separate "
        "statements"
    );

    CHECK(result3.errors.size() == 1);

    auto result4 = matchParseError(
        R"(
        local t = {}
        function f() return t end
        t.x, (f)
        ().y = 5, 6
    )",
        "Ambiguous syntax: this looks like an argument list for a function call, but could also be a start of new statement; use ';' to separate "
        "statements"
    );

    CHECK(result4.errors.size() == 1);
}

TEST_CASE_FIXTURE(Fixture, "parse_error_varargs")
{
    matchParseError("function add(x, y) return ... end", "Cannot use '...' outside of a vararg function");
}

TEST_CASE_FIXTURE(Fixture, "parse_error_assignment_lvalue")
{
    matchParseError(
        R"(
        local a, b
        (2), b = b, a
    )",
        "Assigned expression must be a variable or a field"
    );

    matchParseError(
        R"(
        local a, b
        a, (3) = b, a
    )",
        "Assigned expression must be a variable or a field"
    );
}

TEST_CASE_FIXTURE(Fixture, "parse_error_type_annotation")
{
    matchParseError("local a : 2 = 2", "Expected type, got '2'");
}

TEST_CASE_FIXTURE(Fixture, "parse_error_missing_type_annotation")
{
    {
        ParseResult result = tryParse("local x:");
        CHECK(result.errors.size() == 1);
        Position begin = result.errors[0].getLocation().begin;
        Position end = result.errors[0].getLocation().end;
        CHECK(begin.line == end.line);
        int width = end.column - begin.column;
        CHECK(width == 0);
        CHECK(result.errors[0].getMessage() == "Expected type, got <eof>");
    }

    {
        ParseResult result = tryParse(R"(
local x:=42
    )");
        CHECK(result.errors.size() == 1);
        Position begin = result.errors[0].getLocation().begin;
        Position end = result.errors[0].getLocation().end;
        CHECK(begin.line == end.line);
        int width = end.column - begin.column;
        CHECK(width == 1); // Length of `=`
        CHECK(result.errors[0].getMessage() == "Expected type, got '='");
    }

    {
        ParseResult result = tryParse(R"(
function func():end
    )");
        CHECK(result.errors.size() == 1);
        Position begin = result.errors[0].getLocation().begin;
        Position end = result.errors[0].getLocation().end;
        CHECK(begin.line == end.line);
        int width = end.column - begin.column;
        CHECK(width == 3); // Length of `end`
        CHECK(result.errors[0].getMessage() == "Expected type, got 'end'");
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_declarations")
{
    AstStatBlock* stat = parseEx(R"(
        declare foo: number
        declare function bar(x: number): string
        declare function var(...: any)
    )")
                             .root;

    REQUIRE(stat);
    REQUIRE_EQ(stat->body.size, 3);

    AstStatDeclareGlobal* global = stat->body.data[0]->as<AstStatDeclareGlobal>();
    REQUIRE(global);
    CHECK(global->name == "foo");
    CHECK(global->nameLocation == Location({1, 16}, {1, 19}));
    CHECK(global->type);

    AstStatDeclareFunction* func = stat->body.data[1]->as<AstStatDeclareFunction>();
    REQUIRE(func);
    CHECK(func->name == "bar");
    CHECK(func->nameLocation == Location({2, 25}, {2, 28}));
    REQUIRE_EQ(func->params.types.size, 1);

    auto retTypePack = func->retTypes->as<AstTypePackExplicit>();
    REQUIRE(retTypePack);
    REQUIRE_EQ(retTypePack->typeList.types.size, 1);

    AstStatDeclareFunction* varFunc = stat->body.data[2]->as<AstStatDeclareFunction>();
    REQUIRE(varFunc);
    CHECK(varFunc->name == "var");
    CHECK(varFunc->nameLocation == Location({3, 25}, {3, 28}));
    CHECK(varFunc->params.tailType);
    CHECK(varFunc->vararg);
    CHECK(varFunc->varargLocation == Location({3, 29}, {3, 32}));

    matchParseError("declare function foo(x)", "All declaration parameters must be annotated");
    matchParseError("declare foo", "Expected ':' when parsing global variable declaration, got <eof>");
}

TEST_CASE_FIXTURE(Fixture, "parse_class_declarations")
{
    AstStatBlock* stat = parseEx(R"(
        declare class Foo
            prop: number
            function method(self, foo: number): string
        end

        declare class Bar extends Foo
            prop2: string
        end
    )")
                             .root;

    REQUIRE_EQ(stat->body.size, 2);

    AstStatDeclareExternType* declaredExternType = stat->body.data[0]->as<AstStatDeclareExternType>();
    REQUIRE(declaredExternType);
    CHECK(declaredExternType->name == "Foo");
    CHECK(!declaredExternType->superName);

    REQUIRE_EQ(declaredExternType->props.size, 2);

    AstDeclaredExternTypeProperty& prop = declaredExternType->props.data[0];
    CHECK(prop.name == "prop");
    CHECK(prop.nameLocation == Location({2, 12}, {2, 16}));
    CHECK(prop.ty->is<AstTypeReference>());
    CHECK(prop.location == Location({2, 12}, {2, 24}));

    AstDeclaredExternTypeProperty& method = declaredExternType->props.data[1];
    CHECK(method.name == "method");
    CHECK(method.nameLocation == Location({3, 21}, {3, 27}));
    CHECK(method.ty->is<AstTypeFunction>());
    CHECK(method.location == Location({3, 12}, {3, 54}));
    CHECK(method.isMethod);

    AstStatDeclareExternType* subclass = stat->body.data[1]->as<AstStatDeclareExternType>();
    REQUIRE(subclass);
    REQUIRE(subclass->superName);
    CHECK(subclass->name == "Bar");
    CHECK(*subclass->superName == "Foo");

    REQUIRE_EQ(subclass->props.size, 1);
    AstDeclaredExternTypeProperty& prop2 = subclass->props.data[0];
    CHECK(prop2.name == "prop2");
    CHECK(prop2.nameLocation == Location({7, 12}, {7, 17}));
    CHECK(prop2.ty->is<AstTypeReference>());
    CHECK(prop2.location == Location({7, 12}, {7, 25}));
}

TEST_CASE_FIXTURE(Fixture, "parse_extern_type_declarations")
{
    AstStatBlock* stat = parseEx(R"(
        declare extern type Foo with
            prop: number
            function method(self, foo: number): string
        end

        declare extern type Bar extends Foo with
            prop2: string
        end
    )")
                             .root;

    REQUIRE_EQ(stat->body.size, 2);

    AstStatDeclareExternType* declaredExternType = stat->body.data[0]->as<AstStatDeclareExternType>();
    REQUIRE(declaredExternType);
    CHECK(declaredExternType->name == "Foo");
    CHECK(!declaredExternType->superName);

    REQUIRE_EQ(declaredExternType->props.size, 2);

    AstDeclaredExternTypeProperty& prop = declaredExternType->props.data[0];
    CHECK(prop.name == "prop");
    CHECK(prop.nameLocation == Location({2, 12}, {2, 16}));
    CHECK(prop.ty->is<AstTypeReference>());
    CHECK(prop.location == Location({2, 12}, {2, 24}));

    AstDeclaredExternTypeProperty& method = declaredExternType->props.data[1];
    CHECK(method.name == "method");
    CHECK(method.nameLocation == Location({3, 21}, {3, 27}));
    CHECK(method.ty->is<AstTypeFunction>());
    CHECK(method.location == Location({3, 12}, {3, 54}));
    CHECK(method.isMethod);

    AstStatDeclareExternType* subclass = stat->body.data[1]->as<AstStatDeclareExternType>();
    REQUIRE(subclass);
    REQUIRE(subclass->superName);
    CHECK(subclass->name == "Bar");
    CHECK(*subclass->superName == "Foo");

    REQUIRE_EQ(subclass->props.size, 1);
    AstDeclaredExternTypeProperty& prop2 = subclass->props.data[0];
    CHECK(prop2.name == "prop2");
    CHECK(prop2.nameLocation == Location({7, 12}, {7, 17}));
    CHECK(prop2.ty->is<AstTypeReference>());
    CHECK(prop2.location == Location({7, 12}, {7, 25}));
}

TEST_CASE_FIXTURE(Fixture, "parse_extern_type_declarations_missing_with")
{
    ParseResult result = tryParse(R"(
        declare extern type Foo
            prop: number
            function method(self, foo: number): string
        end

        declare extern type Bar extends Foo
            prop2: string
        end
    )");

    REQUIRE_EQ(result.errors.size(), 2);
    CHECK("Expected `with` keyword before listing properties of the external type, but got prop instead" == result.errors[0].getMessage());
    CHECK("Expected `with` keyword before listing properties of the external type, but got prop2 instead" == result.errors[1].getMessage());

    AstStatBlock* stat = result.root;

    REQUIRE_EQ(stat->body.size, 2);

    AstStatDeclareExternType* declaredExternType = stat->body.data[0]->as<AstStatDeclareExternType>();
    REQUIRE(declaredExternType);
    CHECK(declaredExternType->name == "Foo");
    CHECK(!declaredExternType->superName);

    REQUIRE_EQ(declaredExternType->props.size, 2);

    AstDeclaredExternTypeProperty& prop = declaredExternType->props.data[0];
    CHECK(prop.name == "prop");
    CHECK(prop.nameLocation == Location({2, 12}, {2, 16}));
    CHECK(prop.ty->is<AstTypeReference>());
    CHECK(prop.location == Location({2, 12}, {2, 24}));

    AstDeclaredExternTypeProperty& method = declaredExternType->props.data[1];
    CHECK(method.name == "method");
    CHECK(method.nameLocation == Location({3, 21}, {3, 27}));
    CHECK(method.ty->is<AstTypeFunction>());
    CHECK(method.location == Location({3, 12}, {3, 54}));
    CHECK(method.isMethod);

    AstStatDeclareExternType* subclass = stat->body.data[1]->as<AstStatDeclareExternType>();
    REQUIRE(subclass);
    REQUIRE(subclass->superName);
    CHECK(subclass->name == "Bar");
    CHECK(*subclass->superName == "Foo");

    REQUIRE_EQ(subclass->props.size, 1);
    AstDeclaredExternTypeProperty& prop2 = subclass->props.data[0];
    CHECK(prop2.name == "prop2");
    CHECK(prop2.nameLocation == Location({7, 12}, {7, 17}));
    CHECK(prop2.ty->is<AstTypeReference>());
    CHECK(prop2.location == Location({7, 12}, {7, 25}));
}

TEST_CASE_FIXTURE(Fixture, "parse_extern_type_declarations")
{
    AstStatBlock* stat = parseEx(R"(
        declare extern type Foo with
            prop: number
            function method(self, foo: number): string
        end

        declare extern type Bar extends Foo with
            prop2: string
        end
    )")
                             .root;

    REQUIRE_EQ(stat->body.size, 2);

    AstStatDeclareExternType* declaredExternType = stat->body.data[0]->as<AstStatDeclareExternType>();
    REQUIRE(declaredExternType);
    CHECK(declaredExternType->name == "Foo");
    CHECK(!declaredExternType->superName);

    REQUIRE_EQ(declaredExternType->props.size, 2);

    AstDeclaredExternTypeProperty& prop = declaredExternType->props.data[0];
    CHECK(prop.name == "prop");
    CHECK(prop.nameLocation == Location({2, 12}, {2, 16}));
    CHECK(prop.ty->is<AstTypeReference>());
    CHECK(prop.location == Location({2, 12}, {2, 24}));

    AstDeclaredExternTypeProperty& method = declaredExternType->props.data[1];
    CHECK(method.name == "method");
    CHECK(method.nameLocation == Location({3, 21}, {3, 27}));
    CHECK(method.ty->is<AstTypeFunction>());
    CHECK(method.location == Location({3, 12}, {3, 54}));
    CHECK(method.isMethod);

    AstStatDeclareExternType* subclass = stat->body.data[1]->as<AstStatDeclareExternType>();
    REQUIRE(subclass);
    REQUIRE(subclass->superName);
    CHECK(subclass->name == "Bar");
    CHECK(*subclass->superName == "Foo");

    REQUIRE_EQ(subclass->props.size, 1);
    AstDeclaredExternTypeProperty& prop2 = subclass->props.data[0];
    CHECK(prop2.name == "prop2");
    CHECK(prop2.nameLocation == Location({7, 12}, {7, 17}));
    CHECK(prop2.ty->is<AstTypeReference>());
    CHECK(prop2.location == Location({7, 12}, {7, 25}));
}

TEST_CASE_FIXTURE(Fixture, "parse_extern_type_declarations_missing_with")
{
    ParseResult result = tryParse(R"(
        declare extern type Foo
            prop: number
            function method(self, foo: number): string
        end

        declare extern type Bar extends Foo
            prop2: string
        end
    )");

    REQUIRE_EQ(result.errors.size(), 2);
    CHECK("Expected `with` keyword before listing properties of the external type, but got prop instead" == result.errors[0].getMessage());
    CHECK("Expected `with` keyword before listing properties of the external type, but got prop2 instead" == result.errors[1].getMessage());

    AstStatBlock* stat = result.root;

    REQUIRE_EQ(stat->body.size, 2);

    AstStatDeclareExternType* declaredExternType = stat->body.data[0]->as<AstStatDeclareExternType>();
    REQUIRE(declaredExternType);
    CHECK(declaredExternType->name == "Foo");
    CHECK(!declaredExternType->superName);

    REQUIRE_EQ(declaredExternType->props.size, 2);

    AstDeclaredExternTypeProperty& prop = declaredExternType->props.data[0];
    CHECK(prop.name == "prop");
    CHECK(prop.nameLocation == Location({2, 12}, {2, 16}));
    CHECK(prop.ty->is<AstTypeReference>());
    CHECK(prop.location == Location({2, 12}, {2, 24}));

    AstDeclaredExternTypeProperty& method = declaredExternType->props.data[1];
    CHECK(method.name == "method");
    CHECK(method.nameLocation == Location({3, 21}, {3, 27}));
    CHECK(method.ty->is<AstTypeFunction>());
    CHECK(method.location == Location({3, 12}, {3, 54}));
    CHECK(method.isMethod);

    AstStatDeclareExternType* subclass = stat->body.data[1]->as<AstStatDeclareExternType>();
    REQUIRE(subclass);
    REQUIRE(subclass->superName);
    CHECK(subclass->name == "Bar");
    CHECK(*subclass->superName == "Foo");

    REQUIRE_EQ(subclass->props.size, 1);
    AstDeclaredExternTypeProperty& prop2 = subclass->props.data[0];
    CHECK(prop2.name == "prop2");
    CHECK(prop2.nameLocation == Location({7, 12}, {7, 17}));
    CHECK(prop2.ty->is<AstTypeReference>());
    CHECK(prop2.location == Location({7, 12}, {7, 25}));
}

TEST_CASE_FIXTURE(Fixture, "class_method_properties")
{
    const ParseResult p1 = matchParseError(
        R"(
        declare class Foo
            -- method's first parameter must be 'self'
            function method(foo: number)
            function method2(self)
        end
        )",
        "'self' must be present as the unannotated first parameter"
    );

    REQUIRE_EQ(1, p1.root->body.size);

    AstStatDeclareExternType* klass = p1.root->body.data[0]->as<AstStatDeclareExternType>();
    REQUIRE(klass != nullptr);

    CHECK_EQ(2, klass->props.size);

    const ParseResult p2 = matchParseError(
        R"(
        declare class Foo
            function method(self, foo)
            function method2()
        end
        )",
        "All declaration parameters aside from 'self' must be annotated"
    );

    REQUIRE_EQ(1, p2.root->body.size);

    AstStatDeclareExternType* klass2 = p2.root->body.data[0]->as<AstStatDeclareExternType>();
    REQUIRE(klass2 != nullptr);

    CHECK_EQ(2, klass2->props.size);
}

TEST_CASE_FIXTURE(Fixture, "class_indexer")
{
    AstStatBlock* stat = parseEx(R"(
        declare class Foo
            prop: boolean
            [string]: number
        end
    )")
                             .root;

    REQUIRE_EQ(stat->body.size, 1);

    AstStatDeclareExternType* declaredExternType = stat->body.data[0]->as<AstStatDeclareExternType>();
    REQUIRE(declaredExternType);
    REQUIRE(declaredExternType->indexer);
    REQUIRE(declaredExternType->indexer->indexType->is<AstTypeReference>());
    CHECK(declaredExternType->indexer->indexType->as<AstTypeReference>()->name == "string");
    REQUIRE(declaredExternType->indexer->resultType->is<AstTypeReference>());
    CHECK(declaredExternType->indexer->resultType->as<AstTypeReference>()->name == "number");

    const ParseResult p1 = matchParseError(
        R"(
        declare class Foo
            [string]: number
            -- can only have one indexer
            [number]: number
        end
        )",
        "Cannot have more than one indexer on an extern type"
    );

    REQUIRE_EQ(1, p1.root->body.size);

    AstStatDeclareExternType* klass = p1.root->body.data[0]->as<AstStatDeclareExternType>();
    REQUIRE(klass != nullptr);
    CHECK(klass->indexer);
}

TEST_CASE_FIXTURE(Fixture, "parse_variadics")
{
    AstStatBlock* stat = parseEx(R"(
        function foo(bar, ...: number): ...string
        end

        type Foo = (string, number, ...number) -> ...boolean
        type Bar = () -> (number, ...boolean)
    )")
                             .root;

    REQUIRE(stat);
    REQUIRE_EQ(stat->body.size, 3);

    AstStatFunction* fn = stat->body.data[0]->as<AstStatFunction>();
    REQUIRE(fn);
    CHECK(fn->func->vararg);
    CHECK(fn->func->varargAnnotation);

    AstStatTypeAlias* foo = stat->body.data[1]->as<AstStatTypeAlias>();
    REQUIRE(foo);
    AstTypeFunction* fnFoo = foo->type->as<AstTypeFunction>();
    REQUIRE(fnFoo);
    CHECK_EQ(fnFoo->argTypes.types.size, 2);
    CHECK(fnFoo->argTypes.tailType);
    CHECK(fnFoo->returnTypes->is<AstTypePackVariadic>());

    AstStatTypeAlias* bar = stat->body.data[2]->as<AstStatTypeAlias>();
    REQUIRE(bar);
    AstTypeFunction* fnBar = bar->type->as<AstTypeFunction>();
    REQUIRE(fnBar);
    CHECK_EQ(fnBar->argTypes.types.size, 0);
    CHECK(!fnBar->argTypes.tailType);
    auto returnTypePack = fnBar->returnTypes->as<AstTypePackExplicit>();
    REQUIRE(returnTypePack);
    CHECK_EQ(returnTypePack->typeList.types.size, 1);
    CHECK(returnTypePack->typeList.tailType);
}

TEST_CASE_FIXTURE(Fixture, "variadics_must_be_last")
{
    matchParseError("function foo(): (...number, string) end", "Expected ')' (to close '(' at column 17), got ','");
    matchParseError("type Foo = (...number, string) -> (...string, number)", "Expected ')' (to close '(' at column 12), got ','");
}

TEST_CASE_FIXTURE(Fixture, "variadic_definition_parsing")
{
    AstStatBlock* stat = parseEx(R"(
        declare function foo(...: string): ...string
        declare class Foo
            function a(self, ...: string): ...string
        end
    )")
                             .root;

    REQUIRE(stat != nullptr);

    matchParseError("declare function foo(...)", "All declaration parameters must be annotated");
    matchParseError("declare class Foo function a(self, ...) end", "All declaration parameters aside from 'self' must be annotated");
}

TEST_CASE_FIXTURE(Fixture, "missing_declaration_prop")
{
    matchParseError(
        R"(
        declare class Foo
            a: number,
        end
    )",
        "Expected identifier when parsing property name, got ','"
    );
}

TEST_CASE_FIXTURE(Fixture, "generic_pack_parsing")
{
    ParseResult result = parseEx(R"(
        function f<a...>(...: a...)
        end

        type A = (a...) -> b...
    )");

    AstStatBlock* stat = result.root;
    REQUIRE(stat != nullptr);

    AstStatFunction* fn = stat->body.data[0]->as<AstStatFunction>();
    REQUIRE(fn != nullptr);
    REQUIRE(fn->func->varargAnnotation != nullptr);

    AstTypePackGeneric* annot = fn->func->varargAnnotation->as<AstTypePackGeneric>();
    REQUIRE(annot != nullptr);
    CHECK(annot->genericName == "a");

    AstStatTypeAlias* alias = stat->body.data[1]->as<AstStatTypeAlias>();
    REQUIRE(alias != nullptr);
    AstTypeFunction* fnTy = alias->type->as<AstTypeFunction>();
    REQUIRE(fnTy != nullptr);

    AstTypePackGeneric* argAnnot = fnTy->argTypes.tailType->as<AstTypePackGeneric>();
    REQUIRE(argAnnot != nullptr);
    CHECK(argAnnot->genericName == "a");

    AstTypePackGeneric* retAnnot = fnTy->returnTypes->as<AstTypePackGeneric>();
    REQUIRE(retAnnot != nullptr);
    CHECK(retAnnot->genericName == "b");
}

TEST_CASE_FIXTURE(Fixture, "generic_function_declaration_parsing")
{
    ParseResult result = parseEx(R"(
        declare function f<a, b, c...>()
    )");

    AstStatBlock* stat = result.root;
    REQUIRE(stat != nullptr);

    AstStatDeclareFunction* decl = stat->body.data[0]->as<AstStatDeclareFunction>();
    REQUIRE(decl != nullptr);
    REQUIRE_EQ(decl->generics.size, 2);
    REQUIRE_EQ(decl->genericPacks.size, 1);
}

TEST_CASE_FIXTURE(Fixture, "function_type_named_arguments")
{
    {
        ParseResult result = parseEx("type MyFunc = (a: number, b: string, c: number) -> string");

        AstStatBlock* stat = result.root;
        REQUIRE(stat != nullptr);

        AstStatTypeAlias* decl = stat->body.data[0]->as<AstStatTypeAlias>();
        REQUIRE(decl != nullptr);
        AstTypeFunction* func = decl->type->as<AstTypeFunction>();
        REQUIRE(func != nullptr);
        REQUIRE_EQ(func->argTypes.types.size, 3);
        REQUIRE_EQ(func->argNames.size, 3);
        REQUIRE(func->argNames.data[2]);
        CHECK_EQ(func->argNames.data[2]->first, "c");
    }

    {
        ParseResult result = parseEx("type MyFunc = (a: number, string, c: number) -> string");

        AstStatBlock* stat = result.root;
        REQUIRE(stat != nullptr);

        AstStatTypeAlias* decl = stat->body.data[0]->as<AstStatTypeAlias>();
        REQUIRE(decl != nullptr);
        AstTypeFunction* func = decl->type->as<AstTypeFunction>();
        REQUIRE(func != nullptr);
        REQUIRE_EQ(func->argTypes.types.size, 3);
        REQUIRE_EQ(func->argNames.size, 3);
        REQUIRE(!func->argNames.data[1]);
        REQUIRE(func->argNames.data[2]);
        CHECK_EQ(func->argNames.data[2]->first, "c");
    }

    {
        ParseResult result = parseEx("type MyFunc = (a: number, string, number) -> string");

        AstStatBlock* stat = result.root;
        REQUIRE(stat != nullptr);

        AstStatTypeAlias* decl = stat->body.data[0]->as<AstStatTypeAlias>();
        REQUIRE(decl != nullptr);
        AstTypeFunction* func = decl->type->as<AstTypeFunction>();
        REQUIRE(func != nullptr);
        REQUIRE_EQ(func->argTypes.types.size, 3);
        REQUIRE_EQ(func->argNames.size, 3);
        REQUIRE(!func->argNames.data[1]);
        REQUIRE(!func->argNames.data[2]);
    }

    {
        ParseResult result = parseEx("type MyFunc = (a: number, b: string, c: number) -> (d: number, e: string, f: number) -> string");

        AstStatBlock* stat = result.root;
        REQUIRE(stat != nullptr);

        AstStatTypeAlias* decl = stat->body.data[0]->as<AstStatTypeAlias>();
        REQUIRE(decl != nullptr);
        AstTypeFunction* func = decl->type->as<AstTypeFunction>();
        REQUIRE(func != nullptr);
        REQUIRE_EQ(func->argTypes.types.size, 3);
        REQUIRE_EQ(func->argNames.size, 3);
        REQUIRE(func->argNames.data[2]);
        CHECK_EQ(func->argNames.data[2]->first, "c");
        AstTypeFunction* funcRet = func->returnTypes->as<AstTypePackExplicit>()->typeList.types.data[0]->as<AstTypeFunction>();
        REQUIRE(funcRet != nullptr);
        REQUIRE_EQ(funcRet->argTypes.types.size, 3);
        REQUIRE_EQ(funcRet->argNames.size, 3);
        REQUIRE(func->argNames.data[2]);
        CHECK_EQ(funcRet->argNames.data[2]->first, "f");
    }

    matchParseError(
        "type MyFunc = (a: number, b: string, c: number) -> (d: number, e: string, f: number)", "Expected '->' when parsing function type, got <eof>"
    );

    matchParseError("type MyFunc = (number) -> (d: number) <a, b, c> -> number", "Expected '->' when parsing function type, got '<'");
}

TEST_CASE_FIXTURE(Fixture, "function_type_matching_parenthesis")
{
    matchParseError("local a: <T>(number -> string", "Expected ')' (to close '(' at column 13), got '->'");
}

TEST_CASE_FIXTURE(Fixture, "parse_type_alias_default_type")
{
    AstStat* stat = parse(R"(
type A<T = string> = {}
type B<T... = ...number> = {}
type C<T..., U... = T...> = {}
type D<T..., U... = ()> = {}
type E<T... = (), U... = ()> = {}
type F<T... = (string), U... = ()> = (T...) -> U...
type G<T... = ...number, U... = (string, number, boolean)> = (U...) -> T...
    )");

    REQUIRE(stat != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "parse_type_alias_default_type_errors")
{
    matchParseError("type Y<T = number, U> = {}", "Expected default type after type name", Location{{0, 20}, {0, 21}});
    matchParseError("type Y<T... = ...number, U...> = {}", "Expected default type pack after type pack name", Location{{0, 29}, {0, 30}});
    matchParseError("type Y<T... = (string) -> number> = {}", "Expected type pack after '=', got type", Location{{0, 14}, {0, 32}});
}

TEST_CASE_FIXTURE(Fixture, "parse_type_pack_errors")
{
    matchParseError(
        "type Y<T...> = {a: T..., b: number}",
        "Unexpected '...' after type name; type pack is not allowed in this context",
        Location{{0, 20}, {0, 23}}
    );
    matchParseError("type Y<T...> = {a: (number | string)...", "Unexpected '...' after type annotation", Location{{0, 36}, {0, 39}});
}

TEST_CASE_FIXTURE(Fixture, "parse_if_else_expression")
{
    {
        AstStat* stat = parse("return if true then 1 else 2");

        REQUIRE(stat != nullptr);
        AstStatReturn* str = stat->as<AstStatBlock>()->body.data[0]->as<AstStatReturn>();
        REQUIRE(str != nullptr);
        CHECK(str->list.size == 1);
        auto* ifElseExpr = str->list.data[0]->as<AstExprIfElse>();
        REQUIRE(ifElseExpr != nullptr);
    }

    {
        AstStat* stat = parse("return if true then 1 elseif true then 2 else 3");

        REQUIRE(stat != nullptr);
        AstStatReturn* str = stat->as<AstStatBlock>()->body.data[0]->as<AstStatReturn>();
        REQUIRE(str != nullptr);
        CHECK(str->list.size == 1);
        auto* ifElseExpr1 = str->list.data[0]->as<AstExprIfElse>();
        REQUIRE(ifElseExpr1 != nullptr);
        auto* ifElseExpr2 = ifElseExpr1->falseExpr->as<AstExprIfElse>();
        REQUIRE(ifElseExpr2 != nullptr);
    }

    // Use "else if" as opposed to elseif
    {
        AstStat* stat = parse("return if true then 1 else if true then 2 else 3");

        REQUIRE(stat != nullptr);
        AstStatReturn* str = stat->as<AstStatBlock>()->body.data[0]->as<AstStatReturn>();
        REQUIRE(str != nullptr);
        CHECK(str->list.size == 1);
        auto* ifElseExpr1 = str->list.data[0]->as<AstExprIfElse>();
        REQUIRE(ifElseExpr1 != nullptr);
        auto* ifElseExpr2 = ifElseExpr1->falseExpr->as<AstExprIfElse>();
        REQUIRE(ifElseExpr2 != nullptr);
    }

    // Use an if-else expression as the conditional expression of an if-else expression
    {
        AstStat* stat = parse("return if if true then false else true then 1 else 2");

        REQUIRE(stat != nullptr);
        AstStatReturn* str = stat->as<AstStatBlock>()->body.data[0]->as<AstStatReturn>();
        REQUIRE(str != nullptr);
        CHECK(str->list.size == 1);
        auto* ifElseExpr = str->list.data[0]->as<AstExprIfElse>();
        REQUIRE(ifElseExpr != nullptr);
        auto* nestedIfElseExpr = ifElseExpr->condition->as<AstExprIfElse>();
        REQUIRE(nestedIfElseExpr != nullptr);
    }
}

TEST_CASE_FIXTURE(Fixture, "parse_type_pack_type_parameters")
{
    AstStat* stat = parse(R"(
type Packed<T...> = () -> T...

type A<X...> = Packed<X...>
type B<X...> = Packed<...number>
type C<X...> = Packed<(number, X...)>
    )");
    REQUIRE(stat != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "invalid_type_forms")
{
    matchParseError("type A = (b: number)", "Expected '->' when parsing function type, got <eof>");
    matchParseError("type P<T...> = () -> T... type B = P<(x: number, y: string)>", "Expected '->' when parsing function type, got '>'");
    matchParseError("type F<T... = (a: string)> = (T...) -> ()", "Expected '->' when parsing function type, got '>'");
}

TEST_CASE_FIXTURE(Fixture, "parse_user_defined_type_functions")
{
    AstStat* stat = parse(R"(
        type function foo()
            return types.number
        end

        export type function bar()
            return types.string
        end
    )");

    REQUIRE(stat != nullptr);
    AstStatTypeFunction* f = stat->as<AstStatBlock>()->body.data[0]->as<AstStatTypeFunction>();
    REQUIRE(f != nullptr);
    REQUIRE(f->name == "foo");
}

TEST_CASE_FIXTURE(Fixture, "parse_nested_type_function")
{
    AstStat* stat = parse(R"(
        local v1 = 1
        type function foo()
            local v2 = 2
            local function bar()
                v2 += 1
                type function inner() end
                v2 += 2
            end
            local function bar2()
                v2 += 3
            end
        end
        local function bar() v1 += 1 end
    )");

    REQUIRE(stat != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "invalid_user_defined_type_functions")
{
    matchParseError("local foo = 1; type function bar() print(foo) end", "Type function cannot reference outer local 'foo'");
    matchParseError("type function foo() local v1 = 1; type function bar() print(v1) end end", "Type function cannot reference outer local 'v1'");
}

TEST_CASE_FIXTURE(Fixture, "leading_union_intersection_with_single_type_preserves_the_union_intersection_ast_node")
{
    AstStatBlock* block = parse(R"(
        type Foo = | string
        type Bar = & number
    )");

    REQUIRE_EQ(2, block->body.size);

    const auto alias1 = block->body.data[0]->as<AstStatTypeAlias>();
    REQUIRE(alias1);

    const auto unionType = alias1->type->as<AstTypeUnion>();
    REQUIRE(unionType);
    CHECK_EQ(1, unionType->types.size);

    const auto alias2 = block->body.data[1]->as<AstStatTypeAlias>();
    REQUIRE(alias2);

    const auto intersectionType = alias2->type->as<AstTypeIntersection>();
    REQUIRE(intersectionType);
    CHECK_EQ(1, intersectionType->types.size);
}

TEST_CASE_FIXTURE(Fixture, "parse_simple_ast_type_group")
{
    AstStatBlock* stat = parse(R"(
        type Foo = (string)
    )");
    REQUIRE(stat);
    REQUIRE_EQ(1, stat->body.size);

    auto alias1 = stat->body.data[0]->as<AstStatTypeAlias>();
    REQUIRE(alias1);

    auto group1 = alias1->type->as<AstTypeGroup>();
    REQUIRE(group1);
    CHECK(group1->type->is<AstTypeReference>());
}

TEST_CASE_FIXTURE(Fixture, "parse_nested_ast_type_group")
{
    AstStatBlock* stat = parse(R"(
        type Foo = ((string))
    )");
    REQUIRE(stat);
    REQUIRE_EQ(1, stat->body.size);

    auto alias1 = stat->body.data[0]->as<AstStatTypeAlias>();
    REQUIRE(alias1);

    auto group1 = alias1->type->as<AstTypeGroup>();
    REQUIRE(group1);

    auto group2 = group1->type->as<AstTypeGroup>();
    REQUIRE(group2);
    CHECK(group2->type->is<AstTypeReference>());
}

TEST_CASE_FIXTURE(Fixture, "parse_return_type_ast_type_group")
{
    AstStatBlock* stat = parse(R"(
        type Foo = () -> (string)
    )");
    REQUIRE(stat);
    REQUIRE_EQ(1, stat->body.size);

    auto alias1 = stat->body.data[0]->as<AstStatTypeAlias>();
    REQUIRE(alias1);

    auto funcType = alias1->type->as<AstTypeFunction>();
    REQUIRE(funcType);

    auto returnTypePack = funcType->returnTypes->as<AstTypePackExplicit>();
    REQUIRE(returnTypePack);
    REQUIRE_EQ(1, returnTypePack->typeList.types.size);
    REQUIRE(!returnTypePack->typeList.tailType);
    CHECK(returnTypePack->typeList.types.data[0]->is<AstTypeGroup>());
}

TEST_CASE_FIXTURE(Fixture, "inner_and_outer_scope_of_functions_have_correct_end_position")
{

    AstStatBlock* stat = parse(R"(
        local function foo()
            local x = 1
        end
    )");
    REQUIRE(stat);
    REQUIRE_EQ(1, stat->body.size);

    auto func = stat->body.data[0]->as<AstStatLocalFunction>();
    REQUIRE(func);
    CHECK_EQ(func->func->body->location, Location{{1, 28}, {3, 8}});
    CHECK_EQ(func->location, Location{{1, 8}, {3, 11}});
}

TEST_CASE_FIXTURE(Fixture, "do_block_end_location_is_after_end_token")
{
    AstStatBlock* stat = parse(R"(
        do
            local x = 1
        end
    )");
    REQUIRE(stat);
    REQUIRE_EQ(1, stat->body.size);

    auto block = stat->body.data[0]->as<AstStatBlock>();
    REQUIRE(block);
    CHECK_EQ(block->location, Location{{1, 8}, {3, 11}});
}

TEST_CASE_FIXTURE(Fixture, "function_start_locations_are_before_attributes")
{
    AstStatBlock* stat = parse(R"(
        @native
        function globalFunction()
        end

        @native
        local function localFunction()
        end

        local _ = @native function()
        end
    )");
    REQUIRE(stat);
    REQUIRE_EQ(3, stat->body.size);

    auto globalFunction = stat->body.data[0]->as<AstStatFunction>();
    REQUIRE(globalFunction);
    CHECK_EQ(globalFunction->location, Location({1, 8}, {3, 11}));

    auto localFunction = stat->body.data[1]->as<AstStatLocalFunction>();
    REQUIRE(localFunction);
    CHECK_EQ(localFunction->location, Location({5, 8}, {7, 11}));

    auto localVariable = stat->body.data[2]->as<AstStatLocal>();
    REQUIRE(localVariable);
    REQUIRE_EQ(localVariable->values.size, 1);
    auto anonymousFunction = localVariable->values.data[0]->as<AstExprFunction>();
    CHECK_EQ(anonymousFunction->location, Location({9, 18}, {10, 11}));
}

TEST_CASE_FIXTURE(Fixture, "for_loop_with_single_var_has_comma_positions_of_size_zero")
{
    ParseOptions parseOptions;
    parseOptions.storeCstData = true;

    ParseResult result = parseEx(
        R"(
        for value in tbl do
        end
    )",
        parseOptions
    );
    REQUIRE(result.root);
    REQUIRE_EQ(1, result.root->body.size);

    auto forLoop = result.root->body.data[0]->as<AstStatForIn>();
    auto baseCstNode = result.cstNodeMap.find(forLoop);
    REQUIRE(baseCstNode);

    auto cstNode = (*baseCstNode)->as<CstStatForIn>();
    CHECK_EQ(cstNode->varsCommaPositions.size, 0);
}

TEST_SUITE_END();

TEST_SUITE_BEGIN("ParseErrorRecovery");

TEST_CASE_FIXTURE(Fixture, "multiple_parse_errors")
{
    try
    {
        parse(R"(
local a = 3 * (
return a +
)");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const Luau::ParseErrors& e)
    {
        CHECK_EQ(2, e.getErrors().size());
    }
}

// check that we are not skipping tokens that weren't processed at all
TEST_CASE_FIXTURE(Fixture, "statement_error_recovery_expected")
{
    try
    {
        parse(R"(
function a(a, b) return a + b end
some
a(2, 5)
)");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const Luau::ParseErrors& e)
    {
        CHECK_EQ(1, e.getErrors().size());
    }
}

TEST_CASE_FIXTURE(Fixture, "statement_error_recovery_unexpected")
{
    try
    {
        parse(R"(+)");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const Luau::ParseErrors& e)
    {
        CHECK_EQ(1, e.getErrors().size());
    }
}

TEST_CASE_FIXTURE(Fixture, "extra_token_in_consume")
{
    try
    {
        parse(R"(
function test + (a, f) return a + f end
return test(2, 3)
)");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const Luau::ParseErrors& e)
    {
        CHECK_EQ(1, e.getErrors().size());
        CHECK_EQ("Expected '(' when parsing function, got '+'", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "extra_token_in_consume_match")
{
    try
    {
        parse(R"(
function test(a, f+) return a + f end
return test(2, 3)
)");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const Luau::ParseErrors& e)
    {
        CHECK_EQ(1, e.getErrors().size());
        CHECK_EQ("Expected ')' (to close '(' at column 14), got '+'", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "extra_token_in_consume_match_end")
{
    try
    {
        parse(R"(
if true then
    return 12
then
end
)");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const Luau::ParseErrors& e)
    {
        CHECK_EQ(1, e.getErrors().size());
        CHECK_EQ("Expected 'end' (to close 'then' at line 2), got 'then'", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "extra_table_indexer_recovery")
{
    try
    {
        parse(R"(
local a : { [string] : number, [number] : string, count: number }
)");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const Luau::ParseErrors& e)
    {
        CHECK_EQ(1, e.getErrors().size());
    }
}

TEST_CASE_FIXTURE(Fixture, "recovery_error_limit_1")
{
    ScopedFastInt luauParseErrorLimit(FInt::LuauParseErrorLimit, 1);

    try
    {
        parse("local a = ");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const Luau::ParseErrors& e)
    {
        CHECK_EQ(1, e.getErrors().size());
        CHECK_EQ(e.getErrors().front().getMessage(), e.what());
    }
}

TEST_CASE_FIXTURE(Fixture, "recovery_error_limit_2")
{
    ScopedFastInt luauParseErrorLimit(FInt::LuauParseErrorLimit, 2);

    try
    {
        parse("escape escape escape");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const Luau::ParseErrors& e)
    {
        CHECK_EQ(3, e.getErrors().size());
        CHECK_EQ("3 parse errors", std::string(e.what()));
        CHECK_EQ("Reached error limit (2)", e.getErrors().back().getMessage());
    }
}

class CountAstNodes : public AstVisitor
{
public:
    bool visit(AstNode* node) override
    {
        count++;

        return true;
    }

    unsigned count = 0;
};

TEST_CASE_FIXTURE(Fixture, "recovery_of_parenthesized_expressions")
{
    auto checkAstEquivalence = [this](const char* codeWithErrors, const char* code)
    {
        try
        {
            parse(codeWithErrors);
        }
        catch (const Luau::ParseErrors&)
        {
        }

        CountAstNodes counterWithErrors;
        sourceModule->root->visit(&counterWithErrors);

        parse(code);

        CountAstNodes counter;
        sourceModule->root->visit(&counter);

        CHECK_EQ(counterWithErrors.count, counter.count);
    };

    auto checkRecovery = [this, checkAstEquivalence](const char* codeWithErrors, const char* code, unsigned expectedErrorCount)
    {
        try
        {
            parse(codeWithErrors);
            FAIL("Expected ParseErrors to be thrown");
        }
        catch (const Luau::ParseErrors& e)
        {
            CHECK_EQ(expectedErrorCount, e.getErrors().size());
            checkAstEquivalence(codeWithErrors, code);
        }
    };

    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    checkRecovery("function foo(a, b. c) return a + b end", "function foo(a, b) return a + b end", 1);
    checkRecovery(
        "function foo(a, b: { a: number, b: number. c:number }) return a + b end", "function foo(a, b: { a: number, b: number }) return a + b end", 1
    );

    checkRecovery("function foo(a, b): (number -> number return a + b end", "function foo(a, b): (number) -> number return a + b end", 1);
    checkRecovery("function foo(a, b): (number, number -> number return a + b end", "function foo(a, b): (number) -> number return a + b end", 1);
    checkRecovery("function foo(a, b): (number; number) -> number return a + b end", "function foo(a, b): (number) -> number return a + b end", 1);

    checkRecovery("function foo(a, b): (number, number return a + b end", "function foo(a, b): (number, number) end", 1);
    checkRecovery("local function foo(a, b): (number, number return a + b end", "local function foo(a, b): (number, number) end", 1);

    // These tests correctly recovered before the changes and we test that new recovery didn't make them worse
    // (by skipping more tokens necessary)
    checkRecovery("type F = (number, number -> number", "type F = (number, number) -> number", 1);
    checkRecovery("function foo(a, b: { a: number, b: number) return a + b end", "function foo(a, b: { a: number, b: number }) return a + b end", 1);
    checkRecovery("function foo(a, b: { [number: number}) return a + b end", "function foo(a, b: { [number]: number}) return a + b end", 1);
    checkRecovery("local n: (string | number = 2", "local n: (string | number) = 2", 1);

    // Check that we correctly stop at the end of a line
    checkRecovery(
        R"(
function foo(a, b
    return a + b
end
)",
        "function foo(a, b) return a + b end",
        1
    );
}

TEST_CASE_FIXTURE(Fixture, "incomplete_method_call")
{
    const std::string_view source = R"(
        function howdy()
            return game:
        end
    )";

    SourceModule sourceModule;
    ParseResult result = Parser::parse(source.data(), source.size(), *sourceModule.names, *sourceModule.allocator, {});

    REQUIRE_EQ(1, result.root->body.size);

    AstStatFunction* howdyFunction = result.root->body.data[0]->as<AstStatFunction>();
    REQUIRE(howdyFunction != nullptr);

    AstStatBlock* body = howdyFunction->func->body;
    REQUIRE_EQ(1, body->body.size);

    AstStatReturn* ret = body->body.data[0]->as<AstStatReturn>();
    REQUIRE(ret != nullptr);

    REQUIRE_GT(howdyFunction->location.end, body->location.end);
}

TEST_CASE_FIXTURE(Fixture, "incomplete_method_call_2")
{
    const std::string_view source = R"(
        local game = { GetService=function(s) return 'hello' end }

        function a()
            game:a
        end
    )";

    SourceModule sourceModule;
    ParseResult result = Parser::parse(source.data(), source.size(), *sourceModule.names, *sourceModule.allocator, {});

    REQUIRE_EQ(2, result.root->body.size);

    AstStatFunction* howdyFunction = result.root->body.data[1]->as<AstStatFunction>();
    REQUIRE(howdyFunction != nullptr);

    AstStatBlock* body = howdyFunction->func->body;
    REQUIRE_EQ(1, body->body.size);

    AstStatError* ret = body->body.data[0]->as<AstStatError>();
    REQUIRE(ret != nullptr);

    REQUIRE_GT(howdyFunction->location.end, body->location.end);
}

TEST_CASE_FIXTURE(Fixture, "incomplete_method_call_still_yields_an_AstExprIndexName")
{
    ParseResult result = tryParse(R"(
        game:
    )");

    REQUIRE_EQ(1, result.root->body.size);

    AstStatError* stat = result.root->body.data[0]->as<AstStatError>();
    REQUIRE(stat);

    AstExprError* expr = stat->expressions.data[0]->as<AstExprError>();
    REQUIRE(expr);

    AstExprIndexName* indexName = expr->expressions.data[0]->as<AstExprIndexName>();
    REQUIRE(indexName);
}

TEST_CASE_FIXTURE(Fixture, "recover_confusables")
{
    // Binary
    matchParseError("local a = 4 != 10", "Unexpected '!='; did you mean '~='?");
    matchParseError("local a = true && false", "Unexpected '&&'; did you mean 'and'?");
    matchParseError("local a = false || true", "Unexpected '||'; did you mean 'or'?");

    // Unary
    matchParseError("local a = !false", "Unexpected '!'; did you mean 'not'?");

    // Check that separate tokens are not considered as a single one
    matchParseError("local a = 4 ! = 10", "Expected identifier when parsing expression, got '!'");
    matchParseError("local a = true & & false", "Expected identifier when parsing expression, got '&'");
    matchParseError("local a = false | | true", "Expected identifier when parsing expression, got '|'");
}

TEST_CASE_FIXTURE(Fixture, "capture_comments")
{
    ParseOptions options;
    options.captureComments = true;

    ParseResult result = parseEx(
        R"(
        --!strict

        local a = 5 -- comment one
        local b = 8 -- comment two
        --[[
            Multi line comment
        ]]
        local c = 'see'
    )",
        options
    );

    CHECK(result.errors.empty());

    CHECK_EQ(4, result.commentLocations.size());
    CHECK_EQ((Location{{1, 8}, {1, 17}}), result.commentLocations[0].location);
    CHECK_EQ((Location{{3, 20}, {3, 34}}), result.commentLocations[1].location);
    CHECK_EQ((Location{{4, 20}, {4, 34}}), result.commentLocations[2].location);
    CHECK_EQ((Location{{5, 8}, {7, 10}}), result.commentLocations[3].location);
}

TEST_CASE_FIXTURE(Fixture, "capture_broken_comment_at_the_start_of_the_file")
{
    ParseOptions options;
    options.captureComments = true;

    ParseResult result = tryParse(
        R"(
        --[[
    )",
        options
    );

    CHECK_EQ(1, result.commentLocations.size());
    CHECK_EQ((Location{{1, 8}, {2, 4}}), result.commentLocations[0].location);
}

TEST_CASE_FIXTURE(Fixture, "capture_broken_comment")
{
    ParseOptions options;
    options.captureComments = true;

    ParseResult result = tryParse(
        R"(
        local a = "test"

        --[[broken!
    )",
        options
    );

    CHECK_EQ(1, result.commentLocations.size());
    CHECK_EQ((Location{{3, 8}, {4, 4}}), result.commentLocations[0].location);
}

TEST_CASE_FIXTURE(Fixture, "empty_function_type_error_recovery")
{
    try
    {
        parse(R"(
type Fn = (
    any,
    string | number | ()
) -> any
)");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const Luau::ParseErrors& e)
    {
        CHECK_EQ("Expected '->' after '()' when parsing function type; did you mean 'nil'?", e.getErrors().front().getMessage());
    }

    // If we have arguments or generics, don't use special case
    try
    {
        parse(R"(type Fn = (any, string | number | (number, number)) -> any)");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const Luau::ParseErrors& e)
    {
        CHECK_EQ("Expected '->' when parsing function type, got ')'", e.getErrors().front().getMessage());
    }

    try
    {
        parse(R"(type Fn = (any, string | number | <a>()) -> any)");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const Luau::ParseErrors& e)
    {
        CHECK_EQ("Expected '->' when parsing function type, got ')'", e.getErrors().front().getMessage());
    }

    try
    {
        parse(R"(type Fn = (any, string | number | <a...>()) -> any)");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const Luau::ParseErrors& e)
    {
        CHECK_EQ("Expected '->' when parsing function type, got ')'", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "AstName_comparison")
{
    CHECK(!(AstName() < AstName()));

    AstName one{"one"};
    AstName two{"two"};

    CHECK_NE((one < two), (two < one));
}

TEST_CASE_FIXTURE(Fixture, "generic_type_list_recovery")
{
    try
    {
        parse(R"(
local function foo<T..., U>(a: U, ...: T...): (U, ...T) return a, ... end
return foo(1, 2 -- to check for a second error after recovery
)");
        FAIL("Expected ParseErrors to be thrown");
    }
    catch (const Luau::ParseErrors& e)
    {
        CHECK_EQ(2, e.getErrors().size());
        CHECK_EQ("Generic types come before generic type packs", e.getErrors().front().getMessage());
    }
}

TEST_CASE_FIXTURE(Fixture, "recover_index_name_keyword")
{
    ParseResult result = tryParse(R"(
local b
local a = b.do
    )");
    CHECK_EQ(1, result.errors.size());

    result = tryParse(R"(
local b
local a = b.
do end
    )");
    CHECK_EQ(1, result.errors.size());
}

TEST_CASE_FIXTURE(Fixture, "recover_self_call_keyword")
{
    ParseResult result = tryParse(R"(
local b
local a = b:do
    )");
    CHECK_EQ(2, result.errors.size());

    result = tryParse(R"(
local b
local a = b:
do end
    )");
    CHECK_EQ(2, result.errors.size());
}

TEST_CASE_FIXTURE(Fixture, "recover_type_index_name_keyword")
{
    ParseResult result = tryParse(R"(
local A
local b : A.do
    )");
    CHECK_EQ(1, result.errors.size());

    result = tryParse(R"(
local A
local b : A.do
do end
    )");
    CHECK_EQ(1, result.errors.size());
}

TEST_CASE_FIXTURE(Fixture, "recover_expected_type_pack")
{
    ParseResult result = tryParse(R"(
type Y<T..., U = T...> = (T...) -> U...
    )");
    CHECK_EQ(1, result.errors.size());
}

TEST_CASE_FIXTURE(Fixture, "recover_unexpected_type_pack")
{
    ParseResult result = tryParse(R"(
type X<T...> = { a: T..., b: number }
type Y<T> = { a: T..., b: number }
type Z<T> = { a: string | T..., b: number }
    )");
    REQUIRE_EQ(3, result.errors.size());
}

TEST_CASE_FIXTURE(Fixture, "recover_function_return_type_annotations")
{
    ParseResult result = tryParse(R"(
type Custom<A, B, C> = { x: A, y: B, z: C }
type Packed<A...> = { x: (A...) -> () }
type F = (number): Custom<boolean, number, string>
type G = Packed<(number): (string, number, boolean)>
local function f(x: number) -> Custom<string, boolean, number>
end
    )");
    REQUIRE_EQ(3, result.errors.size());
    CHECK_EQ(result.errors[0].getMessage(), "Return types in function type annotations are written after '->' instead of ':'");
    CHECK_EQ(result.errors[1].getMessage(), "Return types in function type annotations are written after '->' instead of ':'");
    CHECK_EQ(result.errors[2].getMessage(), "Function return type annotations are written after ':' instead of '->'");
}

TEST_CASE_FIXTURE(Fixture, "error_message_for_using_function_as_type_annotation")
{
    ParseResult result = tryParse(R"(
        type Foo = function
    )");
    REQUIRE_EQ(1, result.errors.size());
    CHECK_EQ(
        "Using 'function' as a type annotation is not supported, consider replacing with a function type annotation e.g. '(...any) -> ...any'",
        result.errors[0].getMessage()
    );
}

TEST_CASE_FIXTURE(Fixture, "get_a_nice_error_when_there_is_an_extra_comma_at_the_end_of_a_function_argument_list")
{
    ParseResult result = tryParse(R"(
        foo(a, b, c,)
    )");

    REQUIRE(1 == result.errors.size());

    CHECK(Location({1, 20}, {1, 21}) == result.errors[0].getLocation());
    CHECK("Expected expression after ',' but got ')' instead" == result.errors[0].getMessage());
}

TEST_CASE_FIXTURE(Fixture, "get_a_nice_error_when_there_is_an_extra_comma_at_the_end_of_a_function_parameter_list")
{
    ParseResult result = tryParse(R"(
        export type VisitFn = (
            any,
            Array<TAnyNode | Array<TAnyNode>>, -- extra comma here
        ) -> any
    )");

    REQUIRE(1 == result.errors.size());

    CHECK(Location({4, 8}, {4, 9}) == result.errors[0].getLocation());
    CHECK("Expected type after ',' but got ')' instead" == result.errors[0].getMessage());
}

TEST_CASE_FIXTURE(Fixture, "get_a_nice_error_when_there_is_an_extra_comma_at_the_end_of_a_generic_parameter_list")
{
    ParseResult result = tryParse(R"(
        export type VisitFn = <A, B,>(a: A, b: B) -> ()
    )");

    REQUIRE(1 == result.errors.size());

    CHECK(Location({1, 36}, {1, 37}) == result.errors[0].getLocation());
    CHECK("Expected type after ',' but got '>' instead" == result.errors[0].getMessage());

    REQUIRE(1 == result.root->body.size);

    AstStatTypeAlias* t = result.root->body.data[0]->as<AstStatTypeAlias>();
    REQUIRE(t != nullptr);

    AstTypeFunction* f = t->type->as<AstTypeFunction>();
    REQUIRE(f != nullptr);

    CHECK(2 == f->generics.size);
}

TEST_CASE_FIXTURE(Fixture, "get_a_nice_error_when_there_is_no_comma_between_table_members")
{
    ParseResult result = tryParse(R"(
        local t = {
            first = 1
            second = 2,
            third = 3,
            fouth = 4,
        }
    )");

    REQUIRE(1 == result.errors.size());

    CHECK(Location({3, 12}, {3, 18}) == result.errors[0].getLocation());
    CHECK("Expected ',' after table constructor element" == result.errors[0].getMessage());

    REQUIRE(1 == result.root->body.size);

    AstExprTable* table = Luau::query<AstExprTable>(result.root);
    REQUIRE(table);
    CHECK(table->items.size == 4);
}

TEST_CASE_FIXTURE(Fixture, "get_a_nice_error_when_there_is_no_comma_after_last_table_member")
{
    ParseResult result = tryParse(R"(
        local t = {
            first = 1

        local ok = true
        local good = ok == true
    )");

    REQUIRE(1 == result.errors.size());

    CHECK(Location({4, 8}, {4, 13}) == result.errors[0].getLocation());
    CHECK("Expected '}' (to close '{' at line 2), got 'local'" == result.errors[0].getMessage());

    REQUIRE(3 == result.root->body.size);

    AstExprTable* table = Luau::query<AstExprTable>(result.root);
    REQUIRE(table);
    CHECK(table->items.size == 1);
}

TEST_CASE_FIXTURE(Fixture, "missing_default_type_pack_argument_after_variadic_type_parameter")
{
    ParseResult result = tryParse(R"(
        type Foo<T... = > = nil
    )");

    REQUIRE_EQ(2, result.errors.size());

    CHECK_EQ(Location{{1, 23}, {1, 25}}, result.errors[0].getLocation());
    CHECK_EQ("Expected type, got '>'", result.errors[0].getMessage());

    CHECK_EQ(Location{{1, 23}, {1, 24}}, result.errors[1].getLocation());
    CHECK_EQ("Expected type pack after '=', got type", result.errors[1].getMessage());
}

TEST_CASE_FIXTURE(Fixture, "table_type_keys_cant_contain_nul")
{
    ParseResult result = tryParse(R"(
        type Foo = { ["\0"]: number }
    )");

    REQUIRE_EQ(1, result.errors.size());

    CHECK_EQ(Location{{1, 21}, {1, 22}}, result.errors[0].getLocation());
    CHECK_EQ("String literal contains malformed escape sequence or \\0", result.errors[0].getMessage());
}

TEST_CASE_FIXTURE(Fixture, "invalid_escape_literals_get_reported_but_parsing_continues")
{
    ParseResult result = tryParse(R"(
        local foo = "\xQQ"
        print(foo)
    )");

    REQUIRE_EQ(1, result.errors.size());

    CHECK_EQ(Location{{1, 20}, {1, 26}}, result.errors[0].getLocation());
    CHECK_EQ("String literal contains malformed escape sequence", result.errors[0].getMessage());

    REQUIRE(result.root);
    CHECK_EQ(result.root->body.size, 2);
}

TEST_CASE_FIXTURE(Fixture, "unfinished_string_literals_get_reported_but_parsing_continues")
{
    ParseResult result = tryParse(R"(
        local foo = "hi
        print(foo)
    )");

    REQUIRE_EQ(1, result.errors.size());

    CHECK_EQ(Location{{1, 20}, {1, 23}}, result.errors[0].getLocation());
    CHECK_EQ("Malformed string; did you forget to finish it?", result.errors[0].getMessage());

    REQUIRE(result.root);
    CHECK_EQ(result.root->body.size, 2);
}

TEST_CASE_FIXTURE(Fixture, "unfinished_string_literal_types_get_reported_but_parsing_continues")
{
    ParseResult result = tryParse(R"(
        type Foo = "hi
        print(foo)
    )");

    REQUIRE_EQ(1, result.errors.size());

    CHECK_EQ(Location{{1, 19}, {1, 22}}, result.errors[0].getLocation());
    CHECK_EQ("Malformed string; did you forget to finish it?", result.errors[0].getMessage());

    REQUIRE(result.root);
    CHECK_EQ(result.root->body.size, 2);
}

TEST_CASE_FIXTURE(Fixture, "do_block_with_no_end")
{
    ParseResult result = tryParse(R"(
        do
    )");

    REQUIRE_EQ(1, result.errors.size());

    AstStatBlock* stat0 = result.root->body.data[0]->as<AstStatBlock>();
    REQUIRE(stat0);

    CHECK(!stat0->hasEnd);
}

TEST_CASE_FIXTURE(Fixture, "parse_interpolated_string_with_lookahead_involved")
{
    ParseResult result = tryParse(R"(
        local x = `{ {y} }`
    )");

    REQUIRE_MESSAGE(result.errors.empty(), result.errors[0].getMessage());
}

TEST_CASE_FIXTURE(Fixture, "parse_interpolated_string_with_lookahead_involved2")
{
    ParseResult result = tryParse(R"(
        local x = `{ { y{} } }`
    )");

    REQUIRE_MESSAGE(result.errors.empty(), result.errors[0].getMessage());
}

TEST_CASE_FIXTURE(Fixture, "parse_top_level_checked_fn")
{
    ParseOptions opts;
    opts.allowDeclarationSyntax = true;

    std::string src = R"BUILTIN_SRC(
@checked declare function abs(n: number): number
)BUILTIN_SRC";

    ParseResult pr = tryParse(src, opts);
    LUAU_ASSERT(pr.errors.size() == 0);

    LUAU_ASSERT(pr.root->body.size == 1);
    AstStat* root = *(pr.root->body.data);
    auto func = root->as<AstStatDeclareFunction>();
    LUAU_ASSERT(func);
    LUAU_ASSERT(func->isCheckedFunction());
}

TEST_CASE_FIXTURE(Fixture, "parse_declared_table_checked_member")
{
    ParseOptions opts;
    opts.allowDeclarationSyntax = true;

    const std::string src = R"BUILTIN_SRC(
    declare math : {
        abs : @checked (number) -> number
}
)BUILTIN_SRC";

    ParseResult pr = tryParse(src, opts);
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
    LUAU_ASSERT(func->isCheckedFunction());
}

TEST_CASE_FIXTURE(Fixture, "parse_checked_outside_decl_fails")
{
    ParseOptions opts;
    opts.allowDeclarationSyntax = true;

    ParseResult pr = tryParse(
        R"(
    local @checked = 3
)",
        opts
    );
    LUAU_ASSERT(pr.errors.size() > 0);
    auto ts = pr.errors[1].getMessage();
}

TEST_CASE_FIXTURE(Fixture, "parse_checked_in_and_out_of_decl_fails")
{
    ParseOptions opts;
    opts.allowDeclarationSyntax = true;

    auto pr = tryParse(
        R"(
    local @checked = 3
    @checked declare function abs(n: number): number
)",
        opts
    );
    LUAU_ASSERT(pr.errors.size() == 2);
    LUAU_ASSERT(pr.errors[0].getLocation().begin.line == 1);
    LUAU_ASSERT(pr.errors[1].getLocation().begin.line == 1);
}

TEST_CASE_FIXTURE(Fixture, "parse_checked_as_function_name_fails")
{
    ParseOptions opts;
    opts.allowDeclarationSyntax = true;

    auto pr = tryParse(
        R"(
    @checked function(x: number) : number
    end
)",
        opts
    );
    LUAU_ASSERT(pr.errors.size() > 0);
}

TEST_CASE_FIXTURE(Fixture, "cannot_use_@_as_variable_name")
{
    ParseOptions opts;
    opts.allowDeclarationSyntax = true;

    auto pr = tryParse(
        R"(
    local @blah = 3
)",
        opts
    );

    LUAU_ASSERT(pr.errors.size() > 0);
}

TEST_CASE_FIXTURE(Fixture, "read_write_table_properties")
{
    auto pr = tryParse(R"(
        type A = {read x: number}
        type B = {write x: number}
        type C = {read x: number, write x: number}
        type D = {read: () -> string}
        type E = {write: (string) -> ()}
        type F = {read read: () -> string}
        type G = {read write: (string) -> ()}

        type H = {read ["A"]: number}
        type I = {write ["A"]: string}

        type J = {read [number]: number}
        type K = {write [number]: string}
    )");

    LUAU_ASSERT(pr.errors.size() == 0);
}

void checkAttribute(const AstAttr* attr, const AstAttr::Type type, const Location& location)
{
    CHECK_EQ(attr->type, type);
    CHECK_EQ(attr->location, location);
}

void checkFirstErrorForAttributes(const std::vector<ParseError>& errors, const size_t minSize, const Location& location, const std::string& message)
{
    LUAU_ASSERT(minSize >= 1);

    CHECK_GE(errors.size(), minSize);
    CHECK_EQ(errors[0].getLocation(), location);
    CHECK_EQ(errors[0].getMessage(), message);
}

TEST_CASE_FIXTURE(Fixture, "parse_attribute_on_function_stat")
{

    AstStatBlock* stat = parse(R"(
@checked
function hello(x, y)
    return x + y
end)");

    LUAU_ASSERT(stat != nullptr);

    AstStatFunction* statFun = stat->body.data[0]->as<AstStatFunction>();
    LUAU_ASSERT(statFun != nullptr);

    AstArray<AstAttr*> attributes = statFun->func->attributes;

    CHECK_EQ(attributes.size, 1);

    checkAttribute(attributes.data[0], AstAttr::Type::Checked, Location(Position(1, 0), Position(1, 8)));
}

TEST_CASE_FIXTURE(Fixture, "parse_parametrized_attribute_on_function_stat")
{
    ScopedFastFlag sff{FFlag::LuauParametrizedAttributeSyntax, true};

    AstStatBlock* stat = parse(R"(
@[deprecated{ use = "greetng", reason = "Using <hello> is too causal"}]
function hello(x, y)
    return x + y
end)");

    LUAU_ASSERT(stat != nullptr);

    AstStatFunction* statFun = stat->body.data[0]->as<AstStatFunction>();
    LUAU_ASSERT(statFun != nullptr);

    AstArray<AstAttr*> attributes = statFun->func->attributes;

    CHECK_EQ(attributes.size, 1);

    checkAttribute(attributes.data[0], AstAttr::Type::Deprecated, Location(Position(1, 2), Position(1, 70)));
}

TEST_CASE_FIXTURE(Fixture, "non_literal_attribute_arguments_is_not_allowed")
{
    ScopedFastFlag sff{FFlag::LuauParametrizedAttributeSyntax, true};
    ParseResult result = tryParse(R"(
@[deprecated{ reason = reasonString }]
function hello(x, y)
    return x + y
end)");

    checkFirstErrorForAttributes(
        result.errors, 1, Location(Position(1, 13), Position(1, 37)), "Only literals can be passed as arguments for attributes"
    );
}

TEST_CASE_FIXTURE(Fixture, "unknown_arguments_for_depricated_is_not_allowed")
{
    ScopedFastFlag sff{FFlag::LuauParametrizedAttributeSyntax, true};
    ParseResult result = tryParse(R"(
@[deprecated({}, "Very deprecated")]
function hello(x, y)
    return x + y
end)");

    checkFirstErrorForAttributes(result.errors, 1, Location(Position(1, 2), Position(1, 12)), "@deprecated can be parametrized only by 1 argument");

    result = tryParse(R"(
@[deprecated "Very deprecated"]
function hello(x, y)
    return x + y
end)");

    checkFirstErrorForAttributes(result.errors, 1, Location(Position(1, 13), Position(1, 30)), "Unknown argument type for @deprecated");

    result = tryParse(R"(
@[deprecated{ foo = "bar" }]
function hello(x, y)
    return x + y
end)");

    checkFirstErrorForAttributes(
        result.errors,
        1,
        Location(Position(1, 14), Position(1, 17)),
        "Unknown argument 'foo' for @deprecated. Only string constants for 'use' and 'reason' are allowed"
    );

    result = tryParse(R"(
@[deprecated{ use = 5 }]
function hello(x, y)
    return x + y
end)");

    checkFirstErrorForAttributes(result.errors, 1, Location(Position(1, 20), Position(1, 21)), "Only constant string allowed as value for 'use'");
}

TEST_CASE_FIXTURE(Fixture, "do_not_hang_on_incomplete_attribute_list")
{
    ScopedFastFlag sff{FFlag::LuauParametrizedAttributeSyntax, true};
    ParseResult result = tryParse(R"(
@[]
function hello(x, y)
    return x + y
end)");
    checkFirstErrorForAttributes(result.errors, 1, Location(Position(1, 0), Position(1, 3)), "Attribute list cannot be empty");

    result = tryParse(R"(@[)");

    checkFirstErrorForAttributes(
        result.errors, 1, Location(Position(0, 2), Position(0, 2)), "Expected identifier when parsing attribute name, got <eof>"
    );

    result = tryParse(R"(@[
        function foo() end
    )");

    checkFirstErrorForAttributes(
        result.errors, 1, Location(Position(1, 8), Position(1, 16)), "Expected identifier when parsing attribute name, got 'function'"
    );

    result = tryParse(R"(@[deprecated
        local function foo() end
    )");

    checkFirstErrorForAttributes(result.errors, 1, Location(Position(1, 8), Position(1, 13)), "Expected ']' (to close '@[' at line 1), got 'local'");
}

TEST_CASE_FIXTURE(Fixture, "parse_attribute_for_function_expression")
{
    AstStatBlock* stat1 = parse(R"(
local function invoker(f)
    return f(1)
end

invoker(@checked function(x) return (x + 2) end)
)");

    LUAU_ASSERT(stat1 != nullptr);

    AstExprFunction* func1 = stat1->body.data[1]->as<AstStatExpr>()->expr->as<AstExprCall>()->args.data[0]->as<AstExprFunction>();
    LUAU_ASSERT(func1 != nullptr);

    AstArray<AstAttr*> attributes1 = func1->attributes;

    CHECK_EQ(attributes1.size, 1);

    checkAttribute(attributes1.data[0], AstAttr::Type::Checked, Location(Position(5, 8), Position(5, 16)));

    AstStatBlock* stat2 = parse(R"(
local f = @checked function(x) return (x + 2) end
)");

    LUAU_ASSERT(stat2 != nullptr);

    AstExprFunction* func2 = stat2->body.data[0]->as<AstStatLocal>()->values.data[0]->as<AstExprFunction>();
    LUAU_ASSERT(func2 != nullptr);

    AstArray<AstAttr*> attributes2 = func2->attributes;

    CHECK_EQ(attributes2.size, 1);

    checkAttribute(attributes2.data[0], AstAttr::Type::Checked, Location(Position(1, 10), Position(1, 18)));
}

TEST_CASE_FIXTURE(Fixture, "parse_attribute_on_local_function_stat")
{
    AstStatBlock* stat = parse(R"(
    @checked
local function hello(x, y)
    return x + y
end)");

    LUAU_ASSERT(stat != nullptr);

    AstStatLocalFunction* statFun = stat->body.data[0]->as<AstStatLocalFunction>();
    LUAU_ASSERT(statFun != nullptr);

    AstArray<AstAttr*> attributes = statFun->func->attributes;

    CHECK_EQ(attributes.size, 1);

    checkAttribute(attributes.data[0], AstAttr::Type::Checked, Location(Position(1, 4), Position(1, 12)));
}

TEST_CASE_FIXTURE(Fixture, "empty_attribute_name_is_not_allowed")
{
    ParseResult result = tryParse(R"(
@
function hello(x, y)
    return x + y
end)");

    checkFirstErrorForAttributes(result.errors, 1, Location(Position(1, 0), Position(1, 1)), "Attribute name is missing");
}

TEST_CASE_FIXTURE(Fixture, "dont_parse_attributes_on_non_function_stat")
{
    ParseResult pr1 = tryParse(R"(
@checked
if a<0 then a = 0 end)");
    checkFirstErrorForAttributes(
        pr1.errors,
        1,
        Location(Position(2, 0), Position(2, 2)),
        "Expected 'function', 'local function', 'declare function' or a function type declaration after attribute, but got 'if' instead"
    );

    ParseResult pr2 = tryParse(R"(
local i = 1
@checked
while a[i] do
    print(a[i])
    i = i + 1
end)");
    checkFirstErrorForAttributes(
        pr2.errors,
        1,
        Location(Position(3, 0), Position(3, 5)),
        "Expected 'function', 'local function', 'declare function' or a function type declaration after attribute, but got 'while' instead"
    );

    ParseResult pr3 = tryParse(R"(
@checked
do
    local a2 = 2*a
    local d = sqrt(b^2 - 4*a*c)
    x1 = (-b + d)/a2
    x2 = (-b - d)/a2
end)");
    checkFirstErrorForAttributes(
        pr3.errors,
        1,
        Location(Position(2, 0), Position(2, 2)),
        "Expected 'function', 'local function', 'declare function' or a function type declaration after attribute, but got 'do' instead"
    );

    ParseResult pr4 = tryParse(R"(
@checked
for i=1,10 do print(i) end
)");
    checkFirstErrorForAttributes(
        pr4.errors,
        1,
        Location(Position(2, 0), Position(2, 3)),
        "Expected 'function', 'local function', 'declare function' or a function type declaration after attribute, but got 'for' instead"
    );

    ParseResult pr5 = tryParse(R"(
@checked
repeat
    line = io.read()
until line ~= ""
)");
    checkFirstErrorForAttributes(
        pr5.errors,
        1,
        Location(Position(2, 0), Position(2, 6)),
        "Expected 'function', 'local function', 'declare function' or a function type declaration after attribute, but got 'repeat' instead"
    );


    ParseResult pr6 = tryParse(R"(
@checked
local x = 10
)");
    checkFirstErrorForAttributes(
        pr6.errors, 1, Location(Position(2, 6), Position(2, 7)), "Expected 'function' after local declaration with attribute, but got 'x' instead"
    );

    ParseResult pr7 = tryParse(R"(
local i = 1
while a[i] do
    if a[i] == v then @checked break end
    i = i + 1
end
)");
    checkFirstErrorForAttributes(
        pr7.errors,
        1,
        Location(Position(3, 31), Position(3, 36)),
        "Expected 'function', 'local function', 'declare function' or a function type declaration after attribute, but got 'break' instead"
    );


    ParseResult pr8 = tryParse(R"(
function foo1 () @checked return 'a' end
)");
    checkFirstErrorForAttributes(
        pr8.errors,
        1,
        Location(Position(1, 26), Position(1, 32)),
        "Expected 'function', 'local function', 'declare function' or a function type declaration after attribute, but got 'return' instead"
    );
}

TEST_CASE_FIXTURE(Fixture, "dont_parse_attribute_on_argument_non_function")
{
    ParseResult pr = tryParse(R"(
local function invoker(f, y)
    return f(y)
end

invoker(function(x) return (x + 2) end, @checked 1)
)");

    checkFirstErrorForAttributes(
        pr.errors, 1, Location(Position(5, 40), Position(5, 48)), "Expected 'function' declaration after attribute, but got '1' instead"
    );
}

TEST_CASE_FIXTURE(Fixture, "parse_attribute_on_function_type_declaration")
{
    ParseOptions opts;
    opts.allowDeclarationSyntax = true;

    std::string src = R"(
@checked declare function abs(n: number): number
)";

    ParseResult pr = tryParse(src, opts);
    CHECK_EQ(pr.errors.size(), 0);

    LUAU_ASSERT(pr.root->body.size == 1);

    AstStat* root = *(pr.root->body.data);

    auto func = root->as<AstStatDeclareFunction>();
    LUAU_ASSERT(func != nullptr);

    CHECK(func->isCheckedFunction());

    AstArray<AstAttr*> attributes = func->attributes;

    checkAttribute(attributes.data[0], AstAttr::Type::Checked, Location(Position(1, 0), Position(1, 8)));
}

TEST_CASE_FIXTURE(Fixture, "parse_attributes_on_function_type_declaration_in_table")
{
    ParseOptions opts;
    opts.allowDeclarationSyntax = true;

    std::string src = R"(
declare bit32: {
    band: @checked (...number) -> number
})";

    ParseResult pr = tryParse(src, opts);
    CHECK_EQ(pr.errors.size(), 0);

    LUAU_ASSERT(pr.root->body.size == 1);

    AstStat* root = *(pr.root->body.data);

    AstStatDeclareGlobal* glob = root->as<AstStatDeclareGlobal>();
    LUAU_ASSERT(glob);

    auto tbl = glob->type->as<AstTypeTable>();
    LUAU_ASSERT(tbl);

    LUAU_ASSERT(tbl->props.size == 1);
    AstTableProp prop = tbl->props.data[0];

    AstTypeFunction* func = prop.type->as<AstTypeFunction>();
    LUAU_ASSERT(func);

    AstArray<AstAttr*> attributes = func->attributes;

    CHECK_EQ(attributes.size, 1);
    checkAttribute(attributes.data[0], AstAttr::Type::Checked, Location(Position(2, 10), Position(2, 18)));
}

TEST_CASE_FIXTURE(Fixture, "dont_parse_attributes_on_non_function_type_declarations")
{
    ParseOptions opts;
    opts.allowDeclarationSyntax = true;

    ParseResult pr1 = tryParse(
        R"(
@checked declare foo: number
    )",
        opts
    );

    checkFirstErrorForAttributes(
        pr1.errors, 1, Location(Position(1, 17), Position(1, 20)), "Expected a function type declaration after attribute, but got 'foo' instead"
    );

    ParseResult pr2 = tryParse(
        R"(
@checked declare class Foo
    prop: number
    function method(self, foo: number): string
end)",
        opts
    );

    checkFirstErrorForAttributes(
        pr2.errors, 1, Location(Position(1, 17), Position(1, 22)), "Expected a function type declaration after attribute, but got 'class' instead"
    );

    ParseResult pr3 = tryParse(
        R"(
declare bit32: {
    band: @checked number
})",
        opts
    );

    checkFirstErrorForAttributes(
        pr3.errors, 1, Location(Position(2, 19), Position(2, 25)), "Expected '(' when parsing function parameters, got 'number'"
    );
}

TEST_CASE_FIXTURE(Fixture, "attributes_cannot_be_duplicated")
{
    ParseResult result = tryParse(R"(
@checked
    @checked
function hello(x, y)
    return x + y
end)");

    checkFirstErrorForAttributes(result.errors, 1, Location(Position(2, 4), Position(2, 12)), "Cannot duplicate attribute '@checked'");
}

TEST_CASE_FIXTURE(Fixture, "unsupported_attributes_are_not_allowed")
{
    ParseResult result = tryParse(R"(
@checked
    @cool_attribute
function hello(x, y)
    return x + y
end)");

    checkFirstErrorForAttributes(result.errors, 1, Location(Position(2, 4), Position(2, 19)), "Invalid attribute '@cool_attribute'");
}

TEST_CASE_FIXTURE(Fixture, "can_parse_leading_bar_unions_successfully")
{
    parse(R"(type A = | "Hello" | "World")");
}

TEST_CASE_FIXTURE(Fixture, "can_parse_leading_ampersand_intersections_successfully")
{
    parse(R"(type A = & { string } & { number })");
}

TEST_CASE_FIXTURE(Fixture, "mixed_leading_intersection_and_union_not_allowed")
{
    matchParseError("type A = & number | string | boolean", "Mixing union and intersection types is not allowed; consider wrapping in parentheses.");
    matchParseError("type A = | number & string & boolean", "Mixing union and intersection types is not allowed; consider wrapping in parentheses.");
}

TEST_CASE_FIXTURE(Fixture, "grouped_function_type")
{
    const auto root = parse(R"(
        type X<T> = T
        local x: X<(() -> ())?>
    )");
    LUAU_ASSERT(root);
    CHECK_EQ(root->body.size, 2);
    auto assignment = root->body.data[1]->as<AstStatLocal>();
    LUAU_ASSERT(assignment);
    CHECK_EQ(assignment->vars.size, 1);
    CHECK_EQ(assignment->values.size, 0);
    auto binding = assignment->vars.data[0];
    CHECK_EQ(binding->name, "x");
    auto genericTy = binding->annotation->as<AstTypeReference>();
    LUAU_ASSERT(genericTy);
    CHECK_EQ(genericTy->parameters.size, 1);
    auto paramTy = genericTy->parameters.data[0];
    LUAU_ASSERT(paramTy.type);
    auto unionTy = paramTy.type->as<AstTypeUnion>();
    LUAU_ASSERT(unionTy);
    CHECK_EQ(unionTy->types.size, 2);
    auto groupTy = unionTy->types.data[0]->as<AstTypeGroup>(); // (() -> ())
    REQUIRE(groupTy);
    CHECK(groupTy->type->is<AstTypeFunction>());          // () -> ()
    CHECK(unionTy->types.data[1]->is<AstTypeOptional>()); // ?
}

TEST_CASE_FIXTURE(Fixture, "complex_union_in_generic_ty")
{
    const auto root = parse(R"(
        type X<T> = T
        local x: X<
            | number
            | boolean
            | string
        >
    )");
    LUAU_ASSERT(root);
    CHECK_EQ(root->body.size, 2);
    auto assignment = root->body.data[1]->as<AstStatLocal>();
    LUAU_ASSERT(assignment);
    CHECK_EQ(assignment->vars.size, 1);
    CHECK_EQ(assignment->values.size, 0);
    auto binding = assignment->vars.data[0];
    CHECK_EQ(binding->name, "x");
    auto genericTy = binding->annotation->as<AstTypeReference>();
    LUAU_ASSERT(genericTy);
    CHECK_EQ(genericTy->parameters.size, 1);
    auto paramTy = genericTy->parameters.data[0];
    LUAU_ASSERT(paramTy.type);
    auto unionTy = paramTy.type->as<AstTypeUnion>();
    LUAU_ASSERT(unionTy);
    CHECK_EQ(unionTy->types.size, 3);
    // NOTE: These are `const char*` so we can compare them to `AstName`s later.
    std::vector<const char*> expectedTypes{"number", "boolean", "string"};
    for (size_t i = 0; i < expectedTypes.size(); i++)
    {
        auto ty = unionTy->types.data[i]->as<AstTypeReference>();
        LUAU_ASSERT(ty);
        CHECK_EQ(ty->name, expectedTypes[i]);
    }
}

TEST_CASE_FIXTURE(Fixture, "recover_from_bad_table_type")
{
    ParseOptions opts;
    opts.allowDeclarationSyntax = true;
    const auto result = tryParse(
        R"(
        declare class Widget
            state: {string: function(string, Widget)}
        end
    )",
        opts
    );
    CHECK_EQ(result.errors.size(), 2);
}

TEST_CASE_FIXTURE(Fixture, "function_name_has_correct_start_location")
{
    AstStatBlock* block = parse(R"(
        function simple()
        end

        function T:complex()
        end
    )");

    REQUIRE_EQ(2, block->body.size);

    const auto function1 = block->body.data[0]->as<AstStatFunction>();
    LUAU_ASSERT(function1);
    CHECK_EQ(Position{1, 17}, function1->name->location.begin);

    const auto function2 = block->body.data[1]->as<AstStatFunction>();
    LUAU_ASSERT(function2);
    CHECK_EQ(Position{4, 17}, function2->name->location.begin);
}

TEST_CASE_FIXTURE(Fixture, "stat_end_includes_semicolon_position")
{
    AstStatBlock* block = parse(R"(
        local x = 1
        local y = 2;
        local z = 3  ;
    )");

    REQUIRE_EQ(3, block->body.size);

    const auto stat1 = block->body.data[0];
    LUAU_ASSERT(stat1);
    CHECK_FALSE(stat1->hasSemicolon);
    CHECK_EQ(Position{1, 19}, stat1->location.end);

    const auto stat2 = block->body.data[1];
    LUAU_ASSERT(stat2);
    CHECK(stat2->hasSemicolon);
    CHECK_EQ(Position{2, 20}, stat2->location.end);

    const auto stat3 = block->body.data[2];
    LUAU_ASSERT(stat3);
    CHECK(stat3->hasSemicolon);
    CHECK_EQ(Position{3, 22}, stat3->location.end);
}

TEST_CASE_FIXTURE(Fixture, "parsing_type_suffix_for_return_type_with_variadic")
{
    ScopedFastFlag sff{DFFlag::DebugLuauReportReturnTypeVariadicWithTypeSuffix, true};

    ParseResult result = tryParse(R"(
        function foo(): (string, ...number) | boolean
        end
    )");

    // TODO(CLI-140667): this should produce a ParseError in future when we fix the invalid syntax
    CHECK(result.errors.size() == 0);
    CHECK_EQ(luau_telemetry_parsed_return_type_variadic_with_type_suffix, true);
}

TEST_CASE_FIXTURE(Fixture, "parsing_string_union_indexers")
{
    parse(R"(type foo = { ["bar" | "baz"]: number })");
}

TEST_CASE_FIXTURE(Fixture, "parsing_incomplete_string_interpolation_missing_curly_at_eof")
{
    ScopedFastFlag _{FFlag::LuauParseIncompleteInterpStringsWithLocation, true};
    auto parseResult = tryParse(R"(print(`{e.x} {e.a)");
    const auto first = parseResult.root->body.data[0];
    auto expr = first->as<AstStatExpr>();
    CHECK(expr != nullptr);
    auto call = expr->expr->as<AstExprCall>();
    CHECK(call != nullptr);
    auto interpString = call->args.data[0]->as<AstExprInterpString>();
    CHECK(interpString != nullptr);
    CHECK(interpString->expressions.size == 2);
    CHECK(interpString->location.begin == Position(0, 6));
    CHECK(interpString->location.end == Position(0, 17));
    CHECK_EQ(parseResult.errors.size(), 2);

    auto err = parseResult.errors[0];
    CHECK_EQ(err.getMessage(), "Malformed interpolated string; did you forget to add a '}'?");
    CHECK_EQ(err.getLocation(), Location({0, 16}, {0, 17}));
}

TEST_CASE_FIXTURE(Fixture, "parsing_incomplete_string_interpolation_missing_backtick_at_eof")
{
    ScopedFastFlag _{FFlag::LuauParseIncompleteInterpStringsWithLocation, true};
    auto parseResult = tryParse(R"(print(`{e.x} {e.a})");
    const auto first = parseResult.root->body.data[0];
    auto expr = first->as<AstStatExpr>();
    CHECK(expr != nullptr);
    auto call = expr->expr->as<AstExprCall>();
    CHECK(call != nullptr);
    auto interpString = call->args.data[0]->as<AstExprInterpString>();
    CHECK(interpString != nullptr);
    CHECK(interpString->expressions.size == 2);
    CHECK(interpString->location.begin == Position(0, 6));
    CHECK(interpString->location.end == Position(0, 18));
    CHECK_EQ(parseResult.errors.size(), 2);

    auto err = parseResult.errors[0];
    CHECK_EQ(err.getMessage(), "Malformed interpolated string; did you forget to add a '`'?");
    CHECK_EQ(err.getLocation(), Location({0, 17}, {0, 18}));
}

TEST_CASE_FIXTURE(Fixture, "parsing_incomplete_string_interpolation_missing_curly_with_backtick_at_eof")
{
    ScopedFastFlag _{FFlag::LuauParseIncompleteInterpStringsWithLocation, true};
    auto parseResult = tryParse(R"(print(`{e.x} {e.a`)");
    const auto first = parseResult.root->body.data[0];
    auto expr = first->as<AstStatExpr>();
    CHECK(expr != nullptr);
    auto call = expr->expr->as<AstExprCall>();
    CHECK(call != nullptr);
    auto interpString = call->args.data[0]->as<AstExprInterpString>();
    CHECK(interpString != nullptr);
    CHECK(interpString->expressions.size == 2);
    CHECK(interpString->location.begin == Position(0, 6));
    CHECK(interpString->location.end == Position(0, 18));
    CHECK_EQ(parseResult.errors.size(), 2);

    auto err = parseResult.errors[0];
    CHECK_EQ(err.getMessage(), "Malformed interpolated string; did you forget to add a '}'?");
    CHECK_EQ(err.getLocation(), Location({0, 17}, {0, 18}));
}

TEST_CASE_FIXTURE(Fixture, "parsing_incomplete_string_interpolation_missing_curly_broken_string")
{
    ScopedFastFlag _{FFlag::LuauParseIncompleteInterpStringsWithLocation, true};
    auto parseResult = tryParse(R"(print(`{e.x} {e.a
)");
    const auto first = parseResult.root->body.data[0];
    auto expr = first->as<AstStatExpr>();
    CHECK(expr != nullptr);
    auto call = expr->expr->as<AstExprCall>();
    CHECK(call != nullptr);
    auto interpString = call->args.data[0]->as<AstExprInterpString>();
    CHECK(interpString != nullptr);
    CHECK(interpString->expressions.size == 2);
    CHECK(interpString->location.begin == Position(0, 6));
    CHECK(interpString->location.end == Position(0, 17));
    CHECK_EQ(parseResult.errors.size(), 2);

    auto err = parseResult.errors[0];
    CHECK_EQ(err.getMessage(), "Malformed interpolated string; did you forget to add a '}'?");
    CHECK_EQ(err.getLocation(), Location({0, 16}, {0, 17}));
}

TEST_CASE_FIXTURE(Fixture, "parsing_incomplete_string_interpolation_missing_backtick_broken_string")
{
    ScopedFastFlag _{FFlag::LuauParseIncompleteInterpStringsWithLocation, true};
    auto parseResult = tryParse(R"(print(`{e.x} {e.a}
)");
    const auto first = parseResult.root->body.data[0];
    auto expr = first->as<AstStatExpr>();
    CHECK(expr != nullptr);
    auto call = expr->expr->as<AstExprCall>();
    CHECK(call != nullptr);
    auto interpString = call->args.data[0]->as<AstExprInterpString>();
    CHECK(interpString != nullptr);
    CHECK(interpString->expressions.size == 2);
    CHECK(interpString->location.begin == Position(0, 6));
    CHECK(interpString->location.end == Position(0, 18));
    CHECK_EQ(parseResult.errors.size(), 2);

    auto err = parseResult.errors[0];
    CHECK_EQ(err.getMessage(), "Malformed interpolated string; did you forget to add a '`'?");
    CHECK_EQ(err.getLocation(), Location({0, 17}, {0, 18}));
}

TEST_CASE_FIXTURE(Fixture, "parsing_incomplete_string_interpolation_missing_curly_with_backtick_broken_string")
{
    ScopedFastFlag _{FFlag::LuauParseIncompleteInterpStringsWithLocation, true};
    auto parseResult = tryParse(R"(print(`{e.x} {e.a`
)");
    const auto first = parseResult.root->body.data[0];
    auto expr = first->as<AstStatExpr>();
    CHECK(expr != nullptr);
    auto call = expr->expr->as<AstExprCall>();
    CHECK(call != nullptr);
    auto interpString = call->args.data[0]->as<AstExprInterpString>();
    CHECK(interpString != nullptr);
    CHECK(interpString->expressions.size == 2);
    CHECK(interpString->location.begin == Position(0, 6));
    CHECK(interpString->location.end == Position(0, 18));
    CHECK_EQ(parseResult.errors.size(), 2);

    auto err = parseResult.errors[0];
    CHECK_EQ(err.getMessage(), "Malformed interpolated string; did you forget to add a '}'?");
    CHECK_EQ(err.getLocation(), Location({0, 17}, {0, 18}));
}

TEST_SUITE_END();
