// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Parser.h"
#include "Luau/TypeInfer.h"

#include "Fixture.h"
#include "ScopedFlags.h"

#include "doctest.h"

LUAU_FASTFLAG(LuauFixAmbiguousErrorRecoveryInAssign)

using namespace Luau;

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

// TODO: delete this and replace all other use of this function with matchParseError
std::string getParseError(const std::string& code)
{
    Fixture f;

    try
    {
        f.parse(code);
    }
    catch (const Luau::ParseErrors& e)
    {
        // in general, tests check only the first error
        return e.getErrors().front().getMessage();
    }

    throw std::runtime_error("Expected a parse error in '" + code + "'");
}

} // namespace

TEST_SUITE_BEGIN("AllocatorTests");

TEST_CASE("allocator_can_be_moved")
{
    Counter* c = nullptr;
    auto inner = [&]() {
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

TEST_SUITE_BEGIN("LexerTests");

TEST_CASE("broken_string_works")
{
    const std::string testInput = "[[";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);
    Lexeme lexeme = lexer.next();
    CHECK_EQ(lexeme.type, Lexeme::Type::BrokenString);
    CHECK_EQ(lexeme.location, Luau::Location(Luau::Position(0, 0), Luau::Position(0, 2)));
}

TEST_CASE("broken_comment")
{
    const std::string testInput = "--[[  ";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);
    Lexeme lexeme = lexer.next();
    CHECK_EQ(lexeme.type, Lexeme::Type::BrokenComment);
    CHECK_EQ(lexeme.location, Luau::Location(Luau::Position(0, 0), Luau::Position(0, 6)));
}

TEST_CASE("broken_comment_kept")
{
    const std::string testInput = "--[[  ";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);
    lexer.setSkipComments(true);
    CHECK_EQ(lexer.next().type, Lexeme::Type::BrokenComment);
}

TEST_CASE("comment_skipped")
{
    const std::string testInput = "--  ";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);
    lexer.setSkipComments(true);
    CHECK_EQ(lexer.next().type, Lexeme::Type::Eof);
}

TEST_CASE("multilineCommentWithLexemeInAndAfter")
{
    const std::string testInput = "--[[ function \n"
                                  "]] end";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);
    Lexeme comment = lexer.next();
    Lexeme end = lexer.next();

    CHECK_EQ(comment.type, Lexeme::Type::BlockComment);
    CHECK_EQ(comment.location, Luau::Location(Luau::Position(0, 0), Luau::Position(1, 2)));
    CHECK_EQ(end.type, Lexeme::Type::ReservedEnd);
    CHECK_EQ(end.location, Luau::Location(Luau::Position(1, 3), Luau::Position(1, 6)));
}

TEST_CASE("testBrokenEscapeTolerant")
{
    const std::string testInput = "'\\3729472897292378'";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);
    Lexeme item = lexer.next();

    CHECK_EQ(item.type, Lexeme::QuotedString);
    CHECK_EQ(item.location, Luau::Location(Luau::Position(0, 0), Luau::Position(0, int(testInput.size()))));
}

TEST_CASE("testBigDelimiters")
{
    const std::string testInput = "--[===[\n"
                                  "\n"
                                  "\n"
                                  "\n"
                                  "]===]";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);
    Lexeme item = lexer.next();

    CHECK_EQ(item.type, Lexeme::Type::BlockComment);
    CHECK_EQ(item.location, Luau::Location(Luau::Position(0, 0), Luau::Position(4, 5)));
}

TEST_CASE("lookahead")
{
    const std::string testInput = "foo --[[ comment ]] bar : nil end";

    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);
    lexer.setSkipComments(true);
    lexer.next(); // must call next() before reading data from lexer at least once

    CHECK_EQ(lexer.current().type, Lexeme::Name);
    CHECK_EQ(lexer.current().name, std::string("foo"));
    CHECK_EQ(lexer.lookahead().type, Lexeme::Name);
    CHECK_EQ(lexer.lookahead().name, std::string("bar"));

    lexer.next();

    CHECK_EQ(lexer.current().type, Lexeme::Name);
    CHECK_EQ(lexer.current().name, std::string("bar"));
    CHECK_EQ(lexer.lookahead().type, ':');

    lexer.next();

    CHECK_EQ(lexer.current().type, ':');
    CHECK_EQ(lexer.lookahead().type, Lexeme::ReservedNil);

    lexer.next();

    CHECK_EQ(lexer.current().type, Lexeme::ReservedNil);
    CHECK_EQ(lexer.lookahead().type, Lexeme::ReservedEnd);

    lexer.next();

    CHECK_EQ(lexer.current().type, Lexeme::ReservedEnd);
    CHECK_EQ(lexer.lookahead().type, Lexeme::Eof);

    lexer.next();

    CHECK_EQ(lexer.current().type, Lexeme::Eof);
    CHECK_EQ(lexer.lookahead().type, Lexeme::Eof);
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

TEST_CASE_FIXTURE(Fixture, "local_cannot_have_annotation_with_extensions_disabled")
{
    Luau::ParseOptions options;
    options.allowTypeAnnotations = false;

    CHECK_THROWS_AS(parse("local foo: string = \"Hello Types!\"", options), std::exception);
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

TEST_CASE_FIXTURE(Fixture, "functions_cannot_have_return_annotations_if_extensions_are_disabled")
{
    Luau::ParseOptions options;
    options.allowTypeAnnotations = false;

    CHECK_THROWS_AS(parse("function foo(): number return 55 end", options), std::exception);
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

    CHECK_EQ(statFunction->func->returnAnnotation.types.size, 1);
    CHECK(statFunction->func->returnAnnotation.tailType == nullptr);
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

    AstArray<AstType*>& retTypes = statFunc->func->returnAnnotation.types;
    REQUIRE(statFunc->func->hasReturnAnnotation);
    CHECK(statFunc->func->returnAnnotation.tailType == nullptr);
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

    AstArray<AstType*>& retTypes = statFunc->func->returnAnnotation.types;
    REQUIRE(statFunc->func->hasReturnAnnotation);
    CHECK(statFunc->func->returnAnnotation.tailType == nullptr);
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

    AstArray<AstType*>& retTypes = statFunc->func->returnAnnotation.types;
    REQUIRE(statFunc->func->hasReturnAnnotation);
    CHECK(statFunc->func->returnAnnotation.tailType == nullptr);
    REQUIRE(retTypes.size == 1);

    AstTypeFunction* funTy = retTypes.data[0]->as<AstTypeFunction>();
    REQUIRE(funTy != nullptr);
    REQUIRE(funTy->argTypes.types.size == 0);
    CHECK(funTy->argTypes.tailType == nullptr);
    CHECK(funTy->returnTypes.tailType == nullptr);

    AstTypeReference* ty = funTy->returnTypes.types.data[0]->as<AstTypeReference>();
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

    AstTypeIntersection* returnAnnotation = annotation->returnTypes.types.data[0]->as<AstTypeIntersection>();
    REQUIRE(returnAnnotation != nullptr);
    CHECK(returnAnnotation->types.data[0]->as<AstTypeReference>());
    CHECK(returnAnnotation->types.data[1]->as<AstTypeFunction>());
}

TEST_CASE_FIXTURE(Fixture, "illegal_type_alias_if_extensions_are_disabled")
{
    Luau::ParseOptions options;
    options.allowTypeAnnotations = false;

    CHECK_THROWS_AS(parse("type A = number", options), std::exception);
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

TEST_CASE_FIXTURE(Fixture, "parse_error_messages")
{
    CHECK_EQ(getParseError(R"(
            local a: (number, number) -> (string
        )"),
        "Expected ')' (to close '(' at line 2), got <eof>");

    CHECK_EQ(getParseError(R"(
            local a: (number, number) -> (
                string
        )"),
        "Expected ')' (to close '(' at line 2), got <eof>");

    CHECK_EQ(getParseError(R"(
            local a: (number, number)
        )"),
        "Expected '->' when parsing function type, got <eof>");

    CHECK_EQ(getParseError(R"(
            local a: (number, number
        )"),
        "Expected ')' (to close '(' at line 2), got <eof>");

    CHECK_EQ(getParseError(R"(
            local a: {foo: string,
        )"),
        "Expected identifier when parsing table field, got <eof>");

    CHECK_EQ(getParseError(R"(
            local a: {foo: string
        )"),
        "Expected '}' (to close '{' at line 2), got <eof>");

    CHECK_EQ(getParseError(R"(
            local a: { [string]: number, [number]: string }
        )"),
        "Cannot have more than one table indexer");

    CHECK_EQ(getParseError(R"(
            type T = <a>foo
        )"),
        "Expected '(' when parsing function parameters, got 'foo'");
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
    CHECK_EQ(getParseError("type 5 = number"), "Expected identifier when parsing type name, got '5'");
    CHECK_EQ(getParseError("type A"), "Expected '=' when parsing type alias, got <eof>");
    CHECK_EQ(getParseError("type A<"), "Expected identifier, got <eof>");
    CHECK_EQ(getParseError("type A<B"), "Expected '>' (to close '<' at column 7), got <eof>");
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
    ParseResult result = parseEx("   --!strict ");
    std::optional<Mode> mode = parseMode(result.hotcomments);
    REQUIRE(bool(mode));
    CHECK_EQ(int(*mode), int(Mode::Strict));
}

TEST_CASE_FIXTURE(Fixture, "stop_if_line_ends_with_hyphen")
{
    CHECK_THROWS_AS(parse("   -"), std::exception);
}

TEST_CASE_FIXTURE(Fixture, "nonstrict_mode")
{
    ParseResult result = parseEx("--!nonstrict");
    CHECK(result.errors.empty());
    std::optional<Mode> mode = parseMode(result.hotcomments);
    REQUIRE(bool(mode));
    CHECK_EQ(int(*mode), int(Mode::Nonstrict));
}

TEST_CASE_FIXTURE(Fixture, "nocheck_mode")
{
    ParseResult result = parseEx("--!nocheck");
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
    CHECK_EQ(getParseError(R"(
            local a: Foo.=
        )"),
        "Expected identifier when parsing field name, got '='");
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
    AstStat* stat = parse("return 0xab, 0XAB05, 0xff_ff");
    REQUIRE(stat != nullptr);

    AstStatReturn* str = stat->as<AstStatBlock>()->body.data[0]->as<AstStatReturn>();
    CHECK(str->list.size == 3);
    CHECK_EQ(str->list.data[0]->as<AstExprConstantNumber>()->value, 0xab);
    CHECK_EQ(str->list.data[1]->as<AstExprConstantNumber>()->value, 0xAB05);
    CHECK_EQ(str->list.data[2]->as<AstExprConstantNumber>()->value, 0xFFFF);
}

TEST_CASE_FIXTURE(Fixture, "parse_numbers_binary")
{
    AstStat* stat = parse("return 0b1, 0b0, 0b101010");
    REQUIRE(stat != nullptr);

    AstStatReturn* str = stat->as<AstStatBlock>()->body.data[0]->as<AstStatReturn>();
    CHECK(str->list.size == 3);
    CHECK_EQ(str->list.data[0]->as<AstExprConstantNumber>()->value, 1);
    CHECK_EQ(str->list.data[1]->as<AstExprConstantNumber>()->value, 0);
    CHECK_EQ(str->list.data[2]->as<AstExprConstantNumber>()->value, 42);
}

TEST_CASE_FIXTURE(Fixture, "parse_numbers_error")
{
    CHECK_EQ(getParseError("return 0b123"), "Malformed number");
    CHECK_EQ(getParseError("return 123x"), "Malformed number");
    CHECK_EQ(getParseError("return 0xg"), "Malformed number");
}

TEST_CASE_FIXTURE(Fixture, "break_return_not_last_error")
{
    CHECK_EQ(getParseError("return 0 print(5)"), "Expected <eof>, got 'print'");
    CHECK_EQ(getParseError("while true do break print(5) end"), "Expected 'end' (to close 'do' at column 12), got 'print'");
}

TEST_CASE_FIXTURE(Fixture, "error_on_unicode")
{
    CHECK_EQ(getParseError(R"(
            local ☃ = 10
        )"),
        "Expected identifier when parsing variable name, got Unicode character U+2603");
}

TEST_CASE_FIXTURE(Fixture, "allow_unicode_in_string")
{
    ParseResult result = parseEx("local snowman = \"☃\"");
    CHECK(result.errors.empty());
}

TEST_CASE_FIXTURE(Fixture, "error_on_confusable")
{
    CHECK_EQ(getParseError(R"(
            local pi = 3․13
        )"),
        "Expected identifier when parsing expression, got Unicode character U+2024 (did you mean '.'?)");
}

TEST_CASE_FIXTURE(Fixture, "error_on_non_utf8_sequence")
{
    const char* expected = "Expected identifier when parsing expression, got invalid UTF-8 sequence";

    CHECK_EQ(getParseError("local pi = \xFF!"), expected);
    CHECK_EQ(getParseError("local pi = \xE2!"), expected);
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
    CHECK_EQ(getParseError("while true do continue print(5) end"), "Expected 'end' (to close 'do' at column 12), got 'print'");
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
    CHECK_EQ(getParseError("fiddlesticks"), "Incomplete statement: expected assignment or a function call");
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
        CHECK_EQ("Expected 'end' (to close 'function' at line 2), got <eof>; did you forget to close 'else' at line 8?",
            e.getErrors().front().getMessage());
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
        CHECK_EQ("Expected 'end' (to close 'function' at line 2), got <eof>; did you forget to close 'else' at line 3?",
            e.getErrors().front().getMessage());
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
        CHECK_EQ("Expected 'until' (to close 'repeat' at line 2), got <eof>; did you forget to close 'repeat' at line 4?",
            e.getErrors().front().getMessage());
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
        CHECK_EQ("Expected 'end' (to close 'function' at line 2), got <eof>; did you forget to close 'else' at line 8?",
            e.getErrors().front().getMessage());
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
        CHECK_EQ("Expected 'end' (to close 'function' at line 2), got <eof>; did you forget to close 'else' at line 8?",
            e.getErrors().front().getMessage());
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
            "Expected ')' (to close '(' at column 17), got '='; did you mean to use '{' when defining a table?", e.getErrors().front().getMessage());
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
    ScopedFastInt sfis{"LuauRecursionLimit", 20};

    matchParseError(
        "function f(): (((((((((Fail))))))))) end", "Exceeded allowed recursion depth; simplify your type annotation to make the code compile");

    matchParseError("function f(): () -> () -> () -> () -> () -> () -> () -> () -> () -> () -> () end",
        "Exceeded allowed recursion depth; simplify your type annotation to make the code compile");

    matchParseError(
        "local t: {a: {b: {c: {d: {e: {f: {}}}}}}}", "Exceeded allowed recursion depth; simplify your type annotation to make the code compile");
}

TEST_CASE_FIXTURE(Fixture, "parse_error_with_too_many_nested_if_statements")
{
    ScopedFastInt sfis{"LuauRecursionLimit", 10};

    matchParseErrorPrefix(
        "function f() if true then if true then if true then if true then if true then if true then if true then if true then if true "
        "then if true then if true then end end end end end end end end end end end end",
        "Exceeded allowed recursion depth;");
}

TEST_CASE_FIXTURE(Fixture, "parse_error_with_too_many_changed_elseif_statements")
{
    ScopedFastInt sfis{"LuauRecursionLimit", 10};

    matchParseErrorPrefix(
        "function f() if false then elseif false then elseif false then elseif false then elseif false then elseif false then elseif "
        "false then elseif false then elseif false then elseif false then elseif false then end end",
        "Exceeded allowed recursion depth;");
}

TEST_CASE_FIXTURE(Fixture, "parse_error_with_too_many_nested_ifelse_expressions1")
{
    ScopedFastInt sfis{"LuauRecursionLimit", 10};

    matchParseError("function f() return if true then 1 elseif true then 2 elseif true then 3 elseif true then 4 elseif true then 5 elseif true then "
                    "6 elseif true then 7 elseif true then 8 elseif true then 9 elseif true then 10 else 11 end",
        "Exceeded allowed recursion depth; simplify your expression to make the code compile");
}

TEST_CASE_FIXTURE(Fixture, "parse_error_with_too_many_nested_ifelse_expressions2")
{
    ScopedFastInt sfis{"LuauRecursionLimit", 10};

    matchParseError(
        "function f() return if if if if if if if if if if true then false else true then false else true then false else true then false else true "
        "then false else true then false else true then false else true then false else true then false else true then 1 else 2 end",
        "Exceeded allowed recursion depth; simplify your expression to make the code compile");
}

TEST_CASE_FIXTURE(Fixture, "unparenthesized_function_return_type_list")
{
    matchParseError(
        "function foo(): string, number end", "Expected a statement, got ','; did you forget to wrap the list of return types in parentheses?");

    matchParseError("function foo(): (number) -> string, string",
        "Expected a statement, got ','; did you forget to wrap the list of return types in parentheses?");

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
    ScopedFastFlag luauStartingBrokenComment{"LuauStartingBrokenComment", true};

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
    matchParseError("return \"", "Malformed string");
    matchParseError("return \"\\", "Malformed string");
    matchParseError("return \"\r\r", "Malformed string");
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

TEST_CASE_FIXTURE(Fixture, "parse_error_loop_control")
{
    matchParseError("break", "break statement must be inside a loop");
    matchParseError("repeat local function a() break end until false", "break statement must be inside a loop");
    matchParseError("continue", "continue statement must be inside a loop");
    matchParseError("repeat local function a() continue end until false", "continue statement must be inside a loop");
}

TEST_CASE_FIXTURE(Fixture, "parse_error_confusing_function_call")
{
    auto result1 = matchParseError(R"(
        function add(x, y) return x + y end
        add
        (4, 7)
    )",
        "Ambiguous syntax: this looks like an argument list for a function call, but could also be a start of new statement; use ';' to separate "
        "statements");

    CHECK(result1.errors.size() == 1);

    auto result2 = matchParseError(R"(
        function add(x, y) return x + y end
        local f = add
        (f :: any)['x'] = 2
    )",
        "Ambiguous syntax: this looks like an argument list for a function call, but could also be a start of new statement; use ';' to separate "
        "statements");

    CHECK(result2.errors.size() == 1);

    auto result3 = matchParseError(R"(
        local x = {}
        function x:add(a, b) return a + b end
        x:add
        (1, 2)
    )",
        "Ambiguous syntax: this looks like an argument list for a function call, but could also be a start of new statement; use ';' to separate "
        "statements");

    CHECK(result3.errors.size() == 1);

    auto result4 = matchParseError(R"(
        local t = {}
        function f() return t end
        t.x, (f)
        ().y = 5, 6
    )",
        "Ambiguous syntax: this looks like an argument list for a function call, but could also be a start of new statement; use ';' to separate "
        "statements");

    if (FFlag::LuauFixAmbiguousErrorRecoveryInAssign)
        CHECK(result4.errors.size() == 1);
    else
        CHECK(result4.errors.size() == 5);
}

TEST_CASE_FIXTURE(Fixture, "parse_error_varargs")
{
    matchParseError("function add(x, y) return ... end", "Cannot use '...' outside of a vararg function");
}

TEST_CASE_FIXTURE(Fixture, "parse_error_assignment_lvalue")
{
    matchParseError(R"(
        local a, b
        (2), b = b, a
    )",
        "Assigned expression must be a variable or a field");

    matchParseError(R"(
        local a, b
        a, (3) = b, a
    )",
        "Assigned expression must be a variable or a field");
}

TEST_CASE_FIXTURE(Fixture, "parse_error_type_annotation")
{
    matchParseError("local a : 2 = 2", "Expected type, got '2'");
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
    CHECK(global->type);

    AstStatDeclareFunction* func = stat->body.data[1]->as<AstStatDeclareFunction>();
    REQUIRE(func);
    CHECK(func->name == "bar");
    REQUIRE_EQ(func->params.types.size, 1);
    REQUIRE_EQ(func->retTypes.types.size, 1);

    AstStatDeclareFunction* varFunc = stat->body.data[2]->as<AstStatDeclareFunction>();
    REQUIRE(varFunc);
    CHECK(varFunc->name == "var");
    CHECK(varFunc->params.tailType);

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

    AstStatDeclareClass* declaredClass = stat->body.data[0]->as<AstStatDeclareClass>();
    REQUIRE(declaredClass);
    CHECK(declaredClass->name == "Foo");
    CHECK(!declaredClass->superName);

    REQUIRE_EQ(declaredClass->props.size, 2);

    AstDeclaredClassProp& prop = declaredClass->props.data[0];
    CHECK(prop.name == "prop");
    CHECK(prop.ty->is<AstTypeReference>());

    AstDeclaredClassProp& method = declaredClass->props.data[1];
    CHECK(method.name == "method");
    CHECK(method.ty->is<AstTypeFunction>());

    AstStatDeclareClass* subclass = stat->body.data[1]->as<AstStatDeclareClass>();
    REQUIRE(subclass);
    REQUIRE(subclass->superName);
    CHECK(subclass->name == "Bar");
    CHECK(*subclass->superName == "Foo");

    REQUIRE_EQ(subclass->props.size, 1);
    AstDeclaredClassProp& prop2 = subclass->props.data[0];
    CHECK(prop2.name == "prop2");
    CHECK(prop2.ty->is<AstTypeReference>());
}

TEST_CASE_FIXTURE(Fixture, "class_method_properties")
{
    const ParseResult p1 = matchParseError(R"(
        declare class Foo
            -- method's first parameter must be 'self'
            function method(foo: number)
            function method2(self)
        end
        )",
        "'self' must be present as the unannotated first parameter");

    REQUIRE_EQ(1, p1.root->body.size);

    AstStatDeclareClass* klass = p1.root->body.data[0]->as<AstStatDeclareClass>();
    REQUIRE(klass != nullptr);

    CHECK_EQ(2, klass->props.size);

    const ParseResult p2 = matchParseError(R"(
        declare class Foo
            function method(self, foo)
            function method2()
        end
        )",
        "All declaration parameters aside from 'self' must be annotated");

    REQUIRE_EQ(1, p2.root->body.size);

    AstStatDeclareClass* klass2 = p2.root->body.data[0]->as<AstStatDeclareClass>();
    REQUIRE(klass2 != nullptr);

    CHECK_EQ(2, klass2->props.size);
}

TEST_CASE_FIXTURE(Fixture, "parse_variadics")
{
    //clang-format off
    AstStatBlock* stat = parseEx(R"(
        function foo(bar, ...: number): ...string
        end

        type Foo = (string, number, ...number) -> ...boolean
        type Bar = () -> (number, ...boolean)
    )")
                             .root;
    //clang-format on

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
    CHECK_EQ(fnFoo->returnTypes.types.size, 0);
    CHECK(fnFoo->returnTypes.tailType);

    AstStatTypeAlias* bar = stat->body.data[2]->as<AstStatTypeAlias>();
    REQUIRE(bar);
    AstTypeFunction* fnBar = bar->type->as<AstTypeFunction>();
    REQUIRE(fnBar);
    CHECK_EQ(fnBar->argTypes.types.size, 0);
    CHECK(!fnBar->argTypes.tailType);
    CHECK_EQ(fnBar->returnTypes.types.size, 1);
    CHECK(fnBar->returnTypes.tailType);
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

    AstTypePackGeneric* retAnnot = fnTy->returnTypes.tailType->as<AstTypePackGeneric>();
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
        AstTypeFunction* funcRet = func->returnTypes.types.data[0]->as<AstTypeFunction>();
        REQUIRE(funcRet != nullptr);
        REQUIRE_EQ(funcRet->argTypes.types.size, 3);
        REQUIRE_EQ(funcRet->argNames.size, 3);
        REQUIRE(func->argNames.data[2]);
        CHECK_EQ(funcRet->argNames.data[2]->first, "f");
    }

    matchParseError("type MyFunc = (a: number, b: string, c: number) -> (d: number, e: string, f: number)",
        "Expected '->' when parsing function type, got <eof>");

    matchParseError("type MyFunc = (number) -> (d: number) <a, b, c> -> number", "Expected '->' when parsing function type, got '<'");
}

TEST_CASE_FIXTURE(Fixture, "function_type_matching_parenthesis")
{
    matchParseError("local a: <T>(number -> string", "Expected ')' (to close '(' at column 13), got '->'");
}

TEST_CASE_FIXTURE(Fixture, "parse_type_alias_default_type")
{
    ScopedFastFlag luauParseTypeAliasDefaults{"LuauParseTypeAliasDefaults", true};

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
    ScopedFastFlag luauParseTypeAliasDefaults{"LuauParseTypeAliasDefaults", true};

    matchParseError("type Y<T = number, U> = {}", "Expected default type after type name", Location{{0, 20}, {0, 21}});
    matchParseError("type Y<T... = ...number, U...> = {}", "Expected default type pack after type pack name", Location{{0, 29}, {0, 30}});
    matchParseError("type Y<T... = (string) -> number> = {}", "Expected type pack after '=', got type", Location{{0, 14}, {0, 32}});
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
    ScopedFastInt luauParseErrorLimit("LuauParseErrorLimit", 1);

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
    ScopedFastInt luauParseErrorLimit("LuauParseErrorLimit", 2);

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
    auto checkAstEquivalence = [this](const char* codeWithErrors, const char* code) {
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

    auto checkRecovery = [this, checkAstEquivalence](const char* codeWithErrors, const char* code, unsigned expectedErrorCount) {
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

    checkRecovery("function foo(a, b. c) return a + b end", "function foo(a, b) return a + b end", 1);
    checkRecovery("function foo(a, b: { a: number, b: number. c:number }) return a + b end",
        "function foo(a, b: { a: number, b: number }) return a + b end", 1);

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
    checkRecovery(R"(
function foo(a, b
    return a + b
end
)",
        "function foo(a, b) return a + b end", 1);
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
    matchParseError("local a = 4 != 10", "Unexpected '!=', did you mean '~='?");
    matchParseError("local a = true && false", "Unexpected '&&', did you mean 'and'?");
    matchParseError("local a = false || true", "Unexpected '||', did you mean 'or'?");

    // Unary
    matchParseError("local a = !false", "Unexpected '!', did you mean 'not'?");

    // Check that separate tokens are not considered as a single one
    matchParseError("local a = 4 ! = 10", "Expected identifier when parsing expression, got '!'");
    matchParseError("local a = true & & false", "Expected identifier when parsing expression, got '&'");
    matchParseError("local a = false | | true", "Expected identifier when parsing expression, got '|'");
}

TEST_CASE_FIXTURE(Fixture, "capture_comments")
{
    ParseOptions options;
    options.captureComments = true;

    ParseResult result = parseEx(R"(
        --!strict

        local a = 5 -- comment one
        local b = 8 -- comment two
        --[[
            Multi line comment
        ]]
        local c = 'see'
    )",
        options);

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

    ParseResult result = tryParse(R"(
        --[[
    )",
        options);

    CHECK_EQ(1, result.commentLocations.size());
    CHECK_EQ((Location{{1, 8}, {2, 4}}), result.commentLocations[0].location);
}

TEST_CASE_FIXTURE(Fixture, "capture_broken_comment")
{
    ParseOptions options;
    options.captureComments = true;

    ParseResult result = tryParse(R"(
        local a = "test"

        --[[broken!
    )",
        options);

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
    ScopedFastFlag luauParseTypeAliasDefaults{"LuauParseTypeAliasDefaults", true};
    ScopedFastFlag luauParseRecoverTypePackEllipsis{"LuauParseRecoverTypePackEllipsis", true};

    ParseResult result = tryParse(R"(
type Y<T..., U = T...> = (T...) -> U...
    )");
    CHECK_EQ(1, result.errors.size());
}

TEST_SUITE_END();
