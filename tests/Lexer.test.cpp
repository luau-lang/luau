// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Lexer.h"

#include "Fixture.h"
#include "ScopedFlags.h"

#include "doctest.h"

using namespace Luau;

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

TEST_CASE("string_interpolation_basic")
{
    const std::string testInput = R"(`foo {"bar"}`)";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme interpBegin = lexer.next();
    CHECK_EQ(interpBegin.type, Lexeme::InterpStringBegin);

    Lexeme quote = lexer.next();
    CHECK_EQ(quote.type, Lexeme::QuotedString);

    Lexeme interpEnd = lexer.next();
    CHECK_EQ(interpEnd.type, Lexeme::InterpStringEnd);
    // The InterpStringEnd should start with }, not `.
    CHECK_EQ(interpEnd.location.begin.column, 11);
}

TEST_CASE("string_interpolation_full")
{
    const std::string testInput = R"(`foo {"bar"} {"baz"} end`)";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme interpBegin = lexer.next();
    CHECK_EQ(interpBegin.type, Lexeme::InterpStringBegin);
    CHECK_EQ(interpBegin.toString(), "`foo {");

    Lexeme quote1 = lexer.next();
    CHECK_EQ(quote1.type, Lexeme::QuotedString);
    CHECK_EQ(quote1.toString(), "\"bar\"");

    Lexeme interpMid = lexer.next();
    CHECK_EQ(interpMid.type, Lexeme::InterpStringMid);
    CHECK_EQ(interpMid.toString(), "} {");
    CHECK_EQ(interpMid.location.begin.column, 11);

    Lexeme quote2 = lexer.next();
    CHECK_EQ(quote2.type, Lexeme::QuotedString);
    CHECK_EQ(quote2.toString(), "\"baz\"");

    Lexeme interpEnd = lexer.next();
    CHECK_EQ(interpEnd.type, Lexeme::InterpStringEnd);
    CHECK_EQ(interpEnd.toString(), "} end`");
    CHECK_EQ(interpEnd.location.begin.column, 19);
}

TEST_CASE("string_interpolation_double_brace")
{
    const std::string testInput = R"(`foo{{bad}}bar`)";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    auto brokenInterpBegin = lexer.next();
    CHECK_EQ(brokenInterpBegin.type, Lexeme::BrokenInterpDoubleBrace);
    CHECK_EQ(std::string(brokenInterpBegin.data, brokenInterpBegin.getLength()), std::string("foo"));

    CHECK_EQ(lexer.next().type, Lexeme::Name);

    auto interpEnd = lexer.next();
    CHECK_EQ(interpEnd.type, Lexeme::InterpStringEnd);
    CHECK_EQ(std::string(interpEnd.data, interpEnd.getLength()), std::string("}bar"));
}

TEST_CASE("string_interpolation_double_but_unmatched_brace")
{
    const std::string testInput = R"(`{{oops}`, 1)";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    CHECK_EQ(lexer.next().type, Lexeme::BrokenInterpDoubleBrace);
    CHECK_EQ(lexer.next().type, Lexeme::Name);
    CHECK_EQ(lexer.next().type, Lexeme::InterpStringEnd);
    CHECK_EQ(lexer.next().type, ',');
    CHECK_EQ(lexer.next().type, Lexeme::Number);
}

TEST_CASE("string_interpolation_unmatched_brace")
{
    const std::string testInput = R"({
        `hello {"world"}
    } -- this might be incorrectly parsed as a string)";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    CHECK_EQ(lexer.next().type, '{');
    CHECK_EQ(lexer.next().type, Lexeme::InterpStringBegin);
    CHECK_EQ(lexer.next().type, Lexeme::QuotedString);
    CHECK_EQ(lexer.next().type, Lexeme::BrokenString);
    CHECK_EQ(lexer.next().type, '}');
}

TEST_CASE("string_interpolation_with_unicode_escape")
{
    const std::string testInput = R"(`\u{1F41B}`)";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    CHECK_EQ(lexer.next().type, Lexeme::InterpStringSimple);
    CHECK_EQ(lexer.next().type, Lexeme::Eof);
}

TEST_CASE("single_quoted_string")
{
    const std::string testInput = "'test'";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme lexeme = lexer.next();
    CHECK_EQ(lexeme.type, Lexeme::QuotedString);
    CHECK_EQ(lexeme.getQuoteStyle(), Lexeme::QuoteStyle::Single);
}

TEST_CASE("double_quoted_string")
{
    const std::string testInput = R"("test")";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme lexeme = lexer.next();
    CHECK_EQ(lexeme.type, Lexeme::QuotedString);
    CHECK_EQ(lexeme.getQuoteStyle(), Lexeme::QuoteStyle::Double);
}

TEST_CASE("lexer_determines_string_block_depth_0")
{
    const std::string testInput = "[[ test ]]";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme lexeme = lexer.next();
    REQUIRE_EQ(lexeme.type, Lexeme::RawString);
    CHECK_EQ(lexeme.getBlockDepth(), 0);
}

TEST_CASE("lexer_determines_string_block_depth_0_multiline_1")
{
    const std::string testInput = R"([[ test
    ]])";

    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme lexeme = lexer.next();
    REQUIRE_EQ(lexeme.type, Lexeme::RawString);
    CHECK_EQ(lexeme.getBlockDepth(), 0);
}

TEST_CASE("lexer_determines_string_block_depth_0_multiline_2")
{
    const std::string testInput = R"([[
    test
    ]])";

    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme lexeme = lexer.next();
    REQUIRE_EQ(lexeme.type, Lexeme::RawString);
    CHECK_EQ(lexeme.getBlockDepth(), 0);
}

TEST_CASE("lexer_determines_string_block_depth_0_multiline_3")
{
    const std::string testInput = R"([[
    test ]])";

    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme lexeme = lexer.next();
    REQUIRE_EQ(lexeme.type, Lexeme::RawString);
    CHECK_EQ(lexeme.getBlockDepth(), 0);
}

TEST_CASE("lexer_determines_string_block_depth_1")
{
    const std::string testInput = "[=[[%s]]=]";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme lexeme = lexer.next();
    REQUIRE_EQ(lexeme.type, Lexeme::RawString);
    CHECK_EQ(lexeme.getBlockDepth(), 1);
}

TEST_CASE("lexer_determines_string_block_depth_2")
{
    const std::string testInput = "[==[ test ]==]";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme lexeme = lexer.next();
    REQUIRE_EQ(lexeme.type, Lexeme::RawString);
    CHECK_EQ(lexeme.getBlockDepth(), 2);
}

TEST_CASE("lexer_determines_string_block_depth_2_multiline_1")
{
    const std::string testInput = R"([==[ test
    ]==])";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme lexeme = lexer.next();
    REQUIRE_EQ(lexeme.type, Lexeme::RawString);
    CHECK_EQ(lexeme.getBlockDepth(), 2);
}

TEST_CASE("lexer_determines_string_block_depth_2_multiline_2")
{
    const std::string testInput = R"([==[
    test
    ]==])";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme lexeme = lexer.next();
    REQUIRE_EQ(lexeme.type, Lexeme::RawString);
    CHECK_EQ(lexeme.getBlockDepth(), 2);
}

TEST_CASE("lexer_determines_string_block_depth_2_multiline_3")
{
    const std::string testInput = R"([==[

    test ]==])";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme lexeme = lexer.next();
    REQUIRE_EQ(lexeme.type, Lexeme::RawString);
    CHECK_EQ(lexeme.getBlockDepth(), 2);
}


TEST_CASE("lexer_determines_comment_block_depth_0")
{
    const std::string testInput = "--[[ test ]]";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme lexeme = lexer.next();
    REQUIRE_EQ(lexeme.type, Lexeme::BlockComment);
    CHECK_EQ(lexeme.getBlockDepth(), 0);
}

TEST_CASE("lexer_determines_string_block_depth_1")
{
    const std::string testInput = "--[=[ μέλλον ]=]";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme lexeme = lexer.next();
    REQUIRE_EQ(lexeme.type, Lexeme::BlockComment);
    CHECK_EQ(lexeme.getBlockDepth(), 1);
}

TEST_CASE("lexer_determines_string_block_depth_2")
{
    const std::string testInput = "--[==[ test ]==]";
    Luau::Allocator alloc;
    AstNameTable table(alloc);
    Lexer lexer(testInput.c_str(), testInput.size(), table);

    Lexeme lexeme = lexer.next();
    REQUIRE_EQ(lexeme.type, Lexeme::BlockComment);
    CHECK_EQ(lexeme.getBlockDepth(), 2);
}

TEST_SUITE_END();
