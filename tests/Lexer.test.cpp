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

TEST_SUITE_END();
