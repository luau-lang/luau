// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "Luau/Common.h"
#include "Luau/Location.h"
#include "Luau/Lexer.h"
#include "Luau/StringUtils.h"

namespace Luau
{

class AstStatBlock;

class ParseError : public std::exception
{
public:
    ParseError(const Location& location, const std::string& message);

    virtual const char* what() const throw();

    const Location& getLocation() const;
    const std::string& getMessage() const;

    static LUAU_NORETURN void raise(const Location& location, const char* format, ...) LUAU_PRINTF_ATTR(2, 3);

private:
    Location location;
    std::string message;
};

class ParseErrors : public std::exception
{
public:
    ParseErrors(std::vector<ParseError> errors);

    virtual const char* what() const throw();

    const std::vector<ParseError>& getErrors() const;

private:
    std::vector<ParseError> errors;
    std::string message;
};

struct HotComment
{
    bool header;
    Location location;
    std::string content;
};

struct Comment
{
    Lexeme::Type type; // Comment, BlockComment, or BrokenComment
    Location location;
};

struct ParseResult
{
    AstStatBlock* root;
    size_t lines = 0;

    std::vector<HotComment> hotcomments;
    std::vector<ParseError> errors;

    std::vector<Comment> commentLocations;
};

static constexpr const char* kParseNameError = "%error-id%";

} // namespace Luau
