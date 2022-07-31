// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#pragma once

#include "lluz/Common.h"
#include "lluz/Location.h"
#include "lluz/Lexer.h"
#include "lluz/StringUtils.h"

namespace lluz
{

class AstStatBlock;

class ParseError : public std::exception
{
public:
    ParseError(const Location& location, const std::string& message);

    virtual const char* what() const throw();

    const Location& getLocation() const;
    const std::string& getMessage() const;

    static lluz_NORETURN void raise(const Location& location, const char* format, ...) lluz_PRINTF_ATTR(2, 3);

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
    std::vector<HotComment> hotcomments;
    std::vector<ParseError> errors;

    std::vector<Comment> commentLocations;
};

static constexpr const char* kParseNameError = "%error-id%";

} // namespace lluz
