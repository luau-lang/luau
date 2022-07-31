// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#include <optional>

#include "lluz/Common.h"
#include "lluz/Ast.h"
#include "lluz/JsonEncoder.h"
#include "lluz/Parser.h"
#include "lluz/ParseOptions.h"

#include "FileUtils.h"

static void displayHelp(const char* argv0)
{
    printf("Usage: %s [file]\n", argv0);
}

static int assertionHandler(const char* expr, const char* file, int line, const char* function)
{
    printf("%s(%d): ASSERTION FAILED: %s\n", file, line, expr);
    return 1;
}

int main(int argc, char** argv)
{
    lluz::assertHandler() = assertionHandler;

    for (lluz::FValue<bool>* flag = lluz::FValue<bool>::list; flag; flag = flag->next)
        if (strncmp(flag->name, "lluz", 4) == 0)
            flag->value = true;

    if (argc >= 2 && strcmp(argv[1], "--help") == 0)
    {
        displayHelp(argv[0]);
        return 0;
    }
    else if (argc < 2)
    {
        displayHelp(argv[0]);
        return 1;
    }

    const char* name = argv[1];
    std::optional<std::string> maybeSource = std::nullopt;
    if (strcmp(name, "-") == 0)
    {
        maybeSource = readStdin();
    }
    else
    {
        maybeSource = readFile(name);
    }

    if (!maybeSource)
    {
        fprintf(stderr, "Couldn't read source %s\n", name);
        return 1;
    }

    std::string source = *maybeSource;

    lluz::Allocator allocator;
    lluz::AstNameTable names(allocator);

    lluz::ParseOptions options;
    options.supportContinueStatement = true;
    options.allowTypeAnnotations = true;
    options.allowDeclarationSyntax = true;

    lluz::ParseResult parseResult = lluz::Parser::parse(source.data(), source.size(), names, allocator, options);

    if (parseResult.errors.size() > 0)
    {
        fprintf(stderr, XorStr("Parse errors were encountered:\n"));
        for (const lluz::ParseError& error : parseResult.errors)
        {
            fprintf(stderr, "  %s - %s\n", toString(error.getLocation()).c_str(), error.getMessage().c_str());
        }
        fprintf(stderr, XorStr("\n"));
    }

    printf("%s", lluz::toJson(parseResult.root).c_str());

    return parseResult.errors.size() > 0 ? 1 : 0;
}
