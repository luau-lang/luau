// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include <optional>

#include "Luau/Common.h"
#include "Luau/Ast.h"
#include "Luau/AstJsonEncoder.h"
#include "Luau/Parser.h"
#include "Luau/ParseOptions.h"
#include "Luau/ToString.h"

#include "Luau/FileUtils.h"

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
    Luau::assertHandler() = assertionHandler;

    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
        if (strncmp(flag->name, "Luau", 4) == 0)
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

    Luau::Allocator allocator;
    Luau::AstNameTable names(allocator);

    Luau::ParseOptions options;
    options.captureComments = true;
    options.allowDeclarationSyntax = true;

    Luau::ParseResult parseResult = Luau::Parser::parse(source.data(), source.size(), names, allocator, std::move(options));

    if (parseResult.errors.size() > 0)
    {
        fprintf(stderr, "Parse errors were encountered:\n");
        for (const Luau::ParseError& error : parseResult.errors)
        {
            fprintf(stderr, "  %s - %s\n", toString(error.getLocation()).c_str(), error.getMessage().c_str());
        }
        fprintf(stderr, "\n");
    }

    printf("%s", Luau::toJson(parseResult.root, parseResult.commentLocations).c_str());

    return parseResult.errors.size() > 0 ? 1 : 0;
}
