// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include <string>

#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/Frontend.h"
#include "Luau/Linter.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Parser.h"

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* Data, size_t Size)
{
    Luau::ParseOptions options;

    Luau::Allocator allocator;
    Luau::AstNameTable names(allocator);

    Luau::ParseResult parseResult = Luau::Parser::parse(reinterpret_cast<const char*>(Data), Size, names, allocator, options);

    // "static" here is to accelerate fuzzing process by only creating and populating the type environment once
    static Luau::NullFileResolver fileResolver;
    static Luau::NullConfigResolver configResolver;
    static Luau::Frontend frontend{&fileResolver, &configResolver};
    static int once = (Luau::registerBuiltinGlobals(frontend), 1);
    (void)once;
    static int once2 = (Luau::freeze(frontend.globals.globalTypes), 1);
    (void)once2;

    if (parseResult.errors.empty())
    {
        Luau::TypeChecker typeck(frontend.globals.globalScope, &frontend.moduleResolver, frontend.builtinTypes, &frontend.iceHandler);

        Luau::LintOptions lintOptions;
        lintOptions.warningMask = ~0ull;

        Luau::lint(parseResult.root, names, typeck.globalScope, nullptr, {}, lintOptions);
    }

    return 0;
}
