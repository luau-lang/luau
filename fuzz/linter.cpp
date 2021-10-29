// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include <string>
#include "Luau/TypeInfer.h"
#include "Luau/Linter.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Common.h"

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* Data, size_t Size)
{
    Luau::ParseOptions options;

    Luau::Allocator allocator;
    Luau::AstNameTable names(allocator);

    Luau::ParseResult parseResult = Luau::Parser::parse(reinterpret_cast<const char*>(Data), Size, names, allocator, options);

    // "static" here is to accelerate fuzzing process by only creating and populating the type environment once
    static Luau::NullModuleResolver moduleResolver;
    static Luau::InternalErrorReporter iceHandler;
    static Luau::TypeChecker sharedEnv(&moduleResolver, &iceHandler);
    static int once = (Luau::registerBuiltinTypes(sharedEnv), 1);
    (void)once;
    static int once2 = (Luau::freeze(sharedEnv.globalTypes), 1);
    (void)once2;

    if (parseResult.errors.empty())
    {
        Luau::TypeChecker typeck(&moduleResolver, &iceHandler);
        typeck.globalScope = sharedEnv.globalScope;

        Luau::LintOptions lintOptions;
        lintOptions.warningMask = ~0ull;

        Luau::lint(parseResult.root, names, typeck.globalScope, nullptr, lintOptions);
    }

    return 0;
}
