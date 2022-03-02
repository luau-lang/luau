// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include <string>

#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Parser.h"
#include "Luau/TypeInfer.h"

LUAU_FASTINT(LuauTypeInferRecursionLimit)
LUAU_FASTINT(LuauTypeInferTypePackLoopLimit)

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* Data, size_t Size)
{
    FInt::LuauTypeInferRecursionLimit.value = 100;
    FInt::LuauTypeInferTypePackLoopLimit.value = 100;

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
        Luau::SourceModule module;
        module.root = parseResult.root;
        module.mode = Luau::Mode::Nonstrict;

        Luau::TypeChecker typeck(&moduleResolver, &iceHandler);
        typeck.globalScope = sharedEnv.globalScope;

        try
        {
            typeck.check(module, Luau::Mode::Nonstrict);
        }
        catch (std::exception&)
        {
            // This catches internal errors that the type checker currently (unfortunately) throws in some cases
        }
    }

    return 0;
}
