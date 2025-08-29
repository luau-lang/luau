// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/Frontend.h"
#include "Luau/ModuleResolver.h"
#include "Luau/Parser.h"
#include "Luau/TypeInfer.h"

#include <string>

#include <string.h>

LUAU_FASTINT(LuauTypeInferRecursionLimit)
LUAU_FASTINT(LuauTypeInferTypePackLoopLimit)

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* Data, size_t Size)
{
    FInt::LuauTypeInferRecursionLimit.value = 100;
    FInt::LuauTypeInferTypePackLoopLimit.value = 100;

    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
    {
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = true;
    }

    Luau::ParseOptions options;

    Luau::Allocator allocator;
    Luau::AstNameTable names(allocator);

    Luau::ParseResult parseResult = Luau::Parser::parse(reinterpret_cast<const char*>(Data), Size, names, allocator, options);

    // "static" here is to accelerate fuzzing process by only creating and populating the type environment once
    static Luau::NullFileResolver fileResolver;
    static Luau::NullConfigResolver configResolver;
    static Luau::Frontend frontend{&fileResolver, &configResolver};
    static int once = (Luau::registerBuiltinGlobals(frontend, frontend.globals, false), 1);
    (void)once;
    static int once2 = (Luau::freeze(frontend.globals.globalTypes), 1);
    (void)once2;

    if (parseResult.errors.empty())
    {
        Luau::TypeChecker typeck(frontend.globals.globalScope, &frontend.moduleResolver, frontend.builtinTypes, &frontend.iceHandler);

        Luau::SourceModule module;
        module.root = parseResult.root;
        module.mode = Luau::Mode::Nonstrict;

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
