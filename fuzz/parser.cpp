// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include <string>
#include "Luau/Parser.h"
#include "Luau/Common.h"

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* Data, size_t Size)
{
    Luau::ParseOptions options;

    Luau::Allocator allocator;
    Luau::AstNameTable names(allocator);

    Luau::Parser::parse(reinterpret_cast<const char*>(Data), Size, names, allocator, options);
    return 0;
}
