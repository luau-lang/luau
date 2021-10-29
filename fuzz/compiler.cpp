// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include <string>
#include "Luau/Compiler.h"
#include "Luau/Common.h"

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* Data, size_t Size)
{
    Luau::compile(std::string(reinterpret_cast<const char*>(Data), Size));
    return 0;
}
