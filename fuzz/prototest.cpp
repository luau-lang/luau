// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "src/libfuzzer/libfuzzer_macro.h"
#include "luau.pb.h"

std::string protoprint(const luau::StatBlock& stat, bool types);

DEFINE_PROTO_FUZZER(const luau::StatBlock& message)
{
    std::string source = protoprint(message, true);

    printf("%s\n", source.c_str());
}
