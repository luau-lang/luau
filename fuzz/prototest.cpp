// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "src/libfuzzer/libfuzzer_macro.h"
#include "luau.pb.h"

std::vector<std::string> protoprint(const luau::ModuleSet& stat, bool types);

DEFINE_PROTO_FUZZER(const luau::ModuleSet& message)
{
    std::vector<std::string> sources = protoprint(message, true);

    for (size_t i = 0; i < sources.size(); i++)
    {
        printf("Module 'l%d':\n", int(i));
        printf("%s\n", sources[i].c_str());
    }
}
