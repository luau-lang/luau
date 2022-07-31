// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details
#include "luacode.h"

#include "lluz/Compiler.h"

#include <string.h>

char* lluz_compile(const char* source, size_t size, lua_CompileOptions* options, size_t* outsize)
{
    lluz_ASSERT(outsize);

    lluz::CompileOptions opts;

    if (options)
    {
        static_assert(sizeof(lua_CompileOptions) == sizeof(lluz::CompileOptions), "C and C++ interface must match");
        memcpy(static_cast<void*>(&opts), options, sizeof(opts));
    }

    std::string result = compile(std::string(source, size), opts);

    char* copy = static_cast<char*>(malloc(result.size()));
    if (!copy)
        return nullptr;

    memcpy(copy, result.data(), result.size());
    *outsize = result.size();
    return copy;
}
