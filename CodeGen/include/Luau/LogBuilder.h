#pragma once

#include "Luau/CodeGenOptions.h"
#include "Luau/StringUtils.h"

#include <stdarg.h>

namespace Luau
{
namespace CodeGen
{

struct LogBuilder
{
    LogBuilder(AssemblyOptions& options)
        : options(options)
    {
    }

    void formatAppend(const char* fmt, ...) LUAU_PRINTF_ATTR(2, 3)
    {
        va_list args;
        va_start(args, fmt);
        Luau::vformatAppend(text, fmt, args);
        va_end(args);
    }

    void formatAppendWithPrefix(const char* fmt, ...) LUAU_PRINTF_ATTR(2, 3)
    {
        if (options.includeIrPrefix == IncludeIrPrefix::Yes)
            append("# ");

        va_list args;
        va_start(args, fmt);
        Luau::vformatAppend(text, fmt, args);
        va_end(args);
    }

    void vformatAppend(const char* fmt, va_list args)
    {
        Luau::vformatAppend(text, fmt, args);
    }

    void append(const char* str)
    {
        text.append(str);
    }

    AssemblyOptions& options;

    std::string text;
};

} // namespace CodeGen
} // namespace Luau
