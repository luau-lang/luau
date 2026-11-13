// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TableFormat.h"
#include "Luau/Common.h"

#include <stdint.h>
#include <string.h>

using namespace Luau::TableFormat;

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* Data, size_t Size)
{
    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
    {
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = true;
    }

    ReadResult result = read(std::string_view(reinterpret_cast<const char*>(Data), Size));
    LUAU_ASSERT(result.table.has_value() == result.errors.empty());

    if (!result.table)
        return 0;

    // Anything that can be read can be written, and writing what was written reads back identically.
    WriteResult written = write(*result.table);
    LUAU_ASSERT(written.source);

    ReadResult reread = read(*written.source);
    LUAU_ASSERT(reread.table);

    WriteResult rewritten = write(*reread.table);
    LUAU_ASSERT(rewritten.source == written.source);
    return 0;
}
