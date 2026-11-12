// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TableFormat.h"
#include "Luau/Common.h"

#include <string>

#include <string.h>

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* Data, size_t Size)
{
    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
    {
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = true;
    }

    Luau::TableFormat::ReadResult result = Luau::TableFormat::read(std::string_view(reinterpret_cast<const char*>(Data), Size));
    LUAU_ASSERT(result.table.has_value() == result.errors.empty());

    if (!result.table)
        return 0;

    // Anything that can be read can be written, and writing what was written reads back identically.
    Luau::TableFormat::WriteResult written = Luau::TableFormat::write(*result.table);
    LUAU_ASSERT(written.source);

    Luau::TableFormat::ReadResult reread = Luau::TableFormat::read(*written.source);
    LUAU_ASSERT(reread.table);

    Luau::TableFormat::WriteResult rewritten = Luau::TableFormat::write(*reread.table);
    LUAU_ASSERT(rewritten.source == written.source);
    return 0;
}
