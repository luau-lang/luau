// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"

#include <vector>

#include <stddef.h>
#include <stdint.h>
#include <string.h>

namespace Luau
{
void fuzzFormatString(const char* data, size_t size);
} // namespace Luau

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* Data, size_t Size)
{
    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
    {
        if (strncmp(flag->name, "Luau", 4) == 0)
            flag->value = true;
    }

    // copy data to heap to make sure ASAN can catch out of bounds access
    std::vector<char> str(Data, Data + Size);

    Luau::fuzzFormatString(str.data(), str.size());

    return 0;
}
