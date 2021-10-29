// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"

#include <stdint.h>
#include <vector>

namespace Luau
{
void fuzzFormatString(const char* data, size_t size);
} // namespace Luau

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* Data, size_t Size)
{
    // copy data to heap to make sure ASAN can catch out of bounds access
    std::vector<char> str(Data, Data + Size);

    Luau::fuzzFormatString(str.data(), str.size());

    return 0;
}
