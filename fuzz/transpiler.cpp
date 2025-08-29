// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Transpiler.h"
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

    Luau::transpile(std::string_view(reinterpret_cast<const char*>(Data), Size));
    return 0;
}
