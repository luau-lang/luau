// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>

#define LUAI_MAXNUM2STR 48

char* luai_num2str(char* buf, double n);

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* Data, size_t Size)
{
    if (Size < 8)
        return 0;

    double num;
    memcpy(&num, Data, 8);

    char buf[LUAI_MAXNUM2STR];
    char* end = luai_num2str(buf, num);
    LUAU_ASSERT(end < buf + sizeof(buf));

    *end = 0;

    double rec = strtod(buf, nullptr);

    LUAU_ASSERT(rec == num || (rec != rec && num != num));
    return 0;
}
