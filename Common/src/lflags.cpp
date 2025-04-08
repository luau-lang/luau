// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "luacommon.h"

#include "Luau/Common.h"

#include <string.h>

int luau_setfflag(const char* name, int value)
{
    for (Luau::FValue<bool>* flag = Luau::FValue<bool>::list; flag; flag = flag->next)
    {
        if (strcmp(flag->name, name) == 0)
        {
            flag->value = value;
            return 1;
        }
    }
    return 0;
}
