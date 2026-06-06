// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "lua.h"

#ifndef LCOMMIT_CPP_COMMIT_HASH
#define LCOMMIT_CPP_COMMIT_HASH "unknown"
#endif

#ifndef LCOMMIT_CPP_COMMIT_TAG
#define LCOMMIT_CPP_COMMIT_TAG "unknown"
#endif

const char* luau_getcommithash()
{
    return LCOMMIT_CPP_COMMIT_HASH;
}

const char* luau_getcommittag()
{
    return LCOMMIT_CPP_COMMIT_TAG;
}
