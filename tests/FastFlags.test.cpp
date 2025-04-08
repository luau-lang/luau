// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"

#include "luacommon.h"

#include "doctest.h"
#include "ScopedFlags.h"

LUAU_FASTFLAGVARIABLE(LuauTestingBoolFlag);
LUAU_FASTINTVARIABLE(LuauTestingIntFlag, 123);

using namespace Luau;

TEST_SUITE_BEGIN("FastFlagsTest");

TEST_CASE("set_fflag")
{
    // Set a boolean flag
    luau_setfflag("LuauTestingBoolFlag", true);
    CHECK_EQ(FFlag::LuauTestingBoolFlag, true);

    // Set a non-existent flag
    int result = luau_setfflag("NonExistentFlag", 1);
    CHECK_EQ(result, 0); // Expect 0 for non-existent flag

    // Bool and int flags are not mixed
    result = luau_setfflag("LuauTestingIntFlag", 0);
    CHECK_EQ(result, 0); // Expect 0 for non-existent (boolean) flag
    CHECK_EQ(FInt::LuauTestingIntFlag, 123); // Should be unchanged
}

TEST_SUITE_END();
