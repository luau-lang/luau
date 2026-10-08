// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Common.h"


#include "ScopedFlags.h"
#include "ReplWithPathFixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(DebugLuauUserDefinedClasses)
LUAU_FASTFLAG(DebugLuauUserDefinedClassesRuntime)
LUAU_FASTFLAG(LuauCallFeedback)
LUAU_FASTFLAG(LuauEmitCallFeedback)
LUAU_FASTFLAG(LuauBytecodeCostModel)

TEST_SUITE_BEGIN("ClassRuntimeErrorTests");

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireClassOverrideInstanceMemberError")
{
    ScopedFastFlag sffs[] = {
        {FFlag::DebugLuauUserDefinedClasses, true},
        {FFlag::DebugLuauUserDefinedClassesRuntime, true},
        {FFlag::LuauCallFeedback, true},
        {FFlag::LuauEmitCallFeedback, true},
        {FFlag::LuauBytecodeCostModel, true}
    };
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/classes/class_override_instance_member_error";
    runProtectedRequire(path);
    assertOutputContainsAll({"Cannot override instance member 'x' of parent class 'Parent' in child class 'Child'"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireClassExtendsNonOpenParent")
{
    ScopedFastFlag sffs[] = {
        {FFlag::DebugLuauUserDefinedClasses, true},
        {FFlag::DebugLuauUserDefinedClassesRuntime, true},
        {FFlag::LuauCallFeedback, true},
        {FFlag::LuauEmitCallFeedback, true},
        {FFlag::LuauBytecodeCostModel, true}
    };
    std::string path = getLuauDirectory(PathType::Relative) + "/tests/classes/class_extends_non_open_parent";
    runProtectedRequire(path);
    assertOutputContainsAll({"Non-open class 'Parent' cannot be extended"});
}

TEST_CASE_FIXTURE(ReplWithPathFixture, "RequireClassOverridesComparisonMetamethods")
{
    ScopedFastFlag sffs[] = {
        {FFlag::DebugLuauUserDefinedClasses, true},
        {FFlag::DebugLuauUserDefinedClassesRuntime, true},
        {FFlag::LuauCallFeedback, true},
        {FFlag::LuauEmitCallFeedback, true},
        {FFlag::LuauBytecodeCostModel, true}
    };

    std::string path = getLuauDirectory(PathType::Relative) + "/tests/classes/class_override_eq";
    runProtectedRequire(path);
    assertOutputContainsAll({"Overriding comparison metamethods is not allowed ('__eq' in class 'Child')"});

    path = getLuauDirectory(PathType::Relative) + "/tests/classes/class_override_lt";
    runProtectedRequire(path);
    assertOutputContainsAll({"Overriding comparison metamethods is not allowed ('__lt' in class 'Child')"});

    path = getLuauDirectory(PathType::Relative) + "/tests/classes/class_override_le";
    runProtectedRequire(path);
    assertOutputContainsAll({"Overriding comparison metamethods is not allowed ('__le' in class 'Child')"});
}

TEST_SUITE_END();
