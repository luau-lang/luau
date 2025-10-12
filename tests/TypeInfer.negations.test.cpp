// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "Luau/ToString.h"
#include "doctest.h"
#include "Luau/Common.h"
#include "ScopedFlags.h"

LUAU_FASTFLAG(LuauParseNegationType);
LUAU_FASTFLAG(LuauNegationTypes)

using namespace Luau;

namespace
{

struct NegationFixture : Fixture
{
    TypeArena arena;

    NegationFixture()
    {
        registerHiddenTypes(getFrontend());
    }
};

} // namespace

TEST_SUITE_BEGIN("Negations");

TEST_CASE_FIXTURE(NegationFixture, "negated_string_is_a_subtype_of_string")
{
    ScopedFastFlag sffs[]{
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauParseNegationType, true},
        {FFlag::LuauNegationTypes, true},
    };

    CheckResult result = check(R"(
        function foo(arg: string) end
        local a: string & ~"Hello"
        foo(a)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NegationFixture, "string_is_not_a_subtype_of_negated_string")
{
    ScopedFastFlag sffs[]{
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauParseNegationType, true},
        {FFlag::LuauNegationTypes, true},
    };

    CheckResult result = check(R"(
        function foo(arg: string & ~"hello") end
        local a: string
        foo(a)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "cofinite_strings_can_be_compared_for_equality")
{
    CheckResult result = check(R"(
        function f(e)
            if e == 'strictEqual' then
                e = 'strictEqualObject'
            end
            if e == 'deepStrictEqual' or e == 'strictEqual' then
            elseif e == 'notDeepStrictEqual' or e == 'notStrictEqual' then
            end
            return e
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("(string) -> string" == toString(requireType("f")));
}

TEST_CASE_FIXTURE(NegationFixture, "compare_cofinite_strings")
{
    ScopedFastFlag sffs[]{
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauParseNegationType, true},
        {FFlag::LuauNegationTypes, true},
    };

    CheckResult result = check(R"(
local u : ~"a"
local v : "b"
if u == v then
end
)");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NegationFixture, "cannot_negate_non_testables")
{
    ScopedFastFlag sffs[]{
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauParseNegationType, true},
        {FFlag::LuauNegationTypes, true},
    };

    CheckResult result = check(R"(
local t: ~{}
local f: ~(string) -> string
)");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
}

TEST_SUITE_END();
