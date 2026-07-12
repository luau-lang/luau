// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "Luau/ToString.h"
#include "doctest.h"
#include "Luau/Common.h"
#include "ScopedFlags.h"

LUAU_FASTFLAG(LuauNegationsFixSubtypePath)

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
    CheckResult result = check(R"(
        function foo(arg: string) end
        local a: string & Not<"Hello">
        foo(a)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NegationFixture, "string_is_not_a_subtype_of_negated_string")
{
    CheckResult result = check(R"(
        function foo(arg: string & Not<"hello">) end
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
    CheckResult result = check(R"(
local u : Not<"a">
local v : "b"
if u == v then
end
)");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NegationFixture, "subtype_path_is_valid_for_unions")
{
    ScopedFastFlag fixSubtypePath{FFlag::LuauNegationsFixSubtypePath, true};

    CheckResult result = check(R"(
        type T = Not<false?>
        local x: T = false :: false
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(
        "Expected this to be '~(false?)', but got 'false'; \n"
            "the negation `~(false?)`, and `false` is not a subtype of `~(false?)`",
        toString(result.errors[0])
    );
}

TEST_CASE_FIXTURE(NegationFixture, "subtype_path_is_valid_for_intersections")
{
    ScopedFastFlag fixSubtypePath{FFlag::LuauNegationsFixSubtypePath, true};

    CheckResult result = check(R"(
        type T = Not<unknown & boolean>
        local x: T = false
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(
        "Expected this to be '~(boolean & unknown)', but got 'boolean'; \n"
            "the negation `~(boolean & unknown)`, and `boolean` is not a subtype of `~(boolean & unknown)`",
        toString(result.errors[0])
    );
}

TEST_SUITE_END();
