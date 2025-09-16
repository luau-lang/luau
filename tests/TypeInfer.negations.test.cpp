// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "Luau/ToString.h"
#include "doctest.h"
#include "Luau/Common.h"
#include "ScopedFlags.h"

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

TEST_SUITE_END();
