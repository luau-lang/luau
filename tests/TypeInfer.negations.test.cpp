// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Fixture.h"

#include "Luau/ToString.h"
#include "doctest.h"
#include "Luau/Common.h"
#include "ScopedFlags.h"

LUAU_FASTFLAG(LuauTypeNegationSyntax)
LUAU_FASTFLAG(LuauTypeNegationSupport)
LUAU_FASTFLAG(LuauSolverV2)

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

TEST_CASE_FIXTURE(NegationFixture, "truthy_type")
{
    ScopedFastFlag _[] = {
        {FFlag::LuauTypeNegationSyntax, true},
        {FFlag::LuauTypeNegationSupport, true},
        {FFlag::LuauSolverV2, true}
    };

    CheckResult result = check(R"(
        type truthy = ~(false?)
        local w: truthy = true
        local x: truthy = false
        local y: truthy = nil
        local z: truthy = 0
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ(result.errors[0].location.begin.line, 3);
    CHECK_EQ(result.errors[1].location.begin.line, 4);
}

TEST_CASE_FIXTURE(NegationFixture, "tight_binding")
{
    ScopedFastFlag _[] = {
        {FFlag::LuauTypeNegationSyntax, true},
        {FFlag::LuauTypeNegationSupport, true},
        {FFlag::LuauSolverV2, true}
    };

    CheckResult result = check(R"(
        type V = ~boolean | false
        local x: V = false
        local y: V = 42
        local z: V = true
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(result.errors[0].location.begin.line, 4);
}

TEST_CASE_FIXTURE(NegationFixture, "exclusion_basis_is_unknown")
{
    ScopedFastFlag _[] = {
        {FFlag::LuauTypeNegationSyntax, true},
        {FFlag::LuauTypeNegationSupport, true},
        {FFlag::LuauSolverV2, true}
    };

    CheckResult result = check(R"(
        type T = ~"a"
        local b: unknown & ~"a"
        local y: T = b
        local z: T = "a"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(result.errors[0].location.begin.line, 4);
}

TEST_CASE_FIXTURE(NegationFixture, "double_negation")
{
    ScopedFastFlag _[] = {
        {FFlag::LuauTypeNegationSyntax, true},
        {FFlag::LuauTypeNegationSupport, true}
    };

    CheckResult result = check(R"(
        type T = ~~any
        local a: any
        local x: T = a
    )");

    LUAU_REQUIRE_ERROR_COUNT(0, result);
}

TEST_SUITE_END();
