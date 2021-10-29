// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Parser.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("TypePackTests");

TEST_CASE_FIXTURE(Fixture, "infer_multi_return")
{
    CheckResult result = check(R"(
        function take_two()
            return 2, 2
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionTypeVar* takeTwoType = get<FunctionTypeVar>(requireType("take_two"));
    REQUIRE(takeTwoType != nullptr);

    const auto& [returns, tail] = flatten(takeTwoType->retType);

    CHECK_EQ(2, returns.size());
    CHECK_EQ(typeChecker.numberType, returns[0]);
    CHECK_EQ(typeChecker.numberType, returns[1]);

    CHECK(!tail);
}

TEST_CASE_FIXTURE(Fixture, "empty_varargs_should_return_nil_when_not_in_tail_position")
{
    CheckResult result = check(R"(
        local a, b = ..., 1
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "self_and_varargs_should_work")
{
    CheckResult result = check(R"(
        local t = {}
        function t:f(...) end
        t:f(1)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "last_element_of_return_statement_can_itself_be_a_pack")
{
    CheckResult result = check(R"(
        function take_two()
            return 2, 2
        end

        function take_three()
            return 1, take_two()
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);

    const FunctionTypeVar* takeOneMoreType = get<FunctionTypeVar>(requireType("take_three"));
    REQUIRE(takeOneMoreType != nullptr);

    const auto& [rets, tail] = flatten(takeOneMoreType->retType);

    REQUIRE_EQ(3, rets.size());
    CHECK_EQ(typeChecker.numberType, rets[0]);
    CHECK_EQ(typeChecker.numberType, rets[1]);
    CHECK_EQ(typeChecker.numberType, rets[2]);

    CHECK(!tail);
}

TEST_CASE_FIXTURE(Fixture, "higher_order_function")
{
    CheckResult result = check(R"(
        function apply(f, g, x)
            return f(g(x))
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionTypeVar* applyType = get<FunctionTypeVar>(requireType("apply"));
    REQUIRE(applyType != nullptr);

    std::vector<TypeId> applyArgs = flatten(applyType->argTypes).first;
    REQUIRE_EQ(3, applyArgs.size());

    const FunctionTypeVar* fType = get<FunctionTypeVar>(applyArgs[0]);
    REQUIRE(fType != nullptr);

    const FunctionTypeVar* gType = get<FunctionTypeVar>(applyArgs[1]);
    REQUIRE(gType != nullptr);

    std::vector<TypeId> gArgs = flatten(gType->argTypes).first;
    REQUIRE_EQ(1, gArgs.size());

    // function(function(t1, T2...): (t3, T4...), function(t5): (t1, T2...), t5): (t3, T4...)

    REQUIRE_EQ(*gArgs[0], *applyArgs[2]);
    REQUIRE_EQ(toString(fType->argTypes), toString(gType->retType));
    REQUIRE_EQ(toString(fType->retType), toString(applyType->retType));
}

TEST_CASE_FIXTURE(Fixture, "return_type_should_be_empty_if_nothing_is_returned")
{
    CheckResult result = check(R"(
        function f() end
        function g() return end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    const FunctionTypeVar* fTy = get<FunctionTypeVar>(requireType("f"));
    REQUIRE(fTy != nullptr);
    CHECK_EQ(0, size(fTy->retType));
    const FunctionTypeVar* gTy = get<FunctionTypeVar>(requireType("g"));
    REQUIRE(gTy != nullptr);
    CHECK_EQ(0, size(gTy->retType));
}

TEST_CASE_FIXTURE(Fixture, "no_return_size_should_be_zero")
{
    CheckResult result = check(R"(
        function f(a:any) return a end
        function g() return end
        function h() end

        g(h())
        f(g(),h())
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionTypeVar* fTy = get<FunctionTypeVar>(requireType("f"));
    REQUIRE(fTy != nullptr);
    CHECK_EQ(1, size(follow(fTy->retType)));

    const FunctionTypeVar* gTy = get<FunctionTypeVar>(requireType("g"));
    REQUIRE(gTy != nullptr);
    CHECK_EQ(0, size(gTy->retType));

    const FunctionTypeVar* hTy = get<FunctionTypeVar>(requireType("h"));
    REQUIRE(hTy != nullptr);
    CHECK_EQ(0, size(hTy->retType));
}

TEST_CASE_FIXTURE(Fixture, "varargs_inference_through_multiple_scopes")
{
    CheckResult result = check(R"(
        local function f(...)
            do
                local a: string = ...
                local b: number = ...
            end
        end

        f("foo")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "multiple_varargs_inference_are_not_confused")
{
    CheckResult result = check(R"(
        local function f(...)
            local a: string = ...

            return function(...)
                local b: number = ...
            end
        end

        f("foo", "bar")(1, 2)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "parenthesized_varargs_returns_any")
{
    CheckResult result = check(R"(
        --!strict
        local value

        local function f(...)
            value = ...
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("any", toString(requireType("value")));
}

TEST_CASE_FIXTURE(Fixture, "variadic_packs")
{
    TypeArena& arena = typeChecker.globalTypes;

    unfreeze(arena);

    TypePackId listOfNumbers = arena.addTypePack(TypePackVar{VariadicTypePack{typeChecker.numberType}});
    TypePackId listOfStrings = arena.addTypePack(TypePackVar{VariadicTypePack{typeChecker.stringType}});

    // clang-format off
    addGlobalBinding(typeChecker, "foo",
        arena.addType(
            FunctionTypeVar{
                listOfNumbers,
                arena.addTypePack({typeChecker.numberType})
            }
        ),
        "@test"
    );
    addGlobalBinding(typeChecker, "bar", 
        arena.addType(
            FunctionTypeVar{
                arena.addTypePack({{typeChecker.numberType}, listOfStrings}),
                arena.addTypePack({typeChecker.numberType})
            }
        ),
        "@test"
    );
    // clang-format on

    freeze(arena);

    CheckResult result = check(R"(
        --!strict

        foo(1, 2, 3, "foo")
        bar(1, "foo", "bar", 3)
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK_EQ(result.errors[0], (TypeError{Location(Position{3, 21}, Position{3, 26}), TypeMismatch{typeChecker.numberType, typeChecker.stringType}}));

    CHECK_EQ(result.errors[1], (TypeError{Location(Position{4, 29}, Position{4, 30}), TypeMismatch{typeChecker.stringType, typeChecker.numberType}}));
}

TEST_CASE_FIXTURE(Fixture, "variadic_pack_syntax")
{
    CheckResult result = check(R"(
        --!strict

        local function foo(...: number)
        end

        foo(1, 2, 3, 4, 5, 6)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(toString(requireType("foo")), "(...number) -> ()");
}

// CLI-45791
TEST_CASE_FIXTURE(UnfrozenFixture, "type_pack_hidden_free_tail_infinite_growth")
{
    CheckResult result = check(R"(
--!nonstrict
if _ then
    _[function(l0)end],l0 = _
elseif _ then
    return l0(nil)
elseif 1 / l0(nil) then
elseif _ then
    return #_,l0()
end
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "variadic_argument_tail")
{
    CheckResult result = check(R"(
local _ = function():((...any)->(...any),()->())
	return function() end, function() end
end
for y in _() do
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
