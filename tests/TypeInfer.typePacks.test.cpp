// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)

LUAU_FASTFLAG(LuauInstantiateInSubtyping)
LUAU_FASTFLAG(LuauRemoveGenericErrorForParams)
LUAU_FASTFLAG(LuauAddErrorCaseForIncompatibleTypePacks)

TEST_SUITE_BEGIN("TypePackTests");

TEST_CASE_FIXTURE(Fixture, "infer_multi_return")
{
    CheckResult result = check(R"(
        function take_two()
            return 2, 2
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionType* takeTwoType = get<FunctionType>(requireType("take_two"));
    REQUIRE(takeTwoType != nullptr);

    const auto& [returns, tail] = flatten(takeTwoType->retTypes);

    CHECK_EQ(2, returns.size());
    CHECK_EQ(getBuiltins()->numberType, follow(returns[0]));
    CHECK_EQ(getBuiltins()->numberType, follow(returns[1]));

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

    const FunctionType* takeOneMoreType = get<FunctionType>(requireType("take_three"));
    REQUIRE(takeOneMoreType != nullptr);

    const auto& [rets, tail] = flatten(takeOneMoreType->retTypes);

    REQUIRE_EQ(3, rets.size());
    CHECK_EQ(getBuiltins()->numberType, follow(rets[0]));
    CHECK_EQ(getBuiltins()->numberType, follow(rets[1]));
    CHECK_EQ(getBuiltins()->numberType, follow(rets[2]));

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

    if (FFlag::LuauSolverV2)
        CHECK_EQ("<a, b..., c...>((c...) -> (b...), (a) -> (c...), a) -> (b...)", toString(requireType("apply")));
    else
        CHECK_EQ("<a, b..., c...>((b...) -> (c...), (a) -> (b...), a) -> (c...)", toString(requireType("apply")));
}

TEST_CASE_FIXTURE(Fixture, "return_type_should_be_empty_if_nothing_is_returned")
{
    CheckResult result = check(R"(
        function f() end
        function g() return end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    const FunctionType* fTy = get<FunctionType>(requireType("f"));
    REQUIRE(fTy != nullptr);
    CHECK_EQ(0, size(fTy->retTypes));
    const FunctionType* gTy = get<FunctionType>(requireType("g"));
    REQUIRE(gTy != nullptr);
    CHECK_EQ(0, size(gTy->retTypes));
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

    const FunctionType* fTy = get<FunctionType>(requireType("f"));
    REQUIRE(fTy != nullptr);
    CHECK_EQ(1, size(follow(fTy->retTypes)));

    const FunctionType* gTy = get<FunctionType>(requireType("g"));
    REQUIRE(gTy != nullptr);
    CHECK_EQ(0, size(gTy->retTypes));

    const FunctionType* hTy = get<FunctionType>(requireType("h"));
    REQUIRE(hTy != nullptr);
    CHECK_EQ(0, size(hTy->retTypes));
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
    TypeArena& arena = getFrontend().globals.globalTypes;

    unfreeze(arena);

    TypePackId listOfNumbers = arena.addTypePack(TypePackVar{VariadicTypePack{getBuiltins()->numberType}});
    TypePackId listOfStrings = arena.addTypePack(TypePackVar{VariadicTypePack{getBuiltins()->stringType}});

    // clang-format off
    addGlobalBinding(getFrontend().globals, "foo",
        arena.addType(
            FunctionType{
                listOfNumbers,
                arena.addTypePack({getBuiltins()->numberType})
            }
        ),
        "@test"
    );
    addGlobalBinding(getFrontend().globals, "bar",
        arena.addType(
            FunctionType{
                arena.addTypePack({{getBuiltins()->numberType}, listOfStrings}),
                arena.addTypePack({getBuiltins()->numberType})
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

    CHECK(Location{Position{3, 21}, Position{3, 26}} == result.errors[0].location);
    CHECK(Location{Position{4, 29}, Position{4, 30}} == result.errors[1].location);

    CHECK_EQ(
        result.errors[0], (TypeError{Location(Position{3, 21}, Position{3, 26}), TypeMismatch{getBuiltins()->numberType, getBuiltins()->stringType}})
    );

    CHECK_EQ(
        result.errors[1], (TypeError{Location(Position{4, 29}, Position{4, 30}), TypeMismatch{getBuiltins()->stringType, getBuiltins()->numberType}})
    );
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

#if 0
TEST_CASE_FIXTURE(Fixture, "type_pack_hidden_free_tail_infinite_growth")
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
#endif

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

TEST_CASE_FIXTURE(Fixture, "type_alias_type_packs")
{
    CheckResult result = check(R"(
type Packed<T...> = (T...) -> T...
local a: Packed<>
local b: Packed<number>
local c: Packed<string, number>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    auto tf = lookupType("Packed");
    REQUIRE(tf);
    CHECK_EQ(toString(*tf), "(T...) -> (T...)");
    CHECK_EQ(toString(requireType("a")), "() -> ()");
    CHECK_EQ(toString(requireType("b")), "(number) -> number");
    CHECK_EQ(toString(requireType("c")), "(string, number) -> (string, number)");

    result = check(R"(
-- (U..., T) cannot be parsed right now
type Packed<T, U...> = { f: (a: T, U...) -> (T, U...) }
local a: Packed<number>
local b: Packed<string, number>
local c: Packed<string, number, boolean>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    tf = lookupType("Packed");
    REQUIRE(tf);
    CHECK_EQ(toString(*tf), "Packed<T, U...>");
    CHECK_EQ(toString(*tf, {true}), "{ f: (T, U...) -> (T, U...) }");

    auto ttvA = get<TableType>(requireType("a"));
    REQUIRE(ttvA);
    CHECK_EQ(toString(requireType("a")), "Packed<number>");
    CHECK_EQ(toString(requireType("a"), {true}), "{ f: (number) -> number }");

    REQUIRE(ttvA->instantiatedTypeParams.size() == 1);
    REQUIRE(ttvA->instantiatedTypePackParams.size() == 1);
    CHECK_EQ(toString(ttvA->instantiatedTypeParams[0], {true}), "number");
    CHECK_EQ(toString(ttvA->instantiatedTypePackParams[0], {true}), "()");

    auto ttvB = get<TableType>(requireType("b"));
    REQUIRE(ttvB);
    CHECK_EQ(toString(requireType("b")), "Packed<string, number>");
    CHECK_EQ(toString(requireType("b"), {true}), "{ f: (string, number) -> (string, number) }");

    REQUIRE(ttvB->instantiatedTypeParams.size() == 1);
    REQUIRE(ttvB->instantiatedTypePackParams.size() == 1);
    CHECK_EQ(toString(ttvB->instantiatedTypeParams[0], {true}), "string");
    CHECK_EQ(toString(ttvB->instantiatedTypePackParams[0], {true}), "number");

    auto ttvC = get<TableType>(requireType("c"));
    REQUIRE(ttvC);
    CHECK_EQ(toString(requireType("c")), "Packed<string, number, boolean>");
    CHECK_EQ(toString(requireType("c"), {true}), "{ f: (string, number, boolean) -> (string, number, boolean) }");

    REQUIRE(ttvC->instantiatedTypeParams.size() == 1);
    REQUIRE(ttvC->instantiatedTypePackParams.size() == 1);
    CHECK_EQ(toString(ttvC->instantiatedTypeParams[0], {true}), "string");
    CHECK_EQ(toString(ttvC->instantiatedTypePackParams[0], {true}), "number, boolean");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_alias_type_packs_import")
{
    fileResolver.source["game/A"] = R"(
export type Packed<T, U...> = { a: T, b: (U...) -> () }
return {}
    )";

    CheckResult aResult = getFrontend().check("game/A");
    LUAU_REQUIRE_NO_ERRORS(aResult);

    CheckResult bResult = check(R"(
local Import = require(game.A)
local a: Import.Packed<number>
local b: Import.Packed<string, number>
local c: Import.Packed<string, number, boolean>
local d: { a: typeof(c) }
    )");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    auto tf = lookupImportedType("Import", "Packed");
    REQUIRE(tf);
    CHECK_EQ(toString(*tf), "Packed<T, U...>");

    CHECK_EQ(toString(*tf, {true}), "{ a: T, b: (U...) -> () }");

    CHECK_EQ(toString(requireType("a"), {true}), "{ a: number, b: () -> () }");
    CHECK_EQ(toString(requireType("b"), {true}), "{ a: string, b: (number) -> () }");
    CHECK_EQ(toString(requireType("c"), {true}), "{ a: string, b: (number, boolean) -> () }");
    CHECK_EQ(toString(requireType("d")), "{ a: Packed<string, number, boolean> }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_pack_type_parameters")
{
    fileResolver.source["game/A"] = R"(
export type Packed<T, U...> = { a: T, b: (U...) -> () }
return {}
    )";

    CheckResult cResult = check(R"(
local Import = require(game.A)
type Alias<S, T, R...> = Import.Packed<S, (T, R...)>
local a: Alias<string, number, boolean>

type B<X...> = Import.Packed<string, X...>
type C<X...> = Import.Packed<string, (number, X...)>
    )");
    LUAU_REQUIRE_NO_ERRORS(cResult);

    auto tf = lookupType("Alias");
    REQUIRE(tf);
    CHECK_EQ(toString(*tf), "Alias<S, T, R...>");
    CHECK_EQ(toString(*tf, {true}), "{ a: S, b: (T, R...) -> () }");

    CHECK_EQ(toString(requireType("a"), {true}), "{ a: string, b: (number, boolean) -> () }");

    tf = lookupType("B");
    REQUIRE(tf);
    CHECK_EQ(toString(*tf), "B<X...>");
    CHECK_EQ(toString(*tf, {true}), "{ a: string, b: (X...) -> () }");

    tf = lookupType("C");
    REQUIRE(tf);
    CHECK_EQ(toString(*tf), "C<X...>");
    CHECK_EQ(toString(*tf, {true}), "{ a: string, b: (number, X...) -> () }");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_type_packs_nested")
{
    CheckResult result = check(R"(
type Packed1<T...> = (T...) -> (T...)
type Packed2<T...> = (Packed1<T...>, T...) -> (Packed1<T...>, T...)
type Packed3<T...> = (Packed2<T...>, T...) -> (Packed2<T...>, T...)
type Packed4<T...> = (Packed3<T...>, T...) -> (Packed3<T...>, T...)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    auto tf = lookupType("Packed4");
    REQUIRE(tf);
    CHECK_EQ(
        toString(*tf),
        "((((T...) -> (T...), T...) -> ((T...) -> (T...), T...), T...) -> (((T...) -> (T...), T...) -> ((T...) -> (T...), T...), T...), T...) -> "
        "((((T...) -> (T...), T...) -> ((T...) -> (T...), T...), T...) -> (((T...) -> (T...), T...) -> ((T...) -> (T...), T...), T...), T...)"
    );
}

TEST_CASE_FIXTURE(Fixture, "type_alias_type_pack_variadic")
{
    CheckResult result = check(R"(
type X<T...> = (T...) -> (string, T...)

type D = X<...number>
type E = X<(number, ...string)>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    auto d = lookupType("D");
    REQUIRE(d);
    auto e = lookupType("E");
    REQUIRE(e);
    CHECK_EQ(toString(*d), "(...number) -> (string, ...number)");
    CHECK_EQ(toString(*e), "(number, ...string) -> (string, number, ...string)");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_type_pack_multi")
{
    CheckResult result = check(R"(
type Y<T..., U...> = (T...) -> (U...)
type A<S...> = Y<S..., S...>
type B<S...> = Y<(number, ...string), S...>

type Z<T, U...> = (T) -> (U...)
type E<S...> = Z<number, S...>
type F<S...> = Z<number, (string, S...)>

type W<T, U..., V...> = (T, U...) -> (T, V...)
type H<S..., R...> = W<number, S..., R...>
type I<S..., R...> = W<number, (string, S...), R...>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(*lookupType("A")), "(S...) -> (S...)");
    CHECK_EQ(toString(*lookupType("B")), "(number, ...string) -> (S...)");

    CHECK_EQ(toString(*lookupType("E")), "(number) -> (S...)");
    CHECK_EQ(toString(*lookupType("F")), "(number) -> (string, S...)");

    CHECK_EQ(toString(*lookupType("H")), "(number, S...) -> (number, R...)");
    CHECK_EQ(toString(*lookupType("I")), "(number, string, S...) -> (number, R...)");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_type_pack_explicit")
{
    CheckResult result = check(R"(
type X<T...> = (T...) -> (T...)

type A<S...> = X<(S...)>
type B = X<()>
type C = X<(number)>
type D = X<(number, string)>
type E = X<(...number)>
type F = X<(string, ...number)>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(*lookupType("A")), "(S...) -> (S...)");
    CHECK_EQ(toString(*lookupType("B")), "() -> ()");
    CHECK_EQ(toString(*lookupType("C")), "(number) -> number");
    CHECK_EQ(toString(*lookupType("D")), "(number, string) -> (number, string)");
    CHECK_EQ(toString(*lookupType("E")), "(...number) -> (...number)");
    CHECK_EQ(toString(*lookupType("F")), "(string, ...number) -> (string, ...number)");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_type_pack_explicit_multi")
{
    CheckResult result = check(R"(
type Y<T..., U...> = (T...) -> (U...)

type A = Y<(number, string), (boolean)>
type B = Y<(), ()>
type C<S...> = Y<...string, (number, S...)>
type D<X...> = Y<X..., (number, string, X...)>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(*lookupType("A")), "(number, string) -> boolean");
    CHECK_EQ(toString(*lookupType("B")), "() -> ()");
    CHECK_EQ(toString(*lookupType("C")), "(...string) -> (number, S...)");
    CHECK_EQ(toString(*lookupType("D")), "(X...) -> (number, string, X...)");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_type_pack_explicit_multi_tostring")
{
    CheckResult result = check(R"(
type Y<T..., U...> = { f: (T...) -> (U...) }

local a: Y<(number, string), (boolean)>
local b: Y<(), ()>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireType("a")), "Y<(number, string), (boolean)>");
    CHECK_EQ(toString(requireType("b")), "Y<(), ()>");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_backwards_compatible")
{
    CheckResult result = check(R"(
        type X<T> = () -> T
        type Y<T, U> = (T) -> U

        type A = X<(number)>
        type B = Y<(number), (boolean)>
        type C = Y<(number), boolean>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(*lookupType("A")), "() -> number");
    CHECK_EQ(toString(*lookupType("B")), "(number) -> boolean");
    CHECK_EQ(toString(*lookupType("C")), "(number) -> boolean");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_type_packs_errors")
{
    CheckResult result = check(R"(
type Packed<T, U, V...> = (T, U) -> (V...)
local b: Packed<number>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), "Generic type 'Packed<T, U, V...>' expects at least 2 type arguments, but only 1 is specified");

    result = check(R"(
type Packed<T, U> = (T, U) -> ()
type B<X...> = Packed<number, string, X...>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), "Generic type 'Packed<T, U>' expects 0 type pack arguments, but 1 is specified");

    result = check(R"(
type Packed<T..., U...> = (T...) -> (U...)
type Other<S...> = Packed<S..., string>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), "Type parameters must come before type pack parameters");

    result = check(R"(
type Packed<T, U> = (T) -> U
type Other<S...> = Packed<number, S...>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), "Generic type 'Packed<T, U>' expects 2 type arguments, but only 1 is specified");

    result = check(R"(
type Packed<T..., U...> = (T...) -> (U...)
type Other = Packed<>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), "Generic type 'Packed<T..., U...>' expects 2 type pack arguments, but none are specified");

    result = check(R"(
type Packed<T..., U...> = (T...) -> (U...)
type Other = Packed<number, string>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), "Generic type 'Packed<T..., U...>' expects 2 type pack arguments, but only 1 is specified");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_instantiated_but_missing_parameter_list")
{
    ScopedFastFlag sff{FFlag::LuauRemoveGenericErrorForParams, true};

    CheckResult result = check(R"(
type Packed<T...> = (T...) -> T...
local a: Packed
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    if (FFlag::LuauSolverV2)
        CHECK_EQ(toString(result.errors[0]), "Generic type 'Packed<T...>' expects 1 type pack argument, but none are specified");
    else
        CHECK_EQ(toString(result.errors[0]), "Type parameter list is required");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_default_type_explicit")
{
    CheckResult result = check(R"(
type Y<T, U = string> = { a: T, b: U }

local a: Y<number, number> = { a = 2, b = 3 }
local b: Y<number> = { a = 2, b = "s" }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireType("a")), "Y<number, number>");
    CHECK_EQ(toString(requireType("b")), "Y<number, string>");

    result = check(R"(
type Y<T = string> = { a: T }

local a: Y<number> = { a = 2 }
local b: Y<> = { a = "s" }
local c: Y = { a = "s" }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireType("a")), "Y<number>");
    CHECK_EQ(toString(requireType("b")), "Y<string>");
    CHECK_EQ(toString(requireType("c")), "Y<string>");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_default_type_self")
{
    CheckResult result = check(R"(
type Y<T, U = T> = { a: T, b: U }

local a: Y<number> = { a = 2, b = 3 }
local b: Y<string> = { a = "h", b = "s" }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireType("a")), "Y<number, number>");
    CHECK_EQ(toString(requireType("b")), "Y<string, string>");

    result = check(R"(
type Y<T, U = (T, T) -> string> = { a: T, b: U }

local a: Y<number>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireType("a")), "Y<number, (number, number) -> string>");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_default_type_chained")
{
    CheckResult result = check(R"(
type Y<T, U = T, V = U> = { a: T, b: U, c: V }

local a: Y<number>
local b: Y<number, string>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireType("a")), "Y<number, number, number>");
    CHECK_EQ(toString(requireType("b")), "Y<number, string, string>");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_default_type_pack_explicit")
{
    CheckResult result = check(R"(
type Y<T... = (string, number)> = { a: (T...) -> () }
local a: Y<>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireType("a")), "Y<string, number>");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_default_type_pack_self_ty")
{
    CheckResult result = check(R"(
type Y<T, U... = ...T> = { a: T, b: (U...) -> T }

local a: Y<number>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireType("a")), "Y<number, ...number>");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_default_type_pack_self_tp")
{
    CheckResult result = check(R"(
type Y<T..., U... = T...> = { a: (T...) -> U... }
local a: Y<number, string>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireType("a")), "Y<(number, string), (number, string)>");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_default_type_pack_self_chained_tp")
{
    CheckResult result = check(R"(
type Y<T..., U... = T..., V... = U...> = { a: (T...) -> U..., b: (T...) -> V... }
local a: Y<number, string>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireType("a")), "Y<(number, string), (number, string), (number, string)>");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_default_mixed_self")
{
    CheckResult result = check(R"(
type Y<T, U = T, V... = ...number, W... = (T, U, V...)> = { a: (T, U, V...) -> W... }
local a: Y<number>
local b: Y<number, string>
local c: Y<number, string, ...boolean>
local d: Y<number, string, ...boolean, ...() -> ()>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireType("a")), "Y<number, number, ...number, (number, number, ...number)>");
    CHECK_EQ(toString(requireType("b")), "Y<number, string, ...number, (number, string, ...number)>");
    CHECK_EQ(toString(requireType("c")), "Y<number, string, ...boolean, (number, string, ...boolean)>");
    CHECK_EQ(toString(requireType("d")), "Y<number, string, ...boolean, ...() -> ()>");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_default_type_errors")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        type Y<T = T> = { a: T }
        local a: Y = { a = 2 }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), "Unknown type 'T'");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_default_type_errors2")
{
    CheckResult result = check(R"(
        type Y<T... = T...> = { a: (T...) -> () }
        local a: Y<>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), "Unknown type 'T'");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_default_type_errors3")
{
    ScopedFastFlag sff{FFlag::LuauAddErrorCaseForIncompatibleTypePacks, true};

    CheckResult result = check(R"(
        type Y<T = string, U... = ...string> = { a: (T) -> U... }
        local a: Y<...number>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    if (FFlag::LuauSolverV2)
        CHECK_EQ(toString(result.errors[0]), "Type parameters must come before type pack parameters");
    else
        CHECK_EQ(toString(result.errors[0]), "Generic type 'Y<T, U...>' expects at least 1 type argument, but none are specified");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_default_type_errors4")
{
    ScopedFastFlag sff{FFlag::LuauRemoveGenericErrorForParams, true};

    CheckResult result = check(R"(
        type Packed<T> = (T) -> T
        local a: Packed
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    if (FFlag::LuauSolverV2)
        CHECK_EQ(toString(result.errors[0]), "Generic type 'Packed<T>' expects 1 type argument, but none are specified");
    else
        CHECK_EQ(toString(result.errors[0]), "Type parameter list is required");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_default_type_errors5")
{
    CheckResult result = check(R"(
        type Y<T, U = T, V> = { a: T }
        local a: Y<number>
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "type_alias_default_type_errors6")
{
    CheckResult result = check(R"(
        type Y<T..., U... = T..., V...> = { a: T }
        local a: Y<...number>
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_alias_default_export")
{
    fileResolver.source["Module/Types"] = R"(
export type A<T, U = string> = { a: T, b: U }
export type B<T, U = T> = { a: T, b: U }
export type C<T, U = (T, T) -> string> = { a: T, b: U }
export type D<T, U = T, V = U> = { a: T, b: U, c: V }
export type E<T... = (string, number)> = { a: (T...) -> () }
export type F<T, U... = ...T> = { a: T, b: (U...) -> T }
export type G<T..., U... = ()> = { b: (U...) -> T... }
export type H<T... = ()> = { b: (T...) -> T... }
return {}
    )";

    CheckResult resultTypes = getFrontend().check("Module/Types");
    LUAU_REQUIRE_NO_ERRORS(resultTypes);

    fileResolver.source["Module/Users"] = R"(
local Types = require(script.Parent.Types)

local a: Types.A<number>
local b: Types.B<number>
local c: Types.C<number>
local d: Types.D<number>
local e: Types.E<>
local eVoid: Types.E<()>
local f: Types.F<number>
local g: Types.G<...number>
local h: Types.H<>
    )";

    CheckResult resultUsers = getFrontend().check("Module/Users");
    LUAU_REQUIRE_NO_ERRORS(resultUsers);

    CHECK_EQ(toString(requireType("Module/Users", "a")), "A<number, string>");
    CHECK_EQ(toString(requireType("Module/Users", "b")), "B<number, number>");
    CHECK_EQ(toString(requireType("Module/Users", "c")), "C<number, (number, number) -> string>");
    CHECK_EQ(toString(requireType("Module/Users", "d")), "D<number, number, number>");
    CHECK_EQ(toString(requireType("Module/Users", "e")), "E<string, number>");
    CHECK_EQ(toString(requireType("Module/Users", "eVoid")), "E<>");
    CHECK_EQ(toString(requireType("Module/Users", "f")), "F<number, ...number>");
    CHECK_EQ(toString(requireType("Module/Users", "g")), "G<...number, ()>");
    CHECK_EQ(toString(requireType("Module/Users", "h")), "H<>");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_default_type_skip_brackets")
{
    CheckResult result = check(R"(
type Y<T... = ...string> = (T...) -> number
local a: Y
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireType("a")), "(...string) -> number");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_defaults_confusing_types")
{
    CheckResult result = check(R"(
type A<T, U = T, V... = ...any, W... = V...> = (T, V...) -> (U, W...)
type B = A<string, (number)>
type C = A<string, (number), (boolean)>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(*lookupType("B"), {true}), "(string, ...any) -> (number, ...any)");
    CHECK_EQ(toString(*lookupType("C"), {true}), "(string, boolean) -> (number, boolean)");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_defaults_recursive_type")
{
    CheckResult result = check(R"(
type F<K = string, V = (K) -> ()> = (K) -> V
type R = { m: F<R> }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(*lookupType("R"), {true}), "t1 where t1 = { m: (t1) -> (t1) -> () }");
}

TEST_CASE_FIXTURE(Fixture, "pack_tail_unification_check")
{
    CheckResult result = check(R"(
local a: () -> (number, ...string)
local b: () -> (number, ...boolean)
a = b
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    if (FFlag::LuauSolverV2)
    {

        const std::string expected = "Type\n\t"
                                     "'() -> (number, ...boolean)'"
                                     "\ncould not be converted into\n\t"
                                     "'() -> (number, ...string)'; \n"
                                     "this is because it returns a tail of the variadic `boolean` in the former type and `string` in the latter "
                                     "type, and `boolean` is not a subtype of `string`";

        CHECK(expected == toString(result.errors[0]));
    }
    else
    {
        const std::string expected = R"(Type
	'() -> (number, ...boolean)'
could not be converted into
	'() -> (number, ...string)'
caused by:
  Type 'boolean' could not be converted into 'string')";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

// TODO: File a Jira about this
/*
TEST_CASE_FIXTURE(Fixture, "unifying_vararg_pack_with_fixed_length_pack_produces_fixed_length_pack")
{
    CheckResult result = check(R"(
        function a(x) return 1 end
        a(...)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    ModulePtr mainModule = getMainModule();
    REQUIRE(mainModule);
    REQUIRE(mainModule->hasModuleScope());

    REQUIRE(bool(mainModule->getModuleScope()->varargPack));

    TypePackId varargPack = *mainModule->getModuleScope()->varargPack;

    auto iter = begin(varargPack);
    auto endIter = end(varargPack);

    CHECK(iter != endIter);
    ++iter;
    CHECK(iter == endIter);

    CHECK(!iter.tail());
}
*/

TEST_CASE_FIXTURE(Fixture, "dont_ice_if_a_TypePack_is_an_error")
{
    CheckResult result = check(R"(
        --!strict
        function f(s)
            print(s)
            return f
        end

        f("foo")("bar")
    )");
}

TEST_CASE_FIXTURE(Fixture, "cyclic_type_packs")
{
    // this has a risk of creating cyclic type packs, causing infinite loops / OOMs
    check(R"(
--!nonstrict
_ += _(_,...)
repeat
_ += _(...)
until ... + _
)");

    check(R"(
--!nonstrict
_ += _(_(...,...),_(...))
repeat
until _
)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "detect_cyclic_typepacks")
{
    CheckResult result = check(R"(
        type ( ... ) ( ) ;
        ( ... ) ( - - ... ) ( - ... )
        type = ( ... ) ;
        ( ... ) (  ) ( ... ) ;
        ( ... ) ""
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "detect_cyclic_typepacks2")
{
    CheckResult result = check(R"(
        function _(l0:((typeof((pcall)))|((((t0)->())|(typeof(-67108864)))|(any)))|(any),...):(((typeof(0))|(any))|(any),typeof(-67108864),any)
            xpcall(_,_,_)
            _(_,_,_)
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK("Unknown type 't0'" == toString(result.errors[0]));
    CHECK(get<FunctionExitsWithoutReturning>(result.errors[1]));
}

TEST_CASE_FIXTURE(Fixture, "unify_variadic_tails_in_arguments")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function foo(...: string): number
            return 1
        end

        function bar(...: number): number
            return foo(...)
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), "Type 'number' could not be converted into 'string'");
}

TEST_CASE_FIXTURE(Fixture, "unify_variadic_tails_in_arguments_free")
{
    CheckResult result = check(R"(
        function foo<T...>(...: T...): T...
            return ...
        end

        function bar(...: number): boolean
            return foo(...)
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    if (FFlag::LuauSolverV2)
        CHECK(
            toString(result.errors.at(0)) == "Type pack '...number' could not be converted into 'boolean'; \nthis is because it has a tail of "
                                             "`...number`, which is not a subtype of `boolean`"
        );
    else
        CHECK_EQ(toString(result.errors[0]), "Type 'number' could not be converted into 'boolean'");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_packs_with_tails_in_vararg_adjustment")
{
    std::optional<ScopedFastFlag> sff;
    if (FFlag::LuauSolverV2)
        sff = {FFlag::LuauInstantiateInSubtyping, true};

    CheckResult result = check(R"(
        local function wrapReject<TArg, TResult>(fn: (self: any, ...TArg) -> ...TResult): (self: any, ...TArg) -> ...TResult
            return function(self, ...)
                local arguments = { ... }
                local ok, result = pcall(function()
                    return fn(self, table.unpack(arguments))
                end)
                return result
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generalize_expectedTypes_with_proper_scope")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauInstantiateInSubtyping, true},
    };

    CheckResult result = check(R"(
        local function f<TResult>(fn: () -> ...TResult): () -> ...TResult
            return function()
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "fuzz_typepack_iter_follow")
{
    CheckResult result = check(R"(
local _
local _ = _,_(),_(_)
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzz_typepack_iter_follow_2")
{
    CheckResult result = check(R"(
function test(name, searchTerm)
    local found = string.find(name:lower(), searchTerm:lower())
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "type_param_overflow")
{
    CheckResult result = check(R"(
        type Two<T,U> = { a: T, b: U }
        local x: Two<number, string, number> = { a = 1, b = 'c' }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_SUITE_END();
