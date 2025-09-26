// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "ScopedFlags.h"
#include "doctest.h"

LUAU_FASTFLAG(LuauInstantiateInSubtyping)
LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauReturnMappedGenericPacksFromSubtyping3)
LUAU_FASTFLAG(LuauIntersectNotNil)
LUAU_FASTFLAG(DebugLuauAssertOnForcedConstraint)
LUAU_FASTFLAG(LuauContainsAnyGenericFollowBeforeChecking)
LUAU_FASTFLAG(LuauSubtypingGenericsDoesntUseVariance)
LUAU_FASTFLAG(LuauSubtypingReportGenericBoundMismatches2)
LUAU_FASTFLAG(LuauSubtypingGenericPacksDoesntUseVariance)
LUAU_FASTFLAG(DebugLuauStringSingletonBasedOnQuotes)
LUAU_FASTFLAG(LuauSubtypingUnionsAndIntersectionsInGenericBounds)

using namespace Luau;

TEST_SUITE_BEGIN("GenericsTests");

TEST_CASE_FIXTURE(Fixture, "check_generic_function")
{
    CheckResult result = check(R"(
        function id<a>(x:a): a
            return x
        end
        local x: string = id("hi")
        local y: number = id(37)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(getBuiltins()->stringType, requireType("x"));
    CHECK_EQ(getBuiltins()->numberType, requireType("y"));
}

TEST_CASE_FIXTURE(Fixture, "check_generic_local_function")
{
    CheckResult result = check(R"(
        local function id<a>(x:a): a
            return x
        end
        local x: string = id("hi")
        local y: number = id(37)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(getBuiltins()->stringType, requireType("x"));
    CHECK_EQ(getBuiltins()->numberType, requireType("y"));
}

TEST_CASE_FIXTURE(Fixture, "check_generic_local_function2")
{
    CheckResult result = check(R"(
        local function id<a>(x:a): a
            return x
        end
        local x = id("hi")
        local y = id(37)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(getBuiltins()->stringType, requireType("x"));
    CHECK_EQ(getBuiltins()->numberType, requireType("y"));
}

TEST_CASE_FIXTURE(Fixture, "unions_and_generics")
{
    CheckResult result = check(R"(
        type foo = <T>(T | {T}) -> T
        local foo = (nil :: any) :: foo

        type Test = number | {number}
        local res = foo(1 :: Test)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::LuauSolverV2)
        CHECK_EQ("number | {number}", toString(requireType("res")));
    else // in the old solver, this just totally falls apart
        CHECK_EQ("'a", toString(requireType("res")));
}

TEST_CASE_FIXTURE(Fixture, "check_generic_typepack_function")
{
    CheckResult result = check(R"(
        function id<a...>(...: a...): (a...) return ... end
        local x: string, y: boolean = id("hi", true)
        local z: number = id(37)
        id()
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "types_before_typepacks")
{
    CheckResult result = check(R"(
        function f<a,b...>() end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "local_vars_can_be_polytypes")
{
    CheckResult result = check(R"(
        local function id<a>(x:a):a return x end
        local f: <a>(a)->a = id
        local x: string = f("hi")
        local y: number = f(37)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "inferred_local_vars_can_be_polytypes")
{
    CheckResult result = check(R"(
        local function id(x) return x end
        print("This is bogus") -- TODO: CLI-39916
        local f = id
        local x: string = f("hi")
        local y: number = f(37)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "local_vars_can_be_instantiated_polytypes")
{
    CheckResult result = check(R"(
        local function id(x) return x end
        print("This is bogus") -- TODO: CLI-39916
        local f: (number)->number = id
        local g: (string)->string = id
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "properties_can_be_polytypes")
{
    CheckResult result = check(R"(
        local t = {}
        t.m = function<a>(x: a):a return x end
        local x: string = t.m("hi")
        local y: number = t.m(37)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "properties_can_be_instantiated_polytypes")
{
    CheckResult result = check(R"(
        local t: { m: (number)->number } = { m = function(x:number) return x+1 end }
        local function id<a>(x:a):a return x end
        t.m = id
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "check_nested_generic_function")
{
    CheckResult result = check(R"(
        local function f()
            local function id<a>(x:a): a
                return x
            end
            local x: string = id("hi")
            local y: number = id(37)
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "check_recursive_generic_function")
{
    CheckResult result = check(R"(
        local function id<a>(x:a):a
            local y: string = id("hi")
            local z: number = id(37)
            return x
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "check_mutual_generic_functions")
{
    CheckResult result = check(R"(
        function id1<a>(x:a):a
            local y: string = id2("hi")
            local z: number = id2(37)
            return x
        end

        function id2<a>(x:a):a
            local y: string = id1("hi")
            local z: number = id1(37)
            return x
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "check_mutual_generic_functions_unannotated")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        function id1(x)
            local y: string = id2("hi")
            local z: number = id2(37)
            return x
        end

        function id2(x)
            local y: string = id1("hi")
            local z: number = id1(37)
            return x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "check_mutual_generic_functions_errors")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        function id1(x)
            local y: string = id2(37) -- odd
            local z: number = id2("hi") -- even
            return x
        end

        function id2(x)
            local y: string = id1(37) -- odd
            local z: number = id1("hi") -- even
            return x
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);

    // odd errors
    for (int i = 0; i < 4; i += 2)
    {
        TypeMismatch* tm = get<TypeMismatch>(result.errors[i]);
        REQUIRE(tm);
        CHECK_EQ("string", toString(tm->wantedType));
        CHECK_EQ("number", toString(tm->givenType));
    }

    // even errors
    for (int i = 1; i < 4; i += 2)
    {
        TypeMismatch* tm = get<TypeMismatch>(result.errors[i]);
        REQUIRE(tm);
        CHECK_EQ("number", toString(tm->wantedType));
        CHECK_EQ("string", toString(tm->givenType));
    }
}

TEST_CASE_FIXTURE(Fixture, "generic_functions_in_types")
{
    CheckResult result = check(R"(
        type T = { id: <a>(a) -> a }
        local x: T = { id = function<a>(x:a):a return x end }
        local y: string = x.id("hi")
        local z: number = x.id(37)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "generic_factories")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        type T<a> = { id: (a) -> a }
        type Factory = { build: <a>() -> T<a> }

        local f: Factory = {
            build = function<a>(): T<a>
                return {
                    id = function(x:a):a
                        return x
                    end
                }
            end
        }
        local y: string = f.build().id("hi")
        local z: number = f.build().id(37)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "factories_of_generics")
{
    CheckResult result = check(R"(
        type T = { id: <a>(a) -> a }
        type Factory = { build: () -> T }

        local f: Factory = {
            build = function(): T
                return {
                    id = function<a>(x:a):a
                        return x
                    end
                }
            end
        }
        local x: T = f.build()
        local y: string = x.id("hi")
        local z: number = x.id(37)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "infer_generic_function")
{
    CheckResult result = check(R"(
        function id(x)
            return x
        end
        local x: string = id("hi")
        local y: number = id(37)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId idType = requireType("id");
    const FunctionType* idFun = get<FunctionType>(idType);
    REQUIRE(idFun);
    auto [args, varargs] = flatten(idFun->argTypes);
    auto [rets, varrets] = flatten(idFun->retTypes);

    CHECK_EQ(idFun->generics.size(), 1);
    CHECK_EQ(idFun->genericPacks.size(), 0);
    CHECK_EQ(follow(args[0]), follow(idFun->generics[0]));
    CHECK_EQ(follow(rets[0]), follow(idFun->generics[0]));
}

TEST_CASE_FIXTURE(Fixture, "infer_generic_local_function")
{
    CheckResult result = check(R"(
        local function id(x)
            return x
        end
        local x: string = id("hi")
        local y: number = id(37)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId idType = requireType("id");
    const FunctionType* idFun = get<FunctionType>(idType);
    REQUIRE(idFun);
    auto [args, varargs] = flatten(idFun->argTypes);
    auto [rets, varrets] = flatten(idFun->retTypes);

    CHECK_EQ(idFun->generics.size(), 1);
    CHECK_EQ(idFun->genericPacks.size(), 0);
    CHECK_EQ(follow(args[0]), follow(idFun->generics[0]));
    CHECK_EQ(follow(rets[0]), follow(idFun->generics[0]));
}

TEST_CASE_FIXTURE(Fixture, "infer_nested_generic_function")
{
    CheckResult result = check(R"(
        local function f()
            local function id(x)
                return x
            end
            local x: string = id("hi")
            local y: number = id(37)
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "calling_self_generic_methods")
{
    CheckResult result = check(R"(
        local x = {}
        function x:id(x) return x end
        function x:f()
            local x: string = self:id("hi")
            local y: number = self:id(37)
        end
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "infer_generic_property")
{
    CheckResult result = check(R"(
        local t = {}
        t.m = function(x) return x end
        local x: string = t.m("hi")
        local y: number = t.m(37)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "function_arguments_can_be_polytypes")
{
    CheckResult result = check(R"(
        local function f(g: <a>(a)->a)
            local x: number = g(37)
            local y: string = g("hi")
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "function_results_can_be_polytypes")
{
    CheckResult result = check(R"(
        local function f() : <a>(a)->a
            local function id<a>(x:a):a return x end
            return id
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "type_parameters_can_be_polytypes")
{
    CheckResult result = check(R"(
        local function id<a>(x:a):a return x end
        local f: <a>(a)->a = id(id)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "dont_leak_generic_types")
{
    CheckResult result = check(R"(
        local function f(y)
            -- this will only typecheck if we infer z: any
            -- so f: (any)->(any)
            local z = y
            local function id(x)
                z = x -- this assignment is what forces z: any
                return x
            end
            local x: string = id("hi")
            local y: number = id(37)
            return z
        end
        -- so this assignment should fail
        local b: boolean = f(true)
    )");

    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_NO_ERRORS(result);
    }
    else
    {
        LUAU_REQUIRE_ERRORS(result);
    }
}

TEST_CASE_FIXTURE(Fixture, "dont_leak_inferred_generic_types")
{
    CheckResult result = check(R"(
        local function f(y)
            local z = y
            local function id(x)
                z = x
                return x
            end
            local x: string = id("hi")
            local y: number = id(37)
        end
    )");
    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_NO_ERRORS(result);
    }
    else
    {
        LUAU_REQUIRE_ERRORS(result);
    }
}

TEST_CASE_FIXTURE(Fixture, "dont_substitute_bound_types")
{
    CheckResult result = check(R"(
        type T = { m: <a>(a) -> T }
        function f(t : T)
            local x: T = t.m(37)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "dont_unify_bound_types")
{
    CheckResult result = check(R"(
        type F = <a>() -> <b>(a, b) -> a
        type G = <b>(b, b) -> b
        local f: F = function<a>()
          local x
          return function<b>(y: a, z: b): a
            if not(x) then x = y end
            return x
          end
        end
        -- This assignment shouldn't typecheck
        -- If it does, it means we instantiated
        -- f as () -> <b>(X, b) -> X, then unified X to be b
        local g: G = f()
        -- Oh dear, if that works then the type system is unsound
        local a : string = g("not a number", "hi")
        local b : number = g(5, 37)
    )");
    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "mutable_state_polymorphism")
{
    // Replaying the classic problem with polymorphism and mutable state in Luau
    // See, e.g. Tofte (1990)
    // https://www.sciencedirect.com/science/article/pii/089054019090018D.
    CheckResult result = check(R"(
        --!strict
        -- Our old friend the polymorphic identity function
        local function id(x) return x end
        local a: string = id("hi")
        local b: number = id(37)

        -- This allows <a>(a)->a to be expressed without generic function syntax
        type Id = typeof(id)

        -- This function should have type
        -- <a>() -> (a) -> a
        -- not type
        -- () -> <a>(a) -> a
        local function ohDear(): Id
          local y
          function oh(x)
            -- Returns the same x every time it's called
            if not(y) then y = x end
            return y
          end
          return oh
        end

        -- oh dear, f claims to polymorphic which it shouldn't be
        local f: Id = ohDear()

        -- the first call sets y
        local a: string = f("not a number")
        -- so b has value "not a number" at run time
        local b: number = f(37)
    )");
    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "rank_N_types_via_typeof")
{
    CheckResult result = check(R"(
        --!strict
        local function id(x) return x end
        local x: string = id("hi")
        local y: number = id(37)
        -- This allows <a>(a)->a to be expressed without generic function syntax
        type Id = typeof(id)
        -- The rank 1 restriction causes this not to typecheck, since it's
        -- declared as returning a polytype.
        local function returnsId(): Id
          return id
        end
        -- So this won't typecheck
        local f: Id = returnsId()
        local a: string = f("hi")
        local b: number = f(37)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "duplicate_generic_types")
{
    CheckResult result = check(R"(
        function f<a,a>(x:a):a return x end
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "duplicate_generic_type_packs")
{
    CheckResult result = check(R"(
        function f<a...,a...>() end
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "typepacks_before_types")
{
    CheckResult result = check(R"(
        function f<a...,b>() end
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "variadic_generics")
{
    CheckResult result = check(R"(
        function f<a>(...: a) end

        type F<a> = (...a) -> ...a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "generic_type_pack_syntax")
{
    CheckResult result = check(R"(
        function f<a...>(...: a...): (a...) return ... end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(toString(requireType("f")), "<a...>(a...) -> (a...)");
}

TEST_CASE_FIXTURE(Fixture, "generic_type_pack_parentheses")
{
    CheckResult result = check(R"(
        function f<a...>(...: a...): any return (...) end
    )");

    // This should really error, but the error from the old solver is wrong.
    // `a...` is a generic type pack, and we don't know that it will be non-empty, thus this code may not work.
    if (FFlag::LuauSolverV2)
        LUAU_REQUIRE_NO_ERRORS(result);
    else
        LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "better_mismatch_error_messages")
{
    CheckResult result = check(R"(
        function f<T>(...: T...)
            return ...
        end

        function g<T...>(a: T)
            return a
        end
    )");

    SwappedGenericTypeParameter* fErr;
    SwappedGenericTypeParameter* gErr;

    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_ERROR_COUNT(3, result);
        // The first error here is an unknown symbol that is redundant with the `fErr`.
        fErr = get<SwappedGenericTypeParameter>(result.errors[1]);
        gErr = get<SwappedGenericTypeParameter>(result.errors[2]);
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        fErr = get<SwappedGenericTypeParameter>(result.errors[0]);
        gErr = get<SwappedGenericTypeParameter>(result.errors[1]);
    }

    REQUIRE(fErr);
    CHECK_EQ(fErr->name, "T");
    CHECK_EQ(fErr->kind, SwappedGenericTypeParameter::Pack);

    REQUIRE(gErr);
    CHECK_EQ(gErr->name, "T");
    CHECK_EQ(gErr->kind, SwappedGenericTypeParameter::Type);
}

TEST_CASE_FIXTURE(Fixture, "reject_clashing_generic_and_pack_names")
{
    CheckResult result = check(R"(
        function f<a, a...>() end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    DuplicateGenericParameter* err = get<DuplicateGenericParameter>(result.errors[0]);
    REQUIRE(err != nullptr);
    CHECK_EQ(err->parameterName, "a");
}

TEST_CASE_FIXTURE(Fixture, "instantiation_sharing_types")
{
    CheckResult result = check(R"(
        function f(z)
          local o = {}
          o.x = o
          o.y = {5}
          o.z = z
          return o
        end
        local o1 = f(true)
        local x1, y1, z1 = o1.x, o1.y, o1.z
        local o2 = f("hi")
        local x2, y2, z2 = o2.x, o2.y, o2.z
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(requireType("x1") != requireType("x2"));
    CHECK(requireType("y1") == requireType("y2"));
    CHECK(requireType("z1") != requireType("z2"));
}

TEST_CASE_FIXTURE(Fixture, "quantification_sharing_types")
{
    CheckResult result = check(R"(
        function f(x) return {5} end
        function g(x, y) return f(x) end
        local z1 = f(5)
        local z2 = g(true, "hi")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(requireType("z1") == requireType("z2"));
}

TEST_CASE_FIXTURE(Fixture, "typefuns_sharing_types")
{
    CheckResult result = check(R"(
        type T<a> = { x: {a}, y: {number} }
        local o1: T<boolean> = { x = {true}, y = {5} }
        local x1, y1 = o1.x, o1.y
        local o2: T<string> = { x = {"hi"}, y = {37} }
        local x2, y2 = o2.x, o2.y
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(requireType("x1") != requireType("x2"));
    CHECK(requireType("y1") == requireType("y2"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "bound_tables_do_not_clone_original_fields")
{
    CheckResult result = check(R"(
local exports = {}
local nested = {}

nested.name = function(t, k)
    local a = t.x.y
    return rawget(t, k)
end

exports.nested = nested
return exports
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "instantiated_function_argument_names_old_solver")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        local function f<T, U...>(a: T, ...: U...) end

        f(1, 2, 3)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    auto ty = findTypeAtPosition(Position(3, 8));
    REQUIRE(ty);
    ToStringOptions opts;
    opts.functionTypeArguments = true;
    CHECK_EQ(toString(*ty, opts), "(a: number, number, number) -> ()");
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_function_mismatch_generic_types")
{
    CheckResult result = check(R"(
type C = () -> ()
type D = <T>() -> ()

local c: C
local d: D = c
    )");

    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        const auto genericMismatch = get<GenericTypeCountMismatch>(result.errors[0]);
        CHECK(genericMismatch);
        CHECK_EQ(genericMismatch->subTyGenericCount, 1);
        CHECK_EQ(genericMismatch->superTyGenericCount, 0);

        auto mismatch = get<TypeMismatch>(result.errors[1]);
        CHECK(mismatch);
        CHECK_EQ(toString(mismatch->givenType), "() -> ()");
        CHECK_EQ(toString(mismatch->wantedType), "<T>() -> ()");
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        auto mismatch = get<TypeMismatch>(result.errors[0]);
        CHECK(mismatch);
        CHECK_EQ(mismatch->reason, "different number of generic type parameters");
    }
}
TEST_CASE_FIXTURE(Fixture, "generic_function_mismatch_with_argument")
{
    CheckResult result = check(R"(
type C = (number) -> ()
type D = <T>(number) -> ()

local c: C
local d: D = c
    )");

    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        const auto genericMismatch = get<GenericTypeCountMismatch>(result.errors[0]);
        CHECK(genericMismatch);
        CHECK_EQ(genericMismatch->subTyGenericCount, 1);
        CHECK_EQ(genericMismatch->superTyGenericCount, 0);

        auto mismatch = get<TypeMismatch>(result.errors[1]);
        CHECK(mismatch);
        CHECK_EQ(toString(mismatch->givenType), "(number) -> ()");
        CHECK_EQ(toString(mismatch->wantedType), "<T>(number) -> ()");
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        auto mismatch = get<TypeMismatch>(result.errors[0]);
        CHECK(mismatch);
        CHECK_EQ(mismatch->reason, "different number of generic type parameters");
    }
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_function_mismatch_generic_pack")
{
    CheckResult result = check(R"(
type C = () -> ()
type D = <T...>() -> ()

local c: C
local d: D = c
    )");

    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        const auto genericMismatch = get<GenericTypePackCountMismatch>(result.errors[0]);
        CHECK(genericMismatch);
        CHECK_EQ(genericMismatch->subTyGenericPackCount, 1);
        CHECK_EQ(genericMismatch->superTyGenericPackCount, 0);

        auto mismatch = get<TypeMismatch>(result.errors[1]);
        CHECK(mismatch);
        CHECK_EQ(toString(mismatch->givenType), "() -> ()");
        CHECK_EQ(toString(mismatch->wantedType), "<T...>() -> ()");
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        auto mismatch = get<TypeMismatch>(result.errors[0]);
        CHECK(mismatch);
        CHECK_EQ(mismatch->reason, "different number of generic type pack parameters");
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generic_functions_dont_cache_type_parameters")
{
    CheckResult result = check(R"(
-- See https://github.com/luau-lang/luau/issues/332
-- This function has a type parameter with the same name as clones,
-- so if we cache type parameter names for functions these get confused.
-- function id<Z>(x : Z) : Z
function id<X>(x : X) : X
  return x
end

function clone<X, Y>(dict: {[X]:Y}): {[X]:Y}
  local copy = {}
  for k, v in pairs(dict) do
    copy[k] = v
  end
  return copy
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "generic_functions_should_be_memory_safe")
{
    CheckResult result = check(R"(
--!strict
-- At one point this produced a UAF
type T<a> = { a: U<a>, b: a }
type U<a> = { c: T<a>?, d : a }
local x: T<number> = { a = { c = nil, d = 5 }, b = 37 }
x.a.c = x
local y: T<string> = { a = { c = nil, d = 5 }, b = 37 }
y.a.c = y
    )");

    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        auto mismatch1 = get<TypeMismatch>(result.errors[0]);
        auto mismatch2 = get<TypeMismatch>(result.errors[1]);
        REQUIRE(mismatch1);
        REQUIRE(mismatch2);
        CHECK_EQ(result.errors[0].location, Location{{7, 42}, {7, 43}});
        CHECK_EQ(toString(mismatch1->givenType), "number");
        CHECK_EQ(toString(mismatch1->wantedType), "string");
        CHECK_EQ(result.errors[1].location, Location{{7, 51}, {7, 53}});
        CHECK_EQ(toString(mismatch2->givenType), "number");
        CHECK_EQ(toString(mismatch2->wantedType), "string");
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        const std::string expected = R"(Type 'y' could not be converted into 'T<string>'
caused by:
  Property 'a' is not compatible.
Type '{| c: T<string>?, d: number |}' could not be converted into 'U<string>'
caused by:
  Property 'd' is not compatible.
Type 'number' could not be converted into 'string' in an invariant context)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "generic_type_pack_unification1")
{
    CheckResult result = check(R"(
--!strict
type Dispatcher = {
	useMemo: <T...>(create: () -> T...) -> T...
}

local TheDispatcher: Dispatcher = {
	useMemo = function<U...>(create: () -> U...): U...
		return create()
	end
}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "generic_type_pack_unification2")
{
    CheckResult result = check(R"(
--!strict
type Dispatcher = {
	useMemo: <T...>(create: () -> T...) -> T...
}

local TheDispatcher: Dispatcher = {
	useMemo = function(create)
		return create()
	end
}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "generic_type_pack_unification3")
{
    CheckResult result = check(R"(
--!strict
type Dispatcher = {
	useMemo: <S,T...>(arg: S, create: (S) -> T...) -> T...
}

local TheDispatcher: Dispatcher = {
	useMemo = function<T,U...>(arg: T, create: (T) -> U...): U...
		return create(arg)
	end
}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "generic_argument_count_too_few")
{
    ScopedFastFlag _{FFlag::LuauReturnMappedGenericPacksFromSubtyping3, true};

    CheckResult result = check(R"(
function test(a: number)
    return 1
end

function wrapper<A...>(f: (A...) -> number, ...: A...)
end

wrapper(test)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    if (FFlag::LuauSolverV2)
    {
        const CountMismatch* cm = get<CountMismatch>(result.errors[0]);
        REQUIRE_MESSAGE(cm, "Expected CountMismatch but got " << result.errors[0]);
        CHECK_EQ(cm->expected, 2);
        CHECK_EQ(cm->actual, 1);
        CHECK_EQ(cm->context, CountMismatch::Arg);
    }
    else
        CHECK_EQ(toString(result.errors[0]), R"(Argument count mismatch. Function 'wrapper' expects 2 arguments, but only 1 is specified)");
}

TEST_CASE_FIXTURE(Fixture, "generic_argument_count_too_many")
{
    ScopedFastFlag _{FFlag::LuauReturnMappedGenericPacksFromSubtyping3, true};

    CheckResult result = check(R"(
function test2(a: number, b: string)
    return 1
end

function wrapper<A...>(f: (A...) -> number, ...: A...)
end

wrapper(test2, 1, "", 3)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    if (FFlag::LuauSolverV2)
    {
        const CountMismatch* cm = get<CountMismatch>(result.errors[0]);
        REQUIRE_MESSAGE(cm, "Expected CountMismatch but got " << result.errors[0]);
        CHECK_EQ(cm->expected, 3);
        CHECK_EQ(cm->actual, 4);
        CHECK_EQ(cm->context, CountMismatch::Arg);
    }
    else
        CHECK_EQ(toString(result.errors[0]), R"(Argument count mismatch. Function 'wrapper' expects 3 arguments, but 4 are specified)");
}

TEST_CASE_FIXTURE(Fixture, "generic_argument_count_just_right")
{
    CheckResult result = check(R"(
function test2(a: number, b: string)
    return 1
end

function wrapper<A...>(f: (A...) -> number, ...: A...)
end

wrapper(test2, 1, "")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "generic_argument_pack_type_inferred_from_return")
{
    ScopedFastFlag _{FFlag::LuauReturnMappedGenericPacksFromSubtyping3, true};

    CheckResult result = check(R"(
function test2(a: number)
    return "hello"
end

function wrapper<A...>(f: (number) -> A..., ...: A...)
end

wrapper(test2, 1)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    if (FFlag::LuauSolverV2)
    {
        const TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
        REQUIRE_MESSAGE(tm, "Expected TypeMismatch but got " << result.errors[0]);
        CHECK_EQ(toString(tm->wantedType), "string");
        CHECK_EQ(toString(tm->givenType), "number");
    }
    else
    {
        CHECK_EQ(toString(result.errors[0]), R"(Type 'number' could not be converted into 'string')");
    }
}

TEST_CASE_FIXTURE(Fixture, "generic_argument_pack_type_inferred_from_return_no_error")
{
    ScopedFastFlag _{FFlag::LuauReturnMappedGenericPacksFromSubtyping3, true};

    CheckResult result = check(R"(
function test2(a: number)
    return "hello"
end

function wrapper<A...>(f: (number) -> A..., ...: A...)
end

wrapper(test2, "hello")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "nested_generic_argument_type_packs")
{
    ScopedFastFlag _{FFlag::LuauReturnMappedGenericPacksFromSubtyping3, true};

    CheckResult result = check(R"(
function test2(a: number)
    return 3
end

function foo<B...>(f: (B...) -> number, ...: B...)
    return f(...)
end

-- want A... to contain a generic type pack too

function wrapper<A...>(f: (A...) -> number, ...: A...)
end

-- A... = ((B...) -> number, B...))
-- B... = (number)
-- A... = ((number) -> number, number)
wrapper(foo, test2, 3) -- ok
wrapper(foo, test2, 3, 3) -- not ok (too many args)
wrapper(foo, test2) -- not ok (not enough args)
wrapper(foo, test2, "3") -- not ok (type mismatch, string instead of number)
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);
    if (FFlag::LuauSolverV2)
    {
        CHECK_EQ(result.errors[0].location, Location{{18, 0}, {18, 7}});
        CountMismatch* cm = get<CountMismatch>(result.errors[0]);
        REQUIRE_MESSAGE(cm, "Expected CountMismatch but got " << result.errors[0]);
        CHECK_EQ(cm->expected, 3);
        CHECK_EQ(cm->actual, 4);
        CHECK_EQ(cm->context, CountMismatch::Arg);

        CHECK_EQ(result.errors[1].location, Location{{19, 0}, {19, 7}});
        cm = get<CountMismatch>(result.errors[1]);
        REQUIRE_MESSAGE(cm, "Expected CountMismatch but got " << result.errors[1]);
        CHECK_EQ(cm->expected, 3);
        CHECK_EQ(cm->actual, 2);
        CHECK_EQ(cm->context, CountMismatch::Arg);

        CHECK_EQ(result.errors[2].location, Location{{20, 20}, {20, 23}});
        TypeMismatch* tm = get<TypeMismatch>(result.errors[2]);
        REQUIRE_MESSAGE(tm, "Expected TypeMismatch but got " << result.errors[2]);
        CHECK_EQ(toString(tm->wantedType), "number");
        CHECK_EQ(toString(tm->givenType), "string");
    }
    else
    {
        CHECK_EQ(toString(result.errors[0]), R"(Argument count mismatch. Function 'wrapper' expects 3 arguments, but 4 are specified)");
        CHECK_EQ(toString(result.errors[1]), R"(Argument count mismatch. Function 'wrapper' expects 3 arguments, but only 2 are specified)");
        CHECK_EQ(toString(result.errors[2]), R"(Type 'string' could not be converted into 'number')");
    }
}

TEST_CASE_FIXTURE(Fixture, "generic_function")
{
    CheckResult result = check(R"(
        function id(x) return x end
        local a = id(55)
        local b = id(nil)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("<a>(a) -> a", toString(requireType("id")));
    CHECK_EQ(*getBuiltins()->numberType, *requireType("a"));
    CHECK_EQ(*getBuiltins()->nilType, *requireType("b"));
}

TEST_CASE_FIXTURE(Fixture, "generic_table_method")
{
    CheckResult result = check(R"(
        local T = {}

        function T:bar(i)
            return i
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId tType = requireType("T");
    TableType* tTable = getMutable<TableType>(tType);
    REQUIRE(tTable != nullptr);

    REQUIRE(tTable->props.count("bar"));
    Property& bar = tTable->props["bar"];
    REQUIRE(bar.readTy);
    TypeId barType = *bar.readTy;
    REQUIRE(barType != nullptr);

    const FunctionType* ftv = get<FunctionType>(follow(barType));
    REQUIRE_MESSAGE(ftv != nullptr, "Should be a function: " << *barType);

    std::vector<TypeId> args = flatten(ftv->argTypes).first;
    TypeId argType = args.at(1);

    CHECK_MESSAGE(get<GenericType>(argType), "Should be generic: " << *barType);
}

TEST_CASE_FIXTURE(Fixture, "correctly_instantiate_polymorphic_member_functions")
{
    ScopedFastFlag sff{FFlag::DebugLuauAssertOnForcedConstraint, true};

    CheckResult result = check(R"(
        local T = {}

        function T:foo()
            return T:bar(5)
        end

        function T:bar(i)
            return i
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);

    const TableType* t = get<TableType>(requireType("T"));
    REQUIRE(t != nullptr);

    std::optional<Property> fooProp = get(t->props, "foo");
    REQUIRE(bool(fooProp));

    REQUIRE(fooProp->readTy);
    const FunctionType* foo = get<FunctionType>(follow(*fooProp->readTy));
    REQUIRE(bool(foo));

    std::optional<TypeId> ret_ = first(foo->retTypes);
    REQUIRE(bool(ret_));
    TypeId ret = follow(*ret_);

    REQUIRE_EQ(getPrimitiveType(ret), PrimitiveType::Number);
}

/*
 * We had a bug in instantiation where the argument types of 'f' and 'g' would be inferred as
 * f {+ method: function(<CYCLE>): (t2, T3...) +}
 * g {+ method: function({+ method: function(<CYCLE>): (t2, T3...) +}): (t5, T6...) +}
 *
 * The type of 'g' is totally wrong as t2 and t5 should be unified, as should T3 with T6.
 *
 * The correct unification of the argument to 'g' is
 *
 * {+ method: function(<CYCLE>): (t5, T6...) +}
 */
TEST_CASE_FIXTURE(Fixture, "instantiate_cyclic_generic_function")
{
    auto result = check(R"(
        function f(o)
            o:method()
        end

        function g(o)
            f(o)
        end
    )");

    TypeId g = requireType("g");
    const FunctionType* gFun = get<FunctionType>(g);
    REQUIRE(gFun != nullptr);

    auto optionArg = first(gFun->argTypes);
    REQUIRE(bool(optionArg));

    TypeId arg = follow(*optionArg);
    const TableType* argTable = get<TableType>(arg);
    REQUIRE_MESSAGE(argTable != nullptr, "Expected table but got " << toString(arg));

    std::optional<Property> methodProp = get(argTable->props, "method");
    REQUIRE(bool(methodProp));

    REQUIRE(methodProp->readTy);
    const FunctionType* methodFunction = get<FunctionType>(follow(*methodProp->readTy));
    REQUIRE(methodFunction != nullptr);

    std::optional<TypeId> methodArg = first(methodFunction->argTypes);
    REQUIRE(bool(methodArg));

    REQUIRE_EQ(follow(*methodArg), follow(arg));
}

TEST_CASE_FIXTURE(Fixture, "instantiate_generic_function_in_assignments")
{
    CheckResult result = check(R"(
        function foo(a, b)
            return a(b)
        end

        function bar()
            local c: ((number)->number, number)->number = foo -- no error
            c = foo -- no error
            local d: ((number)->number, string)->number = foo -- error from arg 2 (string) not being convertable to number from the call a(b)
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("((number) -> number, string) -> number", toString(tm->wantedType));
    // The new solver does not attempt to instantiate generics here, so if
    // either the instantiate in subtyping flag _or_ the new solver flags
    // are set, assert that we're getting back the original generic
    // function definition.
    if (FFlag::LuauInstantiateInSubtyping || FFlag::LuauSolverV2)
        CHECK_EQ("<a, b...>((a) -> (b...), a) -> (b...)", toString(tm->givenType));
    else
        CHECK_EQ("((number) -> number, number) -> number", toString(tm->givenType));
}

TEST_CASE_FIXTURE(Fixture, "instantiate_generic_function_in_assignments2")
{
    CheckResult result = check(R"(
        function foo(a, b)
            return a(b)
        end

        function bar()
            local _: (string, string)->number = foo -- string cannot be converted to (string)->number
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("(string, string) -> number", toString(tm->wantedType));
    // The new solver does not attempt to instantiate generics here, so if
    // either the instantiate in subtyping flag _or_ the new solver flags
    // are set, assert that we're getting back the original generic
    // function definition.
    if (FFlag::LuauInstantiateInSubtyping || FFlag::LuauSolverV2)
        CHECK_EQ("<a, b...>((a) -> (b...), a) -> (b...)", toString(tm->givenType));
    else
        CHECK_EQ("((string) -> number, string) -> number", toString(*tm->givenType));
}

TEST_CASE_FIXTURE(Fixture, "self_recursive_instantiated_param")
{
    // Mutability in type function application right now can create strange recursive types
    CheckResult result = check(R"(
type Table = { a: number }
type Self<T> = T
local a: Self<Table>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    if (FFlag::LuauSolverV2)
        CHECK_EQ(toString(requireType("a")), "Table<Table>");
    else
        CHECK_EQ(toString(requireType("a")), "Table");
}

TEST_CASE_FIXTURE(Fixture, "no_stack_overflow_from_quantifying")
{
    CheckResult result = check(R"(
        function _(l0:t0): (any, ()->())
        end

        type t0 = t0 | {}
    )");

    LUAU_REQUIRE_ERRORS(result);

    std::optional<TypeId> t0 = lookupType("t0");
    REQUIRE(t0);
    if (FFlag::LuauSolverV2)
        CHECK_EQ("any", toString(*t0));
    else
        CHECK_EQ("*error-type*", toString(*t0));

    auto it = std::find_if(
        result.errors.begin(),
        result.errors.end(),
        [](TypeError& err)
        {
            return get<OccursCheckFailed>(err);
        }
    );
    CHECK(it != result.errors.end());
}

TEST_CASE_FIXTURE(BuiltinsFixture, "infer_generic_function_function_argument")
{
    if (FFlag::LuauSolverV2)
    {
        CheckResult result = check(R"(
            local function sum<a>(x: a, y: a, f: (a, a) -> add<a>)
                return f(x, y)
            end
            return sum(2, 3, function<T>(a: T, b: T): add<T> return a + b end)
        )");

        LUAU_REQUIRE_NO_ERRORS(result);
    }
    else
    {
        CheckResult result = check(R"(
            local function sum<a>(x: a, y: a, f: (a, a) -> a)
                return f(x, y)
            end
            return sum(2, 3, function(a, b) return a + b end)
        )");

        LUAU_REQUIRE_NO_ERRORS(result);
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "infer_generic_function_function_argument_2")
{
    CheckResult result = check(R"(
        local function map<a, b>(arr: {a}, f: (a) -> b): {b}
            local r = {}
            for i,v in ipairs(arr) do
                table.insert(r, f(v))
            end
            return r
        end
        local a = {1, 2, 3}
        local r = map(a, function(a: number) return a + a > 100 end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    REQUIRE_EQ("{boolean}", toString(requireType("r")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "infer_generic_function_function_argument_3")
{
    CheckResult result = check(R"(
        local function foldl<a, b>(arr: {a}, init: b, f: (b, a) -> b)
            local r = init
            for i,v in ipairs(arr) do
                r = f(r, v)
            end
            return r
        end
        local a = {1, 2, 3}
        local r = foldl(a, {s=0,c=0}, function(a: {s: number, c: number}, b: number) return {s = a.s + b, c = a.c + 1} end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    if (FFlag::LuauSolverV2)
        REQUIRE_EQ("{ c: number, s: number } | { c: number, s: number }", toString(requireType("r")));
    else
        REQUIRE_EQ("{| c: number, s: number |}", toString(requireType("r")));
}

TEST_CASE_FIXTURE(Fixture, "infer_generic_function_function_argument_overloaded_pt_1")
{
    CheckResult result = check(R"(
        local g12: (<T>(T, (T) -> T) -> T) & (<T>(T, T, (T, T) -> T) -> T)

        g12(1, function(x) return x + x end)
        g12(1, 2, function(x, y) return x + y end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "infer_generic_function_function_overloaded_pt_2")
{
    ScopedFastFlag _[] = {
        {FFlag::LuauSubtypingReportGenericBoundMismatches2, true},
        {FFlag::LuauSubtypingGenericsDoesntUseVariance, true},
    };
    CheckResult result = check(R"(
        local g12: (<T>(T, (T) -> T) -> T) & (<T>(T, T, (T, T) -> T) -> T)

        g12({x=1}, function(x) return {x=-x.x} end)
        g12({x=1}, {x=2}, function(x, y) return {x=x.x + y.x} end)
    )");

    if (FFlag::LuauSolverV2)
        LUAU_REQUIRE_ERROR_COUNT(2, result); // FIXME CLI-161355
    else
        LUAU_REQUIRE_NO_ERRORS(result);
}

// Important FIXME CLI-161128: This test exposes some problems with overload
// selection and generic type substitution when
TEST_CASE_FIXTURE(BuiltinsFixture, "do_not_infer_generic_functions")
{
    CheckResult result;

    if (FFlag::LuauSolverV2)
    {
        result = check(R"(
            local function sum<T>(x: T, y: T, z: (T, T) -> T) return z(x, y) end

            local function sumrec(f: typeof(sum))
                return sum(2, 3, function<X>(g: X, h: X): add<X, X> return g + h end)
            end

            local b = sumrec(sum) -- ok
            local c = sumrec(
                function(d, e, f)
                    return f(d, e)
                end
            ) -- type binders are not inferred
        )");

        CHECK("add<X, X> | number" == toString(requireType("b"))); // FIXME CLI-161128
        CHECK("<T>(T, T, (T, T) -> T) -> T" == toString(requireType("sum")));
        CHECK("<T>(T, T, (T, T) -> T) -> T" == toString(requireTypeAtPosition({7, 29})));
        LUAU_REQUIRE_ERROR_COUNT(1, result); // FIXME CLI-161128
        CHECK(get<ExplicitFunctionAnnotationRecommended>(result.errors[0]));
    }
    else
    {
        result = check(R"(
            local function sum<a>(x: a, y: a, f: (a, a) -> a) return f(x, y) end

            local function sumrec(f: typeof(sum))
                return sum(2, 3, function(a, b) return a + b end)
            end

            local b = sumrec(sum) -- ok
            local c = sumrec(function(x, y, f) return f(x, y) end) -- type binders are not inferred
        )");
        LUAU_REQUIRE_NO_ERRORS(result);
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "do_not_infer_generic_functions_2")
{
    ScopedFastFlag _[] = {{FFlag::LuauSolverV2, true}, {FFlag::LuauSubtypingGenericsDoesntUseVariance, true}};

    CheckResult result = check(R"(
        type t = <a>(a, a, (a, a) -> a) -> a
        type u = (number, number, <X>(X, X) -> X) -> number

        local foo = (nil :: any) :: t
        local bar : u = foo
        )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "substitution_with_bound_table")
{
    CheckResult result = check(R"(
        type A = { x: number }
        local a: A = { x = 1 }
        local b = a
        type B = typeof(b)
        type X<T> = T
        local c: X<B>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "apply_type_function_nested_generics1")
{
    // https://github.com/luau-lang/luau/issues/484
    CheckResult result = check(R"(
        --!strict
        type MyObject = {
            getReturnValue: <V>(cb: () -> V) -> V
        }
        local object: MyObject = {
            getReturnValue = function<U>(cb: () -> U): U
                return cb()
            end,
        }

        type ComplexObject<T> = {
            id: T,
            nested: MyObject
        }

        local complex: ComplexObject<string> = {
            id = "Foo",
            nested = object,
        }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "apply_type_function_nested_generics2")
{
    // https://github.com/luau-lang/luau/issues/484
    CheckResult result = check(R"(
--!strict
type MyObject = {
	getReturnValue: <V>(cb: () -> V) -> V
}
type ComplexObject<T> = {
	id: T,
	nested: MyObject
}

function f(complex: ComplexObject<string>)
    local x = complex.nested.getReturnValue(function(): string
        return ""
    end)

    local y = complex.nested.getReturnValue(function()
        return 3
    end)
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "apply_type_function_nested_generics3")
{
    // This minimization was useful for debugging a particular issue with
    // cyclic types under local type inference.

    CheckResult result = check(R"(
        local getReturnValue: <V>(cb: () -> V) -> V = nil :: any

        local y = getReturnValue(function() return nil :: any end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "quantify_functions_with_no_generics")
{
    CheckResult result = check(R"(
        function foo(f, x)
            return f(x)
        end
    )");

    CHECK("<a, b...>((a) -> (b...), a) -> (b...)" == toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "quantify_functions_even_if_they_have_an_explicit_generic")
{
    CheckResult result = check(R"(
        function foo<X>(f, x: X)
            return f(x)
        end
    )");

    CHECK("<X, a...>((X) -> (a...), X) -> (a...)" == toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "no_extra_quantification_for_generic_functions")
{
    CheckResult result = check(R"(
        function foo<X, Y>(f : (X) -> Y, x: X)
            return f(x)
        end
    )");

    CHECK("<X, Y>((X) -> Y, X) -> Y" == toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "do_not_always_instantiate_generic_intersection_types")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        --!strict
        type Array<T> = { [number]: T }

        type Array_Statics = {
            new: <T>() -> Array<T>,
        }

        local _Arr : Array<any> & Array_Statics = {} :: Array_Statics
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "hof_subtype_instantiation_regression")
{
    CheckResult result = check(R"(
--!strict

local function defaultSort<T>(a: T, b: T)
    return true
end
type A = any
return function<T>(array: {T}): {T}
    table.sort(array, defaultSort)
    return array
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "higher_rank_polymorphism_should_not_accept_instantiated_arguments")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    ScopedFastFlag sffs[] = {
        {FFlag::LuauInstantiateInSubtyping, true},
    };

    CheckResult result = check(R"(
--!strict

local function instantiate(f: <a>(a) -> a): (number) -> number
    return f
end

instantiate(function(x: string) return "foo" end)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto tm1 = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm1);

    CHECK_EQ("<a>(a) -> a", toString(tm1->wantedType));
    CHECK_EQ("<a>(string) -> string", toString(tm1->givenType));
}

TEST_CASE_FIXTURE(Fixture, "bidirectional_checking_and_generalization_play_nice")
{
    CheckResult result = check(R"(
        local foo = function(a)
            return a()
        end

        local a = foo(function() return 1 end)
        local b = foo(function() return "bar" end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("number" == toString(requireType("a")));
    CHECK("string" == toString(requireType("b")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generalization_no_cyclic_intersections")
{
    CheckResult result = check(R"(
        local f, t, n = pairs({"foo"})
        local k, v = f(t)
    )");

    CHECK("({string}, number?) -> (number?, string)" == toString(requireType("f")));
    CHECK("{string}" == toString(requireType("t")));
    CHECK("number?" == toString(requireType("k")));
    CHECK("string" == toString(requireType("v")));
}

TEST_CASE_FIXTURE(Fixture, "missing_generic_type_parameter")
{
    CheckResult result = check(R"(
        function f(x: T): T return x end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    REQUIRE(get<UnknownSymbol>(result.errors[0]));
    REQUIRE(get<UnknownSymbol>(result.errors[1]));
}

TEST_CASE_FIXTURE(Fixture, "generic_implicit_explicit_name_clash")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    auto result = check(R"(
        function apply<a>(func, argument: a)
            return func(argument)
        end
    )");

    CHECK("<a, b...>((a) -> (b...), a) -> (b...)" == toString(requireType("apply")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generic_type_functions_work_in_subtyping")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local function addOne<T>(x: T): add<T, number> return x + 1 end

        local function six(): number
            return addOne(5)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "generic_type_subtyping_nested_bounds_with_new_mappings")
{
    // Test shows how going over mapped generics in a subtyping check can generate more mapped generics when making a subtyping check between bounds.
    // It has previously caused iterator invalidation in the new solver, but this specific test doesn't trigger a UAF, only shows an example.
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
type Dispatch<A> = (A) -> ()
type BasicStateAction<S> = ((S) -> S) | S

function updateReducer<S, I, A>(reducer: (S, A) -> S, initialArg: I, init: ((I) -> S)?): (S, Dispatch<A>)
    return 1 :: any, 2 :: any
end

function basicStateReducer<S>(state: S, action: BasicStateAction<S>): S
    return action
end

function updateState<S>(initialState: (() -> S) | S): (S, Dispatch<BasicStateAction<S>>)
    return updateReducer(basicStateReducer, initialState)
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "generic_type_packs_shouldnt_be_bound_to_themselves")
{
    ScopedFastFlag flags[] = {
        {FFlag::LuauSolverV2, true},
    };

    CheckResult result = check(R"(
export type t1<T...> = {
    foo: (self: t1<T...>, bar: (T...) -> ()) -> ()
}

export type t2<T...> = {
    baz: (self: t2<T...>) -> t1<T...>,
}

export type t3<T...> = {
    f: (self: t3<T...>, T...)->  (),
    g: t1<T...>,
    h: t1<(Player, T...)>
}

local t2 = {}

function t2.new<T...>(): t2<T...>
end

local function create_t3<T...>(): t3<T...>
    local t2_1 = t2.new()
    local t2_2 = t2.new()
    local my_t3 = {
        f = function(_self: t3<T...>, ...: T...) end,
        g = t2_1:baz(),
        h = t2_2:baz()
    }
    return my_t3
end
    )");

    // Note: we just need this test not to crash
    LUAU_REQUIRE_ERROR_COUNT(5, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "follow_bound_type_packs_in_generic_type_visitor")
{
    ScopedFastFlag _{FFlag::LuauContainsAnyGenericFollowBeforeChecking, true};
    // Note: we just need this test not to crash
    check(R"(
function (_(_,_,nil))
(if l0 then typeof else `{_:_()}`,typeof).n0<A...,A...>(l0)
function _:_():typeof<A...>()
end
function _:_().typeof<A...>()
end
end
    )");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generic_packs_in_contravariant_position")
{
    ScopedFastFlag sff{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};

    CheckResult result = check(R"(
function f<A>(foo: (A) -> ()): () end
function g<B...>(...: B...): () end
f(g)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generic_packs_in_contravariant_position_2")
{
    ScopedFastFlag sff{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};

    CheckResult result = check(R"(
function f(foo: (number) -> (number)): () end
type T = <A...>(A...) -> A...
local t: T
f(t)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generic_packs_in_contravariant_position_3")
{
    ScopedFastFlag sff{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};

    CheckResult result = check(R"(
function f(foo: <B...>(B...) -> B...): () end
type T = <A...>(A...) -> A...
local t: T
f(t)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generic_packs_in_contravariant_position_4")
{
    ScopedFastFlag sff1{FFlag::LuauSolverV2, true};
    ScopedFastFlag sff2{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};

    CheckResult result = check(R"(
function f(foo: <A...>(A...) -> A...): () end
type T = <B..., C...>(B...) -> C...
local t: T
f(t)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generic_packs_in_contravariant_position_5")
{
    ScopedFastFlag sff{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};

    CheckResult result = check(R"(
function f(foo: (number) -> number): () end
type T = <A...>(A...) -> number
local t: T
f(t)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generic_packs_in_contravariant_position_6")
{
    ScopedFastFlag sff{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};

    CheckResult result = check(R"(
function f(foo: (...number) -> number): () end
type T = <A...>(A...) -> number
local t: T
f(t)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generic_packs_in_contravariant_position_7")
{
    ScopedFastFlag sff{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};

    CheckResult result = check(R"(
function f(foo: () -> ()): () end
type T = <A...>() -> A...
local t: T
f(t)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generic_packs_in_contravariant_position_8")
{
    ScopedFastFlag sff{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};

    CheckResult result = check(R"(
function f(foo: () -> ()): () end
type T = <A...>(A...) -> A...
local t: T
f(t)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "nested_generic_packs")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};
    ScopedFastFlag sff1{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};

    CheckResult result = check(R"(
type T = <A...>(A...) -> (<A...>(A...) -> ())
type U = (string) -> ((number) -> ())
local t: T
local u: U = t
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "ensure_that_invalid_generic_instantiations_error")
{
    ScopedFastFlag _[] = {{FFlag::LuauSubtypingGenericsDoesntUseVariance, true}, {FFlag::LuauSubtypingReportGenericBoundMismatches2, true}};
    CheckResult res = check(R"(
        local func: <T>(T, (T) -> ()) -> () = nil :: any
        local foobar: (number) -> () = nil :: any
        func({}, foobar)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, res);
    if (FFlag::LuauSolverV2)
        CHECK(get<GenericBoundsMismatch>(res.errors[0]));
    else
        CHECK(get<TypeMismatch>(res.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "ensure_that_invalid_generic_instantiations_error_1")
{
    ScopedFastFlag _[] = {{FFlag::LuauSubtypingGenericsDoesntUseVariance, true}, {FFlag::LuauSubtypingReportGenericBoundMismatches2, true}};
    CheckResult res = check(R"(
        --!strict

        function insert<T>(arr: {T}, value: T)
            return arr
        end

        local a: {number} = {}

        local b = insert(a, "five")
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, res);
    if (FFlag::LuauSolverV2)
        CHECK(get<GenericBoundsMismatch>(res.errors[0]));
    else
        CHECK(get<TypeMismatch>(res.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "xpcall_should_work_with_generics")
{
    ScopedFastFlag _{FFlag::LuauSubtypingGenericPacksDoesntUseVariance, true};

    CheckResult result = check(R"(
--!strict
local v: (number) -> (number) = nil :: any

local x = 3

xpcall(v, print, x)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "array_of_singletons_should_subtype_against_generic_array")
{
    ScopedFastFlag _[] = {
        // These flags expose the issue
        {FFlag::LuauSubtypingReportGenericBoundMismatches2, true},
        {FFlag::DebugLuauStringSingletonBasedOnQuotes, true},
        {FFlag::LuauSubtypingGenericsDoesntUseVariance, true},
        // And this flag fixes it
        {FFlag::LuauSubtypingUnionsAndIntersectionsInGenericBounds, true}
    };
    CheckResult res = check(R"(
        local function a<T>(arr: { T }) end

        a({ 'one', 'two' })
    )");

    LUAU_REQUIRE_NO_ERRORS(res);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "gh1985_array_of_union_for_generic")
{
    ScopedFastFlag _[] = {
        {FFlag::LuauSubtypingReportGenericBoundMismatches2, true},
        {FFlag::LuauSubtypingGenericsDoesntUseVariance, true},
        {FFlag::LuauSubtypingUnionsAndIntersectionsInGenericBounds, true}
    };

    CheckResult res = check(R"(
        local function clear<T>(arr: { T }) table.clear(arr) end
        local a: { true | false }
        -- This obviously shouldn't error, '{ true | false }' should fit '{ T }'
        -- TypeError: The generic type parameter Twas found to have invalid bounds. Its lower bounds were [true, false], and its upper bounds were [true].
        clear(a)
    )");

    LUAU_REQUIRE_NO_ERRORS(res);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "gh1985_array_of_union_for_generic_2")
{
    ScopedFastFlag _[] = {
        {FFlag::LuauSubtypingReportGenericBoundMismatches2, true},
        {FFlag::LuauSubtypingGenericsDoesntUseVariance, true},
        {FFlag::LuauSubtypingUnionsAndIntersectionsInGenericBounds, true}
    };

    CheckResult res = check(R"(
        local function id<T>(arr: { T }): { T } return arr end
        local a: { true | false }
        local b = id(a)
    )");

    LUAU_REQUIRE_NO_ERRORS(res);
}



TEST_SUITE_END();
