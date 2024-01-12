// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Error.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"
#include "Luau/VisitType.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauInstantiateInSubtyping);
LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);
LUAU_FASTFLAG(LuauAlwaysCommitInferencesOfFunctionCalls);
LUAU_FASTINT(LuauTarjanChildLimit);

TEST_SUITE_BEGIN("TypeInferFunctions");

TEST_CASE_FIXTURE(Fixture, "tc_function")
{
    CheckResult result = check("function five() return 5 end");
    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionType* fiveType = get<FunctionType>(requireType("five"));
    REQUIRE(fiveType != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "check_function_bodies")
{
    CheckResult result = check("function myFunction()    local a = 0    a = true    end");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(result.errors[0], (TypeError{Location{Position{0, 44}, Position{0, 48}}, TypeMismatch{
                                                                                          builtinTypes->numberType,
                                                                                          builtinTypes->booleanType,
                                                                                      }}));
}

TEST_CASE_FIXTURE(Fixture, "cannot_hoist_interior_defns_into_signature")
{
    // This test verifies that the signature does not have access to types
    // declared within the body. Under DCR, if the function's inner scope
    // encompasses the entire function expression, it would be possible for this
    // to type check (but the solver output is somewhat undefined). This test
    // ensures that this isn't the case.
    CheckResult result = check(R"(
        local function f(x: T)
            type T = number
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(result.errors[0] == TypeError{Location{{1, 28}, {1, 29}}, getMainSourceModule()->name,
                                  UnknownSymbol{
                                      "T",
                                      UnknownSymbol::Context::Type,
                                  }});
}

TEST_CASE_FIXTURE(Fixture, "infer_return_type")
{
    CheckResult result = check("function take_five() return 5 end");
    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionType* takeFiveType = get<FunctionType>(requireType("take_five"));
    REQUIRE(takeFiveType != nullptr);

    std::vector<TypeId> retVec = flatten(takeFiveType->retTypes).first;
    REQUIRE(!retVec.empty());

    REQUIRE_EQ(*follow(retVec[0]), *builtinTypes->numberType);
}

TEST_CASE_FIXTURE(Fixture, "infer_from_function_return_type")
{
    CheckResult result = check("function take_five() return 5 end    local five = take_five()");
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*builtinTypes->numberType, *follow(requireType("five")));
}

TEST_CASE_FIXTURE(Fixture, "infer_that_function_does_not_return_a_table")
{
    CheckResult result = check(R"(
        function take_five()
            return 5
        end

        take_five().prop = 888
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(result.errors[0], (TypeError{Location{Position{5, 8}, Position{5, 24}}, NotATable{builtinTypes->numberType}}));
}

TEST_CASE_FIXTURE(Fixture, "generalize_table_property")
{
    CheckResult result = check(R"(
        local T = {}

        T.foo = function(x)
            return x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId t = requireType("T");
    const TableType* tt = get<TableType>(follow(t));
    REQUIRE(tt);

    TypeId fooTy = tt->props.at("foo").type();
    CHECK("<a>(a) -> a" == toString(fooTy));
}

TEST_CASE_FIXTURE(Fixture, "vararg_functions_should_allow_calls_of_any_types_and_size")
{
    CheckResult result = check(R"(
        function f(...) end

        f(1)
        f("foo", 2)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "vararg_function_is_quantified")
{
    CheckResult result = check(R"(
        local T = {}
        function T.f(...)
            local result = {}

            for i = 1, select("#", ...) do
                local dictionary = select(i, ...)
                for key, value in pairs(dictionary) do
                    result[key] = value
                end
            end

            return result
        end

        return T
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    auto r = first(getMainModule()->returnType);
    REQUIRE(r);

    TableType* ttv = getMutable<TableType>(*r);
    REQUIRE(ttv);

    REQUIRE(ttv->props.count("f"));
    TypeId k = ttv->props["f"].type();
    REQUIRE(k);
}

TEST_CASE_FIXTURE(Fixture, "list_only_alternative_overloads_that_match_argument_count")
{
    CheckResult result = check(R"(
        local multiply: ((number)->number) & ((number)->string) & ((number, number)->number)
        multiply("")
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        GenericError* g = get<GenericError>(result.errors[0]);
        REQUIRE(g);
        CHECK(g->message == "None of the overloads for function that accept 1 arguments are compatible.");
    }
    else
    {
        TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
        REQUIRE(tm);
        CHECK_EQ(builtinTypes->numberType, tm->wantedType);
        CHECK_EQ(builtinTypes->stringType, tm->givenType);
    }

    ExtraInformation* ei = get<ExtraInformation>(result.errors[1]);
    REQUIRE(ei);

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK("Available overloads: (number) -> number; and (number) -> string" == ei->message);
    else
        CHECK_EQ("Other overloads are also not viable: (number) -> string", ei->message);
}

TEST_CASE_FIXTURE(Fixture, "list_all_overloads_if_no_overload_takes_given_argument_count")
{
    CheckResult result = check(R"(
        local multiply: ((number)->number) & ((number)->string) & ((number, number)->number)
        multiply()
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    GenericError* ge = get<GenericError>(result.errors[0]);
    REQUIRE(ge);
    CHECK_EQ("No overload for function accepts 0 arguments.", ge->message);

    ExtraInformation* ei = get<ExtraInformation>(result.errors[1]);
    REQUIRE(ei);
    CHECK_EQ("Available overloads: (number) -> number; (number) -> string; and (number, number) -> number", ei->message);
}

TEST_CASE_FIXTURE(Fixture, "dont_give_other_overloads_message_if_only_one_argument_matching_overload_exists")
{
    CheckResult result = check(R"(
        local multiply: ((number)->number) & ((number)->string) & ((number, number)->number)
        multiply(1, "")
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ(builtinTypes->numberType, tm->wantedType);
    CHECK_EQ(builtinTypes->stringType, tm->givenType);
}

TEST_CASE_FIXTURE(Fixture, "infer_return_type_from_selected_overload")
{
    CheckResult result = check(R"(
        type T = {method: ((T, number) -> number) & ((number) -> string)}
        local T: T

        local a = T.method(T, 4)
        local b = T.method(5)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("number", toString(requireType("a")));
    CHECK_EQ("string", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "too_many_arguments")
{
    CheckResult result = check(R"(
        --!nonstrict

        function g(a: number) end

        g()

    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto err = result.errors[0];
    auto acm = get<CountMismatch>(err);
    REQUIRE(acm);

    CHECK_EQ(1, acm->expected);
    CHECK_EQ(0, acm->actual);
}

TEST_CASE_FIXTURE(Fixture, "too_many_arguments_error_location")
{
    CheckResult result = check(R"(
        --!strict

        function myfunction(a: number, b:number) end
        myfunction(1)

        function getmyfunction()
            return myfunction
        end
        getmyfunction()()
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    {
        TypeError err = result.errors[0];

        // Ensure the location matches the location of the function identifier
        CHECK_EQ(err.location, Location(Position(4, 8), Position(4, 18)));

        auto acm = get<CountMismatch>(err);
        REQUIRE(acm);
        CHECK_EQ(2, acm->expected);
        CHECK_EQ(1, acm->actual);
    }
    {
        TypeError err = result.errors[1];

        // Ensure the location matches the location of the expression returning the function
        CHECK_EQ(err.location, Location(Position(9, 8), Position(9, 23)));

        auto acm = get<CountMismatch>(err);
        REQUIRE(acm);
        CHECK_EQ(2, acm->expected);
        CHECK_EQ(0, acm->actual);
    }
}

TEST_CASE_FIXTURE(Fixture, "recursive_function")
{
    CheckResult result = check(R"(
        function count(n: number)
            if n == 0 then
                return 0
            else
                return count(n - 1)
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "lambda_form_of_local_function_cannot_be_recursive")
{
    CheckResult result = check(R"(
        local f = function() return f() end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "recursive_local_function")
{
    CheckResult result = check(R"(
        local function count(n: number)
            if n == 0 then
                return 0
            else
                return count(n - 1)
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

// FIXME: This and the above case get handled very differently.  It's pretty dumb.
// We really should unify the two code paths, probably by deleting AstStatFunction.
TEST_CASE_FIXTURE(Fixture, "another_recursive_local_function")
{
    CheckResult result = check(R"(
        local count
        function count(n: number)
            if n == 0 then
                return 0
            else
                return count(n - 1)
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

// We had a bug where we'd look up the type of a recursive call using the DFG,
// not the bindings tables.  As a result, we would erroneously use the
// generalized type of foo() in this recursive fragment.  This creates a
// constraint cycle that doesn't always work itself out.
//
// The fix is for the DFG node within the scope of foo() to retain the
// ungeneralized type of foo.
TEST_CASE_FIXTURE(BuiltinsFixture, "recursive_calls_must_refer_to_the_ungeneralized_type")
{
    CheckResult result = check(R"(
        function foo()
            string.format('%s: %s', "51", foo())
        end
    )");
}

TEST_CASE_FIXTURE(Fixture, "cyclic_function_type_in_rets")
{
    CheckResult result = check(R"(
        function f()
            return f
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("t1 where t1 = () -> t1", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "another_higher_order_function")
{
    CheckResult result = check(R"(
        local Get_des
        function Get_des(func)
            Get_des(func)
        end

        local function f(d)
            d:IsA("BasePart")
            d.Parent:FindFirstChild("Humanoid")
            d:IsA("Decal")
        end
        Get_des(f)

    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "another_other_higher_order_function")
{
    CheckResult result = check(R"(
        local d
        d:foo()
        d:foo()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "local_function")
{
    CheckResult result = check(R"(
        function f()
            return 8
        end

        function g()
            local function f()
                return 'hello'
            end
            return f
        end

        local h = g()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId h = follow(requireType("h"));

    const FunctionType* ftv = get<FunctionType>(h);
    REQUIRE(ftv != nullptr);

    std::optional<TypeId> rt = first(ftv->retTypes);
    REQUIRE(bool(rt));

    TypeId retType = follow(*rt);
    CHECK_EQ(PrimitiveType::String, getPrimitiveType(retType));
}

TEST_CASE_FIXTURE(Fixture, "func_expr_doesnt_leak_free")
{
    CheckResult result = check(R"(
        local p = function(x) return x end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    const Luau::FunctionType* fn = get<FunctionType>(requireType("p"));
    REQUIRE(fn);
    auto ret = first(fn->retTypes);
    REQUIRE(ret);
    REQUIRE(get<GenericType>(follow(*ret)));
}

TEST_CASE_FIXTURE(Fixture, "first_argument_can_be_optional")
{
    CheckResult result = check(R"(
        local T = {}
        function T.new(a: number?, b: number?, c: number?) return 5 end
        local m = T.new()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);
}

TEST_CASE_FIXTURE(Fixture, "it_is_ok_not_to_supply_enough_retvals")
{
    CheckResult result = check(R"(
        function get_two() return 5, 6 end

        local a = get_two()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);
}

TEST_CASE_FIXTURE(Fixture, "duplicate_functions2")
{
    CheckResult result = check(R"(
        function foo() end

        function bar()
            local function foo() end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(0, result);
}

TEST_CASE_FIXTURE(Fixture, "duplicate_functions_allowed_in_nonstrict")
{
    CheckResult result = check(R"(
        --!nonstrict
        function foo() end

        function foo() end

        function bar()
            local function foo() end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "duplicate_functions_with_different_signatures_not_allowed_in_nonstrict")
{
    CheckResult result = check(R"(
        --!nonstrict
        function foo(): number
            return 1
        end
        foo()

        function foo(n: number): number
            return 2
        end
        foo()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("() -> number", toString(tm->wantedType));
    CHECK_EQ("(number) -> number", toString(tm->givenType));
}

TEST_CASE_FIXTURE(Fixture, "complicated_return_types_require_an_explicit_annotation")
{
    CheckResult result = check(R"(
        local i = 0
        function most_of_the_natural_numbers(): number?
            if i < 10 then
                i = i + 1
                return i
            else
                return nil
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId ty = requireType("most_of_the_natural_numbers");
    const FunctionType* functionType = get<FunctionType>(ty);
    REQUIRE_MESSAGE(functionType, "Expected function but got " << toString(ty));

    std::optional<TypeId> retType = first(functionType->retTypes);
    REQUIRE(retType);
    CHECK(get<UnionType>(*retType));
}

TEST_CASE_FIXTURE(Fixture, "infer_higher_order_function")
{
    CheckResult result = check(R"(
        function apply(f, x)
            return f(x)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionType* ftv = get<FunctionType>(requireType("apply"));
    REQUIRE(ftv != nullptr);

    std::vector<TypeId> argVec = flatten(ftv->argTypes).first;

    REQUIRE_EQ(2, argVec.size());

    const FunctionType* fType = get<FunctionType>(follow(argVec[0]));
    REQUIRE_MESSAGE(fType != nullptr, "Expected a function but got " << toString(argVec[0]));

    std::vector<TypeId> fArgs = flatten(fType->argTypes).first;

    TypeId xType = follow(argVec[1]);

    CHECK_EQ(1, fArgs.size());
    CHECK_EQ(xType, follow(fArgs[0]));
}

TEST_CASE_FIXTURE(Fixture, "higher_order_function_2")
{
    CheckResult result = check(R"(
        function bottomupmerge(comp, a, b, left, mid, right)
            local i, j = left, mid
            for k = left, right do
                if i < mid and (j > right or not comp(a[j], a[i])) then
                    b[k] = a[i]
                    i = i + 1
                else
                    b[k] = a[j]
                    j = j + 1
                end
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionType* ftv = get<FunctionType>(requireType("bottomupmerge"));
    REQUIRE(ftv != nullptr);

    std::vector<TypeId> argVec = flatten(ftv->argTypes).first;

    REQUIRE_EQ(6, argVec.size());

    const FunctionType* fType = get<FunctionType>(follow(argVec[0]));
    REQUIRE(fType != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "higher_order_function_3")
{
    CheckResult result = check(R"(
        function swap(p)
            local t = p[0]
            p[0] = p[1]
            p[1] = t
            return nil
        end

        function swapTwice(p)
            swap(p)
            swap(p)
            return p
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionType* ftv = get<FunctionType>(requireType("swapTwice"));
    REQUIRE(ftv != nullptr);

    std::vector<TypeId> argVec = flatten(ftv->argTypes).first;

    REQUIRE_EQ(1, argVec.size());

    const TableType* argType = get<TableType>(follow(argVec[0]));
    REQUIRE(argType != nullptr);

    CHECK(bool(argType->indexer));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "higher_order_function_4")
{
    CheckResult result = check(R"(
        function bottomupmerge(comp, a, b, left, mid, right)
            local i, j = left, mid
            for k = left, right do
                if i < mid and (j > right or not comp(a[j], a[i])) then
                    b[k] = a[i]
                    i = i + 1
                else
                    b[k] = a[j]
                    j = j + 1
                end
            end
        end

        function mergesort(arr, comp)
            local work = {}
            for i = 1, #arr do
                work[i] = arr[i]
            end
            local width = 1
            while width < #arr do
                for i = 1, #arr, 2*width do
                    bottomupmerge(comp, arr, work, i, math.min(i+width, #arr), math.min(i+2*width-1, #arr))
                end
                local temp = work
                work = arr
                arr = temp
                width = width * 2
            end
            return arr
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    /*
     * mergesort takes two arguments: an array of some type T and a function that takes two Ts.
     * We must assert that these two types are in fact the same type.
     * In other words, comp(arr[x], arr[y]) is well-typed.
     */

    const FunctionType* ftv = get<FunctionType>(requireType("mergesort"));
    REQUIRE(ftv != nullptr);

    std::vector<TypeId> argVec = flatten(ftv->argTypes).first;

    REQUIRE_EQ(2, argVec.size());

    const TableType* arg0 = get<TableType>(follow(argVec[0]));
    REQUIRE(arg0 != nullptr);
    REQUIRE(bool(arg0->indexer));

    const FunctionType* arg1 = get<FunctionType>(follow(argVec[1]));
    REQUIRE(arg1 != nullptr);
    REQUIRE_EQ(2, size(arg1->argTypes));

    std::vector<TypeId> arg1Args = flatten(arg1->argTypes).first;

    CHECK_EQ(*arg0->indexer->indexResultType, *arg1Args[0]);
    CHECK_EQ(*arg0->indexer->indexResultType, *arg1Args[1]);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "mutual_recursion")
{
    CheckResult result = check(R"(
        --!strict

        function newPlayerCharacter()
            startGui() -- Unknown symbol 'startGui'
        end

        local characterAddedConnection: any
        function startGui()
            characterAddedConnection = game:GetService("Players").LocalPlayer.CharacterAdded:connect(newPlayerCharacter)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "toposort_doesnt_break_mutual_recursion")
{
    CheckResult result = check(R"(
        --!strict
        local x = nil
        function f() g() end
        -- make sure print(x) doesn't get toposorted here, breaking the mutual block
        function g() x = f end
        print(x)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);
}

TEST_CASE_FIXTURE(Fixture, "check_function_before_lambda_that_uses_it")
{
    CheckResult result = check(R"(
        --!nonstrict

        function f()
            return 114
        end

        return function()
            return f():andThen()
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "it_is_ok_to_oversaturate_a_higher_order_function_argument")
{
    CheckResult result = check(R"(
        function onerror() end
        function foo() end
        xpcall(foo, onerror)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "another_indirect_function_case_where_it_is_ok_to_provide_too_many_arguments")
{
    CheckResult result = check(R"(
        local mycb: (number, number) -> ()

        function f() end

        mycb = f
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "report_exiting_without_return_nonstrict")
{
    CheckResult result = check(R"(
        --!nonstrict

        local function f1(v): number?
            if v then
                return 1
            end
        end

        local function f2(v)
            if v then
                return 1
            end
        end

        local function f3(v): ()
            if v then
                return
            end
        end

        local function f4(v)
            if v then
                return
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    FunctionExitsWithoutReturning* err = get<FunctionExitsWithoutReturning>(result.errors[0]);
    CHECK(err);
}

TEST_CASE_FIXTURE(Fixture, "report_exiting_without_return_strict")
{
    CheckResult result = check(R"(
        --!strict

        local function f1(v): number?
            if v then
                return 1
            end
        end

        local function f2(v)
            if v then
                return 1
            end
        end

        local function f3(v): ()
            if v then
                return
            end
        end

        local function f4(v)
            if v then
                return
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    FunctionExitsWithoutReturning* annotatedErr = get<FunctionExitsWithoutReturning>(result.errors[0]);
    CHECK(annotatedErr);

    FunctionExitsWithoutReturning* inferredErr = get<FunctionExitsWithoutReturning>(result.errors[1]);
    CHECK(inferredErr);
}

TEST_CASE_FIXTURE(Fixture, "calling_function_with_incorrect_argument_type_yields_errors_spanning_argument")
{
    CheckResult result = check(R"(
        function foo(a: number, b: string) end

        foo("Test", 123)
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK_EQ(result.errors[0], (TypeError{Location{Position{3, 12}, Position{3, 18}}, TypeMismatch{
                                                                                          builtinTypes->numberType,
                                                                                          builtinTypes->stringType,
                                                                                      }}));

    CHECK_EQ(result.errors[1], (TypeError{Location{Position{3, 20}, Position{3, 23}}, TypeMismatch{
                                                                                          builtinTypes->stringType,
                                                                                          builtinTypes->numberType,
                                                                                      }}));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "calling_function_with_anytypepack_doesnt_leak_free_types")
{
    CheckResult result = check(R"(
        --!nonstrict

        function Test(a)
            return 1, ""
        end


        local tab = {}
        table.insert(tab, Test(1));
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    ToStringOptions opts;
    opts.exhaustive = true;
    opts.maxTableLength = 0;

    CHECK_EQ("{any}", toString(requireType("tab"), opts));
}

TEST_CASE_FIXTURE(Fixture, "too_many_return_values")
{
    CheckResult result = check(R"(
        --!strict

        function f()
            return 55
        end

        local a, b = f()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CountMismatch* acm = get<CountMismatch>(result.errors[0]);
    REQUIRE(acm);
    CHECK_EQ(acm->context, CountMismatch::FunctionResult);
    CHECK_EQ(acm->expected, 1);
    CHECK_EQ(acm->actual, 2);
}

TEST_CASE_FIXTURE(Fixture, "too_many_return_values_in_parentheses")
{
    CheckResult result = check(R"(
        --!strict

        function f()
            return 55
        end

        local a, b = (f())
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CountMismatch* acm = get<CountMismatch>(result.errors[0]);
    REQUIRE(acm);
    CHECK_EQ(acm->context, CountMismatch::FunctionResult);
    CHECK_EQ(acm->expected, 1);
    CHECK_EQ(acm->actual, 2);
}

TEST_CASE_FIXTURE(Fixture, "too_many_return_values_no_function")
{
    CheckResult result = check(R"(
        --!strict

        local a, b = 55
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CountMismatch* acm = get<CountMismatch>(result.errors[0]);
    REQUIRE(acm);
    CHECK_EQ(acm->context, CountMismatch::ExprListResult);
    CHECK_EQ(acm->expected, 1);
    CHECK_EQ(acm->actual, 2);
}

TEST_CASE_FIXTURE(Fixture, "ignored_return_values")
{
    CheckResult result = check(R"(
        --!strict

        function f()
            return 55, ""
        end

        local a = f()
    )");

    LUAU_REQUIRE_ERROR_COUNT(0, result);
}

TEST_CASE_FIXTURE(Fixture, "function_does_not_return_enough_values")
{
    CheckResult result = check(R"(
        --!strict

        function f(): (number, string)
            return 55
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CountMismatch* acm = get<CountMismatch>(result.errors[0]);
    REQUIRE(acm);
    CHECK_EQ(acm->context, CountMismatch::Return);
    CHECK_EQ(acm->expected, 2);
    CHECK_EQ(acm->actual, 1);
}

TEST_CASE_FIXTURE(Fixture, "function_cast_error_uses_correct_language")
{
    CheckResult result = check(R"(
        function foo(a, b): number
            return 0
        end

        local a: (string)->number = foo
        local b: (number, number)->(number, number) = foo

        local c: (string, number)->number = foo -- no error
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    auto tm1 = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm1);

    CHECK_EQ("(string) -> number", toString(tm1->wantedType));
    CHECK_EQ("(string, *error-type*) -> number", toString(tm1->givenType));

    auto tm2 = get<TypeMismatch>(result.errors[1]);
    REQUIRE(tm2);

    CHECK_EQ("(number, number) -> (number, number)", toString(tm2->wantedType));
    CHECK_EQ("(string, *error-type*) -> number", toString(tm2->givenType));
}

TEST_CASE_FIXTURE(Fixture, "no_lossy_function_type")
{
    CheckResult result = check(R"(
        --!strict
        local tbl = {}
        function tbl:abc(a: number, b: number)
            return a
        end
        tbl:abc(1, 2) -- Line 6
        --   | Column 14
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    TypeId type = requireTypeAtPosition(Position(6, 14));
    CHECK_EQ("(tbl, number, number) -> number", toString(type));
    auto ftv = get<FunctionType>(follow(type));
    REQUIRE(ftv);
    CHECK(ftv->hasSelf);
}

TEST_CASE_FIXTURE(Fixture, "record_matching_overload")
{
    CheckResult result = check(R"(
        type Overload = ((string) -> string) & ((number) -> number)
        local abc: Overload
        abc(1)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // AstExprCall is the node that has the overload stored on it.
    // findTypeAtPosition will look at the AstExprLocal, but this is not what
    // we want to look at.
    std::vector<AstNode*> ancestry = findAstAncestryOfPosition(*getMainSourceModule(), Position(3, 10));
    REQUIRE_GE(ancestry.size(), 2);
    AstExpr* parentExpr = ancestry[ancestry.size() - 2]->asExpr();
    REQUIRE(bool(parentExpr));
    REQUIRE(parentExpr->is<AstExprCall>());

    ModulePtr module = getMainModule();
    auto it = module->astOverloadResolvedTypes.find(parentExpr);
    REQUIRE(it);
    CHECK_EQ(toString(*it), "(number) -> number");
}

TEST_CASE_FIXTURE(Fixture, "return_type_by_overload")
{
    CheckResult result = check(R"(
        type Overload = ((string) -> string) & ((number, number) -> number)
        local abc: Overload
        local x = abc(true)
        local y = abc(true,true)
        local z = abc(true,true,true)
    )");

    LUAU_REQUIRE_ERRORS(result);
    CHECK_EQ("string", toString(requireType("x")));
    CHECK_EQ("number", toString(requireType("y")));
    // Should this be string|number?
    CHECK_EQ("string", toString(requireType("z")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "infer_anonymous_function_arguments")
{
    // Simple direct arg to arg propagation
    CheckResult result = check(R"(
type Table = { x: number, y: number }
local function f(a: (Table) -> number) return a({x = 1, y = 2}) end
f(function(a) return a.x + a.y end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // An optional function is accepted, but since we already provide a function, nil can be ignored
    result = check(R"(
type Table = { x: number, y: number }
local function f(a: ((Table) -> number)?) if a then return a({x = 1, y = 2}) else return 0 end end
f(function(a) return a.x + a.y end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // Make sure self calls match correct index
    result = check(R"(
type Table = { x: number, y: number }
local x = {}
x.b = {x = 1, y = 2}
function x:f(a: (Table) -> number) return a(self.b) end
x:f(function(a) return a.x + a.y end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // Mix inferred and explicit argument types
    result = check(R"(
function f(a: (a: number, b: number, c: boolean) -> number) return a(1, 2, true) end
f(function(a: number, b, c) return c and a + b or b - a end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // Anonymous function has a variadic pack
    result = check(R"(
type Table = { x: number, y: number }
local function f(a: (Table) -> number) return a({x = 1, y = 2}) end
f(function(...) return select(1, ...).z end)
    )");

    LUAU_REQUIRE_ERRORS(result);
    CHECK_EQ("Key 'z' not found in table 'Table'", toString(result.errors[0]));

    // Can't accept more arguments than provided
    result = check(R"(
function f(a: (a: number, b: number) -> number) return a(1, 2) end
f(function(a, b, c, ...) return a + b end)
    )");

    LUAU_REQUIRE_ERRORS(result);

    std::string expected;
    if (FFlag::LuauInstantiateInSubtyping)
    {
        expected = R"(Type
    '<a>(number, number, a) -> number'
could not be converted into
    '(number, number) -> number'
caused by:
  Argument count mismatch. Function expects 3 arguments, but only 2 are specified)";
    }
    else
    {
        expected = R"(Type
    '(number, number, a) -> number'
could not be converted into
    '(number, number) -> number'
caused by:
  Argument count mismatch. Function expects 3 arguments, but only 2 are specified)";
    }

    CHECK_EQ(expected, toString(result.errors[0]));

    // Infer from variadic packs into elements
    result = check(R"(
function f(a: (...number) -> number) return a(1, 2) end
f(function(a, b) return a + b end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // Infer from variadic packs into variadic packs
    result = check(R"(
type Table = { x: number, y: number }
function f(a: (...Table) -> number) return a({x = 1, y = 2}, {x = 3, y = 4}) end
f(function(a, ...) local b = ... return b.z end)
    )");

    LUAU_REQUIRE_ERRORS(result);
    CHECK_EQ("Key 'z' not found in table 'Table'", toString(result.errors[0]));

    // Return type inference
    result = check(R"(
type Table = { x: number, y: number }
function f(a: (number) -> Table) return a(4) end
f(function(x) return x * 2 end)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type 'number' could not be converted into 'Table'", toString(result.errors[0]));

    // Return type doesn't inference 'nil'
    result = check(R"(
        function f(a: (number) -> nil) return a(4) end
        f(function(x) print(x) end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "infer_generic_function_function_argument")
{
    CheckResult result = check(R"(
local function sum<a>(x: a, y: a, f: (a, a) -> a) return f(x, y) end
return sum(2, 3, function(a, b) return a + b end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    result = check(R"(
local function map<a, b>(arr: {a}, f: (a) -> b) local r = {} for i,v in ipairs(arr) do table.insert(r, f(v)) end return r end
local a = {1, 2, 3}
local r = map(a, function(a) return a + a > 100 end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    REQUIRE_EQ("{boolean}", toString(requireType("r")));

    check(R"(
local function foldl<a, b>(arr: {a}, init: b, f: (b, a) -> b) local r = init for i,v in ipairs(arr) do r = f(r, v) end return r end
local a = {1, 2, 3}
local r = foldl(a, {s=0,c=0}, function(a, b) return {s = a.s + b, c = a.c + 1} end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    REQUIRE_EQ("{ c: number, s: number }", toString(requireType("r")));
}

TEST_CASE_FIXTURE(Fixture, "infer_generic_function_function_argument_overloaded")
{
    CheckResult result = check(R"(
local function g1<T>(a: T, f: (T) -> T) return f(a) end
local function g2<T>(a: T, b: T, f: (T, T) -> T) return f(a, b) end

local g12: typeof(g1) & typeof(g2)

g12(1, function(x) return x + x end)
g12(1, 2, function(x, y) return x + y end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    result = check(R"(
local function g1<T>(a: T, f: (T) -> T) return f(a) end
local function g2<T>(a: T, b: T, f: (T, T) -> T) return f(a, b) end

local g12: typeof(g1) & typeof(g2)

g12({x=1}, function(x) return {x=-x.x} end)
g12({x=1}, {x=2}, function(x, y) return {x=x.x + y.x} end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "infer_generic_lib_function_function_argument")
{
    CheckResult result = check(R"(
local a = {{x=4}, {x=7}, {x=1}}
table.sort(a, function(x, y) return x.x < y.x end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "variadic_any_is_compatible_with_a_generic_TypePack")
{
    CheckResult result = check(R"(
        --!strict
        local function f(...) return ... end
        local g = function(...) return f(...) end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

// https://github.com/luau-lang/luau/issues/767
TEST_CASE_FIXTURE(BuiltinsFixture, "variadic_any_is_compatible_with_a_generic_TypePack_2")
{
    CheckResult result = check(R"(
        local function somethingThatsAny(...: any)
            print(...)
        end

        local function x<T...>(...: T...)
            somethingThatsAny(...) -- Failed to unify variadic type packs
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "infer_anonymous_function_arguments_outside_call")
{
    CheckResult result = check(R"(
type Table = { x: number, y: number }
local f: (Table) -> number = function(t) return t.x + t.y end

type TableWithFunc = { x: number, y: number, f: (number, number) -> number }
local a: TableWithFunc = { x = 3, y = 4, f = function(a, b) return a + b end }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "infer_return_value_type")
{
    CheckResult result = check(R"(
local function f(): {string|number}
    return {1, "b", 3}
end

local function g(): (number, {string|number})
    return 4, {1, "b", 3}
end

local function h(): ...{string|number}
    return {4}, {1, "b", 3}, {"s"}
end

local function i(): ...{string|number}
    return {1, "b", 3}, h()
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_function_mismatch_arg_count")
{
    CheckResult result = check(R"(
type A = (number, number) -> string
type B = (number) -> string

local a: A
local b: B = a
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '(number, number) -> string'
could not be converted into
    '(number) -> string'
caused by:
  Argument count mismatch. Function expects 2 arguments, but only 1 is specified)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_function_mismatch_arg")
{
    CheckResult result = check(R"(
type A = (number, number) -> string
type B = (number, string) -> string

local a: A
local b: B = a
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '(number, number) -> string'
could not be converted into
    '(number, string) -> string'
caused by:
  Argument #2 type is not compatible.
Type 'string' could not be converted into 'number')";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_function_mismatch_ret_count")
{
    CheckResult result = check(R"(
type A = (number, number) -> (number)
type B = (number, number) -> (number, boolean)

local a: A
local b: B = a
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '(number, number) -> number'
could not be converted into
    '(number, number) -> (number, boolean)'
caused by:
  Function only returns 1 value, but 2 are required here)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_function_mismatch_ret")
{
    CheckResult result = check(R"(
type A = (number, number) -> string
type B = (number, number) -> number

local a: A
local b: B = a
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '(number, number) -> string'
could not be converted into
    '(number, number) -> number'
caused by:
  Return type is not compatible.
Type 'string' could not be converted into 'number')";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_function_mismatch_ret_mult")
{
    CheckResult result = check(R"(
type A = (number, number) -> (number, string)
type B = (number, number) -> (number, boolean)

local a: A
local b: B = a
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '(number, number) -> (number, string)'
could not be converted into
    '(number, number) -> (number, boolean)'
caused by:
  Return #2 type is not compatible.
Type 'string' could not be converted into 'boolean')";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "function_decl_quantify_right_type")
{
    fileResolver.source["game/isAMagicMock"] = R"(
--!nonstrict
return function(value)
    return false
end
    )";

    CheckResult result = check(R"(
--!nonstrict
local MagicMock = {}
MagicMock.is = require(game.isAMagicMock)

function MagicMock.is(value)
    return false
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "function_decl_non_self_sealed_overwrite")
{
    CheckResult result = check(R"(
        function string.len(): number
            return 1
        end

        local s = string
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // if 'string' library property was replaced with an internal module type, it will be freed and the next check will crash
    frontend.clear();

    CheckResult result2 = check(R"(
        print(string.len('hello'))
    )");

    LUAU_REQUIRE_NO_ERRORS(result2);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "function_decl_non_self_sealed_overwrite_2")
{
    CheckResult result = check(R"(
local t: { f: ((x: number) -> number)? } = {}

function t.f(x)
    print(x + 5)
    return x .. "asd" -- 1st error: we know that return type is a number, not a string
end

t.f = function(x)
    print(x + 5)
    return x .. "asd" -- 2nd error: we know that return type is a number, not a string
end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ(toString(result.errors[0]), R"(Type 'string' could not be converted into 'number')");
    CHECK_EQ(toString(result.errors[1]), R"(Type 'string' could not be converted into 'number')");
}

TEST_CASE_FIXTURE(Fixture, "inferred_higher_order_functions_are_quantified_at_the_right_time2")
{
    CheckResult result = check(R"(
        --!strict

        local function resolveDispatcher()
            return (nil :: any) :: {useContext: (number?) -> any}
        end

        local useContext
        useContext = function(unstable_observedBits: number?)
            resolveDispatcher().useContext(unstable_observedBits)
        end
    )");

    // LUAU_REQUIRE_NO_ERRORS is particularly unhelpful when this test is broken.
    // You get a TypeMismatch error where both types stringify the same.

    CHECK(result.errors.empty());
    if (!result.errors.empty())
    {
        for (const auto& e : result.errors)
            printf("%s %s: %s\n", e.moduleName.c_str(), toString(e.location).c_str(), toString(e).c_str());
    }
}

TEST_CASE_FIXTURE(Fixture, "inferred_higher_order_functions_are_quantified_at_the_right_time3")
{
    CheckResult result = check(R"(
        local foo

        foo():bar(function()
            return foo()
        end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "function_decl_non_self_unsealed_overwrite")
{
    CheckResult result = check(R"(
local t = { f = nil :: ((x: number) -> number)? }

function t.f(x: string): string -- 1st error: new function value type is incompatible
    return x .. "asd"
end

t.f = function(x)
    print(x + 5)
    return x .. "asd" -- 2nd error: we know that return type is a number, not a string
end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ(toString(result.errors[0]), R"(Type
    '(string) -> string'
could not be converted into
    '((number) -> number)?'
caused by:
  None of the union options are compatible. For example:
Type
    '(string) -> string'
could not be converted into
    '(number) -> number'
caused by:
  Argument #1 type is not compatible.
Type 'number' could not be converted into 'string')");
    CHECK_EQ(toString(result.errors[1]), R"(Type 'string' could not be converted into 'number')");
}

TEST_CASE_FIXTURE(Fixture, "strict_mode_ok_with_missing_arguments")
{
    CheckResult result = check(R"(
        local function f(x: any) end
        f()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "function_statement_sealed_table_assignment_through_indexer")
{
    CheckResult result = check(R"(
local t: {[string]: () -> number} = {}

function t.a() return 1 end -- OK
function t:b() return 2 end -- not OK
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(R"(Type
    '(*error-type*) -> number'
could not be converted into
    '() -> number'
caused by:
  Argument count mismatch. Function expects 1 argument, but none are specified)",
        toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "too_few_arguments_variadic")
{
    CheckResult result = check(R"(
    function test(a: number, b: string, ...)
    end

    test(1)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto err = result.errors[0];
    auto acm = get<CountMismatch>(err);
    REQUIRE(acm);

    CHECK_EQ(2, acm->expected);
    CHECK_EQ(1, acm->actual);
    CHECK_EQ(CountMismatch::Context::Arg, acm->context);
    CHECK(acm->isVariadic);
}

TEST_CASE_FIXTURE(Fixture, "too_few_arguments_variadic_generic")
{
    CheckResult result = check(R"(
function test(a: number, b: string, ...)
    return 1
end

function wrapper<A...>(f: (A...) -> number, ...: A...)
end

wrapper(test)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto err = result.errors[0];
    auto acm = get<CountMismatch>(err);
    REQUIRE(acm);

    CHECK_EQ(3, acm->expected);
    CHECK_EQ(1, acm->actual);
    CHECK_EQ(CountMismatch::Context::Arg, acm->context);
    CHECK(acm->isVariadic);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "too_few_arguments_variadic_generic2")
{
    CheckResult result = check(R"(
function test(a: number, b: string, ...)
    return 1
end

function wrapper<A...>(f: (A...) -> number, ...: A...)
end

pcall(wrapper, test)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto err = result.errors[0];
    auto acm = get<CountMismatch>(err);
    REQUIRE(acm);

    CHECK_EQ(4, acm->expected);
    CHECK_EQ(2, acm->actual);
    CHECK_EQ(CountMismatch::Context::Arg, acm->context);
    CHECK(acm->isVariadic);
}

TEST_CASE_FIXTURE(Fixture, "occurs_check_failure_in_function_return_type")
{
    CheckResult result = check(R"(
        function f()
            return 5, f()
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK(nullptr != get<OccursCheckFailed>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "free_is_not_bound_to_unknown")
{
    // This test only makes sense for the old solver
    if (FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local function foo(f: (unknown) -> (), x)
            f(x)
        end
    )");

    CHECK_EQ("<a>((unknown) -> (), a) -> ()", toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "dont_infer_parameter_types_for_functions_from_their_call_site")
{
    CheckResult result = check(R"(
        local t = {}

        function t.f(x)
            return x
        end

        t.__index = t

        function g(s)
            local q = s.p and s.p.q or nil
            return q and t.f(q) or nil
        end

        local f = t.f
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("<a>(a) -> a", toString(requireType("f")));
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("<a>({+ p: {+ q: a +} +}) -> a & ~false", toString(requireType("g")));
    else
        CHECK_EQ("({+ p: {+ q: nil +} +}) -> nil", toString(requireType("g")));
}

TEST_CASE_FIXTURE(Fixture, "dont_mutate_the_underlying_head_of_typepack_when_calling_with_self")
{
    CheckResult result = check(R"(
        local t = {}
        function t:m(x) end
        function f(): never return 5 :: never end
        t:m(f())
        t:m(f())
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "improved_function_arg_mismatch_errors")
{
    CheckResult result = check(R"(
local function foo1(a: number) end
foo1()

local function foo2(a: number, b: string?) end
foo2()

local function foo3(a: number, b: string?, c: any) end -- any is optional
foo3()

string.find()

local t = {}
function t.foo(x: number, y: string?, ...: any) return 1 end
function t:bar(x: number, y: string?) end
t.foo()

t:bar()

local u = { a = t, b = function() return t end }
u.a.foo()
local x = (u.a).foo()

u.b().foo()
    )");

    LUAU_REQUIRE_ERROR_COUNT(9, result);
    CHECK_EQ(toString(result.errors[0]), "Argument count mismatch. Function 'foo1' expects 1 argument, but none are specified");
    CHECK_EQ(toString(result.errors[1]), "Argument count mismatch. Function 'foo2' expects 1 to 2 arguments, but none are specified");
    CHECK_EQ(toString(result.errors[2]), "Argument count mismatch. Function 'foo3' expects 1 to 3 arguments, but none are specified");
    CHECK_EQ(toString(result.errors[3]), "Argument count mismatch. Function 'string.find' expects 2 to 4 arguments, but none are specified");
    CHECK_EQ(toString(result.errors[4]), "Argument count mismatch. Function 't.foo' expects at least 1 argument, but none are specified");
    CHECK_EQ(toString(result.errors[5]), "Argument count mismatch. Function 't.bar' expects 2 to 3 arguments, but only 1 is specified");
    CHECK_EQ(toString(result.errors[6]), "Argument count mismatch. Function 'u.a.foo' expects at least 1 argument, but none are specified");
    CHECK_EQ(toString(result.errors[7]), "Argument count mismatch. Function 'u.a.foo' expects at least 1 argument, but none are specified");
    CHECK_EQ(toString(result.errors[8]), "Argument count mismatch. Function expects at least 1 argument, but none are specified");
}

// This might be surprising, but since 'any' became optional, unannotated functions in non-strict 'expect' 0 arguments
TEST_CASE_FIXTURE(BuiltinsFixture, "improved_function_arg_mismatch_error_nonstrict")
{
    CheckResult result = check(R"(
--!nonstrict
local function foo(a, b) end
foo(string.find("hello", "e"))
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), "Argument count mismatch. Function 'foo' expects 0 to 2 arguments, but 3 are specified");
}

TEST_CASE_FIXTURE(Fixture, "luau_subtyping_is_np_hard")
{
    CheckResult result = check(R"(
--!strict

-- An example of coding up graph coloring in the Luau type system.
-- This codes a three-node, two color problem.
-- A three-node triangle is uncolorable,
-- but a three-node line is colorable.

type Red = "red"
type Blue = "blue"
type Color = Red | Blue
type Coloring = (Color) -> (Color) -> (Color) -> boolean
type Uncolorable = (Color) -> (Color) -> (Color) -> false

type Line = Coloring
  & ((Red) -> (Red) -> (Color) -> false)
  & ((Blue) -> (Blue) -> (Color) -> false)
  & ((Color) -> (Red) -> (Red) -> false)
  & ((Color) -> (Blue) -> (Blue) -> false)

type Triangle = Line
  & ((Red) -> (Color) -> (Red) -> false)
  & ((Blue) -> (Color) -> (Blue) -> false)

local x : Triangle
local y : Line
local z : Uncolorable
z = x -- OK, so the triangle is uncolorable
z = y -- Not OK, so the line is colorable
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '(("blue" | "red") -> ("blue" | "red") -> ("blue" | "red") -> boolean) & (("blue" | "red") -> ("blue") -> ("blue") -> false) & (("blue" | "red") -> ("red") -> ("red") -> false) & (("blue") -> ("blue") -> ("blue" | "red") -> false) & (("red") -> ("red") -> ("blue" | "red") -> false)'
could not be converted into
    '("blue" | "red") -> ("blue" | "red") -> ("blue" | "red") -> false'; none of the intersection parts are compatible)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "function_is_supertype_of_concrete_functions")
{
    registerHiddenTypes(&frontend);

    CheckResult result = check(R"(
        function foo(f: fun) end

        function a() end
        function id(x) return x end

        foo(a)
        foo(id)
        foo(foo)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "concrete_functions_are_not_supertypes_of_function")
{
    registerHiddenTypes(&frontend);

    CheckResult result = check(R"(
        local a: fun = function() end

        function one(arg: () -> ()) end
        function two(arg: <T>(T) -> T) end

        one(a)
        two(a)
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK(6 == result.errors[0].location.begin.line);
    CHECK(7 == result.errors[1].location.begin.line);
}

TEST_CASE_FIXTURE(Fixture, "other_things_are_not_related_to_function")
{
    registerHiddenTypes(&frontend);

    CheckResult result = check(R"(
        local a: fun = function() end
        local b: {} = a
        local c: boolean = a
        local d: fun = true
        local e: fun = {}
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);

    CHECK(2 == result.errors[0].location.begin.line);
    CHECK(3 == result.errors[1].location.begin.line);
    CHECK(4 == result.errors[2].location.begin.line);
    CHECK(5 == result.errors[3].location.begin.line);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzz_must_follow_in_overload_resolution")
{
    CheckResult result = check(R"(
for _ in function<t0>():(t0)&((()->())&(()->()))
end do
_(_(_,_,_),_)
end
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "dont_assert_when_the_tarjan_limit_is_exceeded_during_generalization")
{
    ScopedFastInt sfi{FInt::LuauTarjanChildLimit, 2};
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        function f(t)
            t.x.y.z = 441
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_MESSAGE(get<CodeTooComplex>(result.errors[0]), "Expected CodeTooComplex but got: " << toString(result.errors[0]));
    CHECK(Location({1, 17}, {1, 18}) == result.errors[0].location);
}

/* We had a bug under DCR where instantiated type packs had a nullptr scope.
 *
 * This caused an issue with promotion.
 */
TEST_CASE_FIXTURE(Fixture, "instantiated_type_packs_must_have_a_non_null_scope")
{
    CheckResult result = check(R"(
        function pcall<A..., R...>(...: A...): R...
        end

        type Dispatch<A> = (A) -> ()

        function mountReducer()
            dispatchAction()
            return nil :: any
        end

        function dispatchAction()
        end

        function useReducer(): Dispatch<any>
            local result, setResult = pcall(mountReducer)
            return setResult
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "inner_frees_become_generic_in_dcr")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        function f(x)
            local z = x
            return x
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    std::optional<TypeId> ty = findTypeAtPosition(Position{3, 19});
    REQUIRE(ty);
    CHECK(get<GenericType>(follow(*ty)));
}

TEST_CASE_FIXTURE(Fixture, "function_exprs_are_generalized_at_signature_scope_not_enclosing")
{
    CheckResult result = check(R"(
        local foo
        local bar

        -- foo being a function expression is deliberate: the bug we're testing
        -- only existed for function expressions, not for function statements.
        foo = function(a)
            return bar
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    // note that b is not in the generic list; it is free, the unconstrained type of `bar`.
    CHECK(toString(requireType("foo")) == "<a>(a) -> b");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "param_1_and_2_both_takes_the_same_generic_but_their_arguments_are_incompatible")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauAlwaysCommitInferencesOfFunctionCalls, true},
    };

    CheckResult result = check(R"(
        local function foo<a>(x: a, y: a?)
            return x
        end
        local vec2 = { x = 5, y = 7 }
        local ret: number = foo(vec2, { x = 5 })
    )");

    // In the old solver, this produces a very strange result:
    //
    //   Here, we instantiate `<a>(x: a, y: a?) -> a` with a fresh type `'a` for `a`.
    //   In argument #1, we unify `vec2` with `'a`.
    //     This is ok, so we record an equality constraint `'a` with `vec2`.
    //   In argument #2, we unify `{ x: number }` with `'a?`.
    //     This fails because `'a` has equality constraint with `vec2`,
    //     so `{ x: number } <: vec2?`, which is false.
    //
    // If the unifications were to be committed, then it'd result in the following type error:
    //
    //   Type '{ x: number }' could not be converted into 'vec2?'
    //   caused by:
    //     [...] Table type '{ x: number }' not compatible with type 'vec2' because the former is missing field 'y'
    //
    // However, whenever we check the argument list, if there's an error, we don't commit the unifications, so it actually looks like this:
    //
    //   Type '{ x: number }' could not be converted into 'a?'
    //   caused by:
    //     [...] Table type '{ x: number }' not compatible with type 'vec2' because the former is missing field 'y'
    //
    // Then finally, that generic is left floating free, and since the function returns that generic,
    // that free type is then later bound to `number`, which succeeds and mutates the type graph.
    // This again changes the type error where `a` becomes bound to `number`.
    //
    //   Type '{ x: number }' could not be converted into 'number?'
    //   caused by:
    //     [...] Table type '{ x: number }' not compatible with type 'vec2' because the former is missing field 'y'
    //
    // Uh oh, that type error is extremely confusing for people who doesn't know how that went down.
    // Really, what should happen is we roll each argument incompatibility into a union type, but that needs local type inference.

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    const std::string expected = R"(Type '{ x: number }' could not be converted into 'vec2?'
caused by:
  None of the union options are compatible. For example:
Table type '{ x: number }' not compatible with type 'vec2' because the former is missing field 'y')";
    CHECK_EQ(expected, toString(result.errors[0]));
    CHECK_EQ("Type 'vec2' could not be converted into 'number'", toString(result.errors[1]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "param_1_and_2_both_takes_the_same_generic_but_their_arguments_are_incompatible_2")
{
    ScopedFastFlag sff{FFlag::LuauAlwaysCommitInferencesOfFunctionCalls, true};

    CheckResult result = check(R"(
        local function f<a>(x: a, y: a): a
            return if math.random() > 0.5 then x else y
        end

        local z: boolean = f(5, "five")
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK_EQ(toString(result.errors[0]), "Type 'string' could not be converted into 'number'");
    CHECK_EQ(toString(result.errors[1]), "Type 'number' could not be converted into 'boolean'");
}

TEST_CASE_FIXTURE(Fixture, "attempt_to_call_an_intersection_of_tables")
{
    CheckResult result = check(R"(
        local function f(t: { x: number } & { y: string })
            t()
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ(toString(result.errors[0]), "Cannot call non-function { x: number } & { y: string }");
    else
        CHECK_EQ(toString(result.errors[0]), "Cannot call non-function {| x: number |}");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "attempt_to_call_an_intersection_of_tables_with_call_metamethod")
{
    CheckResult result = check(R"(
        type Callable = typeof(setmetatable({}, {
            __call = function(self, ...) return ... end
        }))

        local function f(t: Callable & { x: number })
            t()
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "generic_packs_are_not_variadic")
{
    // This test is blocking CI until subtyping is complete.
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        local function apply<a, b..., c...>(f: (a, b...) -> c..., x: a)
            return f(x)
        end

        local function add(x: number, y: number)
            return x + y
        end

        apply(add, 5)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "num_is_solved_before_num_or_str")
{
    CheckResult result = check(R"(
        function num()
            return 5
        end

        local function num_or_str()
            if math.random() > 0.5 then
                return num()
            else
                return "some string"
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK(toString(result.errors.at(0)) == "Type pack 'string' could not be converted into 'number'; at [0], string is not a subtype of number");
    else
        CHECK_EQ("Type 'string' could not be converted into 'number'", toString(result.errors[0]));

    CHECK_EQ("() -> number", toString(requireType("num_or_str")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "num_is_solved_after_num_or_str")
{
    CheckResult result = check(R"(
        local function num_or_str()
            if math.random() > 0.5 then
                return num()
            else
                return "some string"
            end
        end

        function num()
            return 5
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK(toString(result.errors.at(0)) == "Type pack 'string' could not be converted into 'number'; at [0], string is not a subtype of number");
    else
        CHECK_EQ("Type 'string' could not be converted into 'number'", toString(result.errors[0]));
    CHECK_EQ("() -> number", toString(requireType("num_or_str")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "apply_of_lambda_with_inferred_and_explicit_types")
{
    CheckResult result = check(R"(
        local function apply(f, x) return f(x) end
        local x = apply(function(x: string): number return 5 end, "hello!")

        local function apply_explicit<A, B...>(f: (A) -> B..., x: A): B... return f(x) end
        local x = apply_explicit(function(x: string): number return 5 end, "hello!")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "regex_benchmark_string_format_minimization")
{
    CheckResult result = check(R"(
        (nil :: any)(function(n)
            if tonumber(n) then
                n = tonumber(n)
            elseif n ~= nil then
                string.format("invalid argument #4 to 'sub': number expected, got %s", typeof(n))
            end
        end);
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
