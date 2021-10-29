// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Parser.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"
#include "Luau/VisitTypeVar.h"

#include "Fixture.h"

#include "doctest.h"

#include <algorithm>

LUAU_FASTFLAG(LuauFixLocationSpanTableIndexExpr)
LUAU_FASTFLAG(LuauEqConstraint)

using namespace Luau;

TEST_SUITE_BEGIN("TypeInfer");

TEST_CASE_FIXTURE(Fixture, "tc_hello_world")
{
    CheckResult result = check("local a = 7");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId aType = requireType("a");
    CHECK_EQ(getPrimitiveType(aType), PrimitiveTypeVar::Number);
}

TEST_CASE_FIXTURE(Fixture, "tc_propagation")
{
    CheckResult result = check("local a = 7   local b = a");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId bType = requireType("b");
    CHECK_EQ(getPrimitiveType(bType), PrimitiveTypeVar::Number);
}

TEST_CASE_FIXTURE(Fixture, "tc_error")
{
    CheckResult result = check("local a = 7   local b = 'hi'   a = b");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(result.errors[0], (TypeError{Location{Position{0, 35}, Position{0, 36}}, TypeMismatch{
                                                                                          requireType("a"),
                                                                                          requireType("b"),
                                                                                      }}));
}

TEST_CASE_FIXTURE(Fixture, "tc_error_2")
{
    CheckResult result = check("local a = 7   a = 'hi'");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(result.errors[0], (TypeError{Location{Position{0, 18}, Position{0, 22}}, TypeMismatch{
                                                                                          requireType("a"),
                                                                                          typeChecker.stringType,
                                                                                      }}));
}

TEST_CASE_FIXTURE(Fixture, "tc_function")
{
    CheckResult result = check("function five() return 5 end");
    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionTypeVar* fiveType = get<FunctionTypeVar>(requireType("five"));
    REQUIRE(fiveType != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "infer_locals_with_nil_value")
{
    CheckResult result = check("local f = nil; f = 'hello world'");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId ty = requireType("f");
    CHECK_EQ(getPrimitiveType(ty), PrimitiveTypeVar::String);
}

TEST_CASE_FIXTURE(Fixture, "infer_locals_via_assignment_from_its_call_site")
{
    CheckResult result = check(R"(
        local a
        function f(x) a = x end
        f(1)
        f("foo")
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("number", toString(requireType("a")));
}

TEST_CASE_FIXTURE(Fixture, "infer_in_nocheck_mode")
{
    CheckResult result = check(R"(
        --!nocheck
        function f(x)
            return x
        end
         -- we get type information even if there's type errors
        f(1, 2)
    )");

    CHECK_EQ("(any) -> (...any)", toString(requireType("f")));

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "check_function_bodies")
{
    CheckResult result = check("function myFunction()    local a = 0    a = true    end");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(result.errors[0], (TypeError{Location{Position{0, 44}, Position{0, 48}}, TypeMismatch{
                                                                                          typeChecker.numberType,
                                                                                          typeChecker.booleanType,
                                                                                      }}));
}

TEST_CASE_FIXTURE(Fixture, "infer_return_type")
{
    CheckResult result = check("function take_five() return 5 end");
    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionTypeVar* takeFiveType = get<FunctionTypeVar>(requireType("take_five"));
    REQUIRE(takeFiveType != nullptr);

    std::vector<TypeId> retVec = flatten(takeFiveType->retType).first;
    REQUIRE(!retVec.empty());

    REQUIRE_EQ(*follow(retVec[0]), *typeChecker.numberType);
}

TEST_CASE_FIXTURE(Fixture, "infer_from_function_return_type")
{
    CheckResult result = check("function take_five() return 5 end    local five = take_five()");
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.numberType, *follow(requireType("five")));
}

TEST_CASE_FIXTURE(Fixture, "cannot_call_primitives")
{
    CheckResult result = check("local foo = 5    foo()");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    REQUIRE(get<CannotCallNonFunction>(result.errors[0]) != nullptr);
}

TEST_CASE_FIXTURE(Fixture, "cannot_call_tables")
{
    CheckResult result = check("local foo = {}    foo()");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK(get<CannotCallNonFunction>(result.errors[0]) != nullptr);
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
    CHECK_EQ(result.errors[0], (TypeError{Location{Position{5, 8}, Position{5, 24}}, NotATable{typeChecker.numberType}}));
}

TEST_CASE_FIXTURE(Fixture, "expr_statement")
{
    CheckResult result = check("local foo = 5    foo()");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "generic_function")
{
    CheckResult result = check("function id(x) return x end    local a = id(55)    local b = id(nil)");
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.numberType, *requireType("a"));
    CHECK_EQ(*typeChecker.nilType, *requireType("b"));
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

TEST_CASE_FIXTURE(Fixture, "vararg_function_is_quantified")
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

    auto r = first(getMainModule()->getModuleScope()->returnType);
    REQUIRE(r);

    TableTypeVar* ttv = getMutable<TableTypeVar>(*r);
    REQUIRE(ttv);

    TypeId k = ttv->props["f"].type;
    REQUIRE(k);

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "for_loop")
{
    CheckResult result = check(R"(
        local q
        for i=0, 50, 2 do
            q = i
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.numberType, *requireType("q"));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop")
{
    CheckResult result = check(R"(
        local n
        local s
        for i, v in pairs({ "foo" }) do
            n = i
            s = v
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.numberType, *requireType("n"));
    CHECK_EQ(*typeChecker.stringType, *requireType("s"));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_with_next")
{
    CheckResult result = check(R"(
        local n
        local s
        for i, v in next, { "foo", "bar" } do
            n = i
            s = v
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.numberType, *requireType("n"));
    CHECK_EQ(*typeChecker.stringType, *requireType("s"));
}

TEST_CASE_FIXTURE(Fixture, "for_in_with_an_iterator_of_type_any")
{
    CheckResult result = check(R"(
        local it: any
        local a, b
        for i, v in it do
            a, b = i, v
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_should_fail_with_non_function_iterator")
{
    CheckResult result = check(R"(
        local foo = "bar"
        for i, v in foo do
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "for_in_with_just_one_iterator_is_ok")
{
    CheckResult result = check(R"(
        local function keys(dictionary)
            local new = {}
            local index = 1

            for key in pairs(dictionary) do
                new[index] = key
                index = index + 1
            end

            return new
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "for_in_with_a_custom_iterator_should_type_check")
{
    CheckResult result = check(R"(
        local function range(l, h): () -> number
            return function()
                return l
            end
        end

        for n: string in range(1, 10) do
            print(n)
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_on_error")
{
    CheckResult result = check(R"(
        function f(x)
            gobble.prop = x.otherprop
        end

        local p
        for _, part in i_am_not_defined do
            p = part
            f(part)
            part.thirdprop = false
        end
    )");

    CHECK_EQ(2, result.errors.size());

    TypeId p = requireType("p");
    CHECK_EQ(*p, *typeChecker.errorType);
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_on_non_function")
{
    CheckResult result = check(R"(
        local bad_iter = 5

        for a in bad_iter() do
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    REQUIRE(get<CannotCallNonFunction>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_error_on_factory_not_returning_the_right_amount_of_values")
{
    CheckResult result = check(R"(
        local function hasDivisors(value: number, table)
            return false
        end

        function prime_iter(state, index)
            while hasDivisors(index, state) do
                index += 1
            end

            state[index] = true
            return index
        end

        function primes1()
            return prime_iter, {}
        end

        function primes2()
            return prime_iter, {}, ""
        end

        function primes3()
            return prime_iter, {}, 2
        end

        for p in primes1() do print(p) end -- mismatch in argument count

        for p in primes2() do print(p) end -- mismatch in argument types, prime_iter takes {}, number, we are given {}, string

        for p in primes3() do print(p) end -- no errror
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CountMismatch* acm = get<CountMismatch>(result.errors[0]);
    REQUIRE(acm);
    CHECK_EQ(acm->context, CountMismatch::Arg);
    CHECK_EQ(2, acm->expected);
    CHECK_EQ(1, acm->actual);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[1]);
    REQUIRE(tm);
    CHECK_EQ(typeChecker.numberType, tm->wantedType);
    CHECK_EQ(typeChecker.stringType, tm->givenType);
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_error_on_iterator_requiring_args_but_none_given")
{
    CheckResult result = check(R"(
        function prime_iter(state, index)
            return 1
        end

        for p in prime_iter do print(p) end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CountMismatch* acm = get<CountMismatch>(result.errors[0]);
    REQUIRE(acm);
    CHECK_EQ(acm->context, CountMismatch::Arg);
    CHECK_EQ(2, acm->expected);
    CHECK_EQ(0, acm->actual);
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_iterator_returns_any")
{
    CheckResult result = check(R"(
        function bar(): any
            return true
        end

        local a
        for b in bar do
            a = b
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(typeChecker.anyType, requireType("a"));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_iterator_returns_any2")
{
    CheckResult result = check(R"(
        function bar(): any
            return true
        end

        local a
        for b in bar() do
            a = b
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(typeChecker.anyType, requireType("a"));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_iterator_is_any")
{
    CheckResult result = check(R"(
        local bar: any

        local a
        for b in bar do
            a = b
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(typeChecker.anyType, requireType("a"));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_iterator_is_any2")
{
    CheckResult result = check(R"(
        local bar: any

        local a
        for b in bar() do
            a = b
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(typeChecker.anyType, requireType("a"));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_iterator_is_error")
{
    CheckResult result = check(R"(
        local a
        for b in bar do
            a = b
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(typeChecker.errorType, requireType("a"));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_iterator_is_error2")
{
    CheckResult result = check(R"(
        function bar(c) return c end

        local a
        for b in bar() do
            a = b
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(typeChecker.errorType, requireType("a"));
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_with_custom_iterator")
{
    CheckResult result = check(R"(
        function primes()
            return function (state: number) end,  2
        end

        for p, q in primes do
            q = ""
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ(typeChecker.numberType, tm->wantedType);
    CHECK_EQ(typeChecker.stringType, tm->givenType);
}

TEST_CASE_FIXTURE(Fixture, "if_statement")
{
    CheckResult result = check(R"(
        local a
        local b

        if true then
            a = 'hello'
        else
            b = 999
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.stringType, *requireType("a"));
    CHECK_EQ(*typeChecker.numberType, *requireType("b"));
}

TEST_CASE_FIXTURE(Fixture, "while_loop")
{
    CheckResult result = check(R"(
        local i
        while true do
            i = 8
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.numberType, *requireType("i"));
}

TEST_CASE_FIXTURE(Fixture, "repeat_loop")
{
    CheckResult result = check(R"(
        local i
        repeat
            i = 'hi'
        until true
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(*typeChecker.stringType, *requireType("i"));
}

TEST_CASE_FIXTURE(Fixture, "repeat_loop_condition_binds_to_its_block")
{
    CheckResult result = check(R"(
        repeat
            local x = true
        until x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "symbols_in_repeat_block_should_not_be_visible_beyond_until_condition")
{
    CheckResult result = check(R"(
        repeat
            local x = true
        until x

        print(x)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "table_length")
{
    CheckResult result = check(R"(
        local t = {}
        local s = #t
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(nullptr != get<TableTypeVar>(requireType("t")));
    CHECK_EQ(*typeChecker.numberType, *requireType("s"));
}

TEST_CASE_FIXTURE(Fixture, "string_length")
{
    CheckResult result = check(R"(
        local s = "Hello, World!"
        local t = #s
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("number", toString(requireType("t")));
}

TEST_CASE_FIXTURE(Fixture, "string_index")
{
    CheckResult result = check(R"(
        local s = "Hello, World!"
        local t = s[4]
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    NotATable* nat = get<NotATable>(result.errors[0]);
    REQUIRE(nat);
    CHECK_EQ("string", toString(nat->ty));

    CHECK(get<ErrorTypeVar>(requireType("t")));
}

TEST_CASE_FIXTURE(Fixture, "length_of_error_type_does_not_produce_an_error")
{
    CheckResult result = check(R"(
        local l = #this_is_not_defined
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "indexing_error_type_does_not_produce_an_error")
{
    CheckResult result = check(R"(
        local originalReward = unknown.Parent.Reward:GetChildren()[1]
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "nil_assign_doesnt_hit_indexer")
{
    CheckResult result = check("local a = {} a[0] = 7  a[0] = nil");
    LUAU_REQUIRE_ERROR_COUNT(0, result);
}

TEST_CASE_FIXTURE(Fixture, "wrong_assign_does_hit_indexer")
{
    CheckResult result = check("local a = {} a[0] = 7  a[0] = 't'");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(result.errors[0], (TypeError{Location{Position{0, 30}, Position{0, 33}}, TypeMismatch{
                                                                                          typeChecker.numberType,
                                                                                          typeChecker.stringType,
                                                                                      }}));
}

TEST_CASE_FIXTURE(Fixture, "nil_assign_doesnt_hit_no_indexer")
{
    CheckResult result = check("local a = {a=1, b=2} a['a'] = nil");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(result.errors[0], (TypeError{Location{Position{0, 30}, Position{0, 33}}, TypeMismatch{
                                                                                          typeChecker.numberType,
                                                                                          typeChecker.nilType,
                                                                                      }}));
}

TEST_CASE_FIXTURE(Fixture, "dot_on_error_type_does_not_produce_an_error")
{
    CheckResult result = check(R"(
        local foo = (true).x
        foo.x = foo.y
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "dont_suggest_using_colon_rather_than_dot_if_not_defined_with_colon")
{
    CheckResult result = check(R"(
        local someTable = {}

        someTable.Function1 = function(Arg1)
        end

        someTable.Function1() -- Argument count mismatch
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    REQUIRE(get<CountMismatch>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "dont_suggest_using_colon_rather_than_dot_if_it_wont_help_2")
{
    CheckResult result = check(R"(
        local someTable = {}

        someTable.Function2 = function(Arg1, Arg2)
        end

        someTable.Function2() -- Argument count mismatch
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    REQUIRE(get<CountMismatch>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "dont_suggest_using_colon_rather_than_dot_if_another_overload_works")
{
    CheckResult result = check(R"(
        type T = {method: ((T, number) -> number) & ((number) -> number)}
        local T: T

        T.method(4)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "list_only_alternative_overloads_that_match_argument_count")
{
    CheckResult result = check(R"(
        local multiply: ((number)->number) & ((number)->string) & ((number, number)->number)
        multiply("")
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ(typeChecker.numberType, tm->wantedType);
    CHECK_EQ(typeChecker.stringType, tm->givenType);

    ExtraInformation* ei = get<ExtraInformation>(result.errors[1]);
    REQUIRE(ei);
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
    CHECK_EQ(typeChecker.numberType, tm->wantedType);
    CHECK_EQ(typeChecker.stringType, tm->givenType);
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

TEST_CASE_FIXTURE(Fixture, "any_type_propagates")
{
    CheckResult result = check(R"(
        local foo: any
        local bar = foo:method("argument")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("any", toString(requireType("bar")));
}

TEST_CASE_FIXTURE(Fixture, "can_subscript_any")
{
    CheckResult result = check(R"(
        local foo: any
        local bar = foo[5]
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("any", toString(requireType("bar")));
}

// Not strictly correct: metatables permit overriding this
TEST_CASE_FIXTURE(Fixture, "can_get_length_of_any")
{
    CheckResult result = check(R"(
        local foo: any = {}
        local bar = #foo
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(PrimitiveTypeVar::Number, getPrimitiveType(requireType("bar")));
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

TEST_CASE_FIXTURE(Fixture, "cyclic_function_type_in_rets")
{
    ScopedFastFlag sff{"LuauOccursCheckOkWithRecursiveFunctions", true};

    CheckResult result = check(R"(
        function f()
            return f
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("t1 where t1 = () -> t1", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "cyclic_function_type_in_args")
{
    ScopedFastFlag sff{"LuauOccursCheckOkWithRecursiveFunctions", true};

    CheckResult result = check(R"(
        function f(g)
            return f(f)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("t1 where t1 = (t1) -> ()", toString(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "cyclic_function_type_in_type_alias")
{
    ScopedFastFlag sff{"LuauOccursCheckOkWithRecursiveFunctions", true};

    CheckResult result = check(R"(
        type F = () -> F?
        local function f()
            return f
        end

        local g: F = f
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("t1 where t1 = () -> t1?", toString(requireType("g")));
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

    REQUIRE(bool(getMainModule()->getModuleScope()->varargPack));

    TypePackId varargPack = *getMainModule()->getModuleScope()->varargPack;

    auto iter = begin(varargPack);
    auto endIter = end(varargPack);

    CHECK(iter != endIter);
    ++iter;
    CHECK(iter == endIter);

    CHECK(!iter.tail());
}
*/

TEST_CASE_FIXTURE(Fixture, "method_depends_on_table")
{
    CheckResult result = check(R"(
        -- This catches a bug where x:m didn't count as a use of x
        -- so toposort would happily reorder a definition of
        -- function x:m before the definition of x.
        function g() f() end
        local x = {}
        function x:m() end
        function f() x:m() end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
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

TEST_CASE_FIXTURE(Fixture, "statements_are_topologically_sorted")
{
    CheckResult result = check(R"(
        function foo()
            return bar(999), bar("hi")
        end

        function bar(i)
            return i
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);
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
    TableTypeVar* tTable = getMutable<TableTypeVar>(tType);
    REQUIRE(tTable != nullptr);

    TypeId barType = tTable->props["bar"].type;
    REQUIRE(barType != nullptr);

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(follow(barType));
    REQUIRE_MESSAGE(ftv != nullptr, "Should be a function: " << *barType);

    std::vector<TypeId> args = flatten(ftv->argTypes).first;
    TypeId argType = args.at(1);

    CHECK_MESSAGE(get<Unifiable::Generic>(argType), "Should be generic: " << *barType);
}

TEST_CASE_FIXTURE(Fixture, "correctly_instantiate_polymorphic_member_functions")
{
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

    const TableTypeVar* t = get<TableTypeVar>(requireType("T"));
    REQUIRE(t != nullptr);

    std::optional<Property> fooProp = get(t->props, "foo");
    REQUIRE(bool(fooProp));

    const FunctionTypeVar* foo = get<FunctionTypeVar>(follow(fooProp->type));
    REQUIRE(bool(foo));

    std::optional<TypeId> ret_ = first(foo->retType);
    REQUIRE(bool(ret_));
    TypeId ret = follow(*ret_);

    REQUIRE_EQ(getPrimitiveType(ret), PrimitiveTypeVar::Number);
}

TEST_CASE_FIXTURE(Fixture, "methods_are_topologically_sorted")
{
    CheckResult result = check(R"(
        local T = {}

        function T:foo()
            return T:bar(999), T:bar("hi")
        end

        function T:bar(i)
            return i
        end

        local a, b = T:foo()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);

    CHECK_EQ(PrimitiveTypeVar::Number, getPrimitiveType(requireType("a")));
    CHECK_EQ(PrimitiveTypeVar::String, getPrimitiveType(requireType("b")));
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

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(h);
    REQUIRE(ftv != nullptr);

    std::optional<TypeId> rt = first(ftv->retType);
    REQUIRE(bool(rt));

    TypeId retType = follow(*rt);
    CHECK_EQ(PrimitiveTypeVar::String, getPrimitiveType(retType));
}

TEST_CASE_FIXTURE(Fixture, "unify_nearly_identical_recursive_types")
{
    CheckResult result = check(R"(
        local o
        o:method()

        local p
        p:method()

        o = p
    )");
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
    const FunctionTypeVar* gFun = get<FunctionTypeVar>(g);
    REQUIRE(gFun != nullptr);

    auto optionArg = first(gFun->argTypes);
    REQUIRE(bool(optionArg));

    TypeId arg = follow(*optionArg);
    const TableTypeVar* argTable = get<TableTypeVar>(arg);
    REQUIRE(argTable != nullptr);

    std::optional<Property> methodProp = get(argTable->props, "method");
    REQUIRE(bool(methodProp));

    const FunctionTypeVar* methodFunction = get<FunctionTypeVar>(methodProp->type);
    REQUIRE(methodFunction != nullptr);

    std::optional<TypeId> methodArg = first(methodFunction->argTypes);
    REQUIRE(bool(methodArg));

    REQUIRE_EQ(follow(*methodArg), follow(arg));
}

TEST_CASE_FIXTURE(Fixture, "cyclic_types_of_named_table_fields_do_not_expand_when_stringified")
{
    CheckResult result = check(R"(
        --!strict
        type Node = { Parent: Node?; }
        local node: Node;
        node.Parent = 1
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("Node?", toString(tm->wantedType));
    CHECK_EQ(typeChecker.numberType, tm->givenType);
}

TEST_CASE_FIXTURE(Fixture, "varlist_declared_by_for_in_loop_should_be_free")
{
    CheckResult result = check(R"(
        local T = {}

        function T.f(p)
            for i, v in pairs(p) do
                T.f(v)
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "properly_infer_iteratee_is_a_free_table")
{
    // In this case, we cannot know the element type of the table {}.  It could be anything.
    // We therefore must initially ascribe a free typevar to iter.
    CheckResult result = check(R"(
        for iter in pairs({}) do
            iter:g().p = true
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "quantify_methods_defined_using_dot_syntax_and_explicit_self_parameter")
{
    check(R"(
        local T = {}

        function T.method(self)
            self:method()
        end

        function T.method2(self)
            self:method()
        end

        T:method2()
    )");
}

TEST_CASE_FIXTURE(Fixture, "free_rhs_table_can_also_be_bound")
{
    check(R"(
        local o
        local v = o:i()

        function g(u)
            v = u
        end

        o:f(g)
        o:h()
        o:h()
    )");
}

TEST_CASE_FIXTURE(Fixture, "require")
{
    fileResolver.source["game/A"] = R"(
        local function hooty(x: number): string
            return "Hi there!"
        end

        return {hooty=hooty}
    )";

    fileResolver.source["game/B"] = R"(
        local Hooty = require(game.A)

        local h -- free!
        local i = Hooty.hooty(h)
    )";

    CheckResult aResult = frontend.check("game/A");
    dumpErrors(aResult);
    LUAU_REQUIRE_NO_ERRORS(aResult);

    CheckResult bResult = frontend.check("game/B");
    dumpErrors(bResult);
    LUAU_REQUIRE_NO_ERRORS(bResult);

    ModulePtr b = frontend.moduleResolver.modules["game/B"];

    REQUIRE(b != nullptr);

    dumpErrors(bResult);

    std::optional<TypeId> iType = requireType(b, "i");
    REQUIRE_EQ("string", toString(*iType));

    std::optional<TypeId> hType = requireType(b, "h");
    REQUIRE_EQ("number", toString(*hType));
}

TEST_CASE_FIXTURE(Fixture, "require_types")
{
    fileResolver.source["workspace/A"] = R"(
        export type Point = {x: number, y: number}

        return {}
    )";

    fileResolver.source["workspace/B"] = R"(
        local Hooty = require(workspace.A)

        local h: Hooty.Point
    )";

    CheckResult bResult = frontend.check("workspace/B");
    dumpErrors(bResult);

    ModulePtr b = frontend.moduleResolver.modules["workspace/B"];
    REQUIRE(b != nullptr);

    TypeId hType = requireType(b, "h");
    REQUIRE_MESSAGE(bool(get<TableTypeVar>(hType)), "Expected table but got " << toString(hType));
}

TEST_CASE_FIXTURE(Fixture, "require_a_variadic_function")
{
    fileResolver.source["game/A"] = R"(
        local T = {}
        function T.f(...) end
        return T
    )";

    fileResolver.source["game/B"] = R"(
        local A = require(game.A)
        local f = A.f
    )";

    CheckResult result = frontend.check("game/B");

    ModulePtr bModule = frontend.moduleResolver.getModule("game/B");
    REQUIRE(bModule != nullptr);

    TypeId f = follow(requireType(bModule, "f"));

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(f);
    REQUIRE(ftv);

    auto iter = begin(ftv->argTypes);
    auto endIter = end(ftv->argTypes);

    REQUIRE(iter == endIter);
    REQUIRE(iter.tail());

    CHECK(get<VariadicTypePack>(*iter.tail()));
}

TEST_CASE_FIXTURE(Fixture, "assign_prop_to_table_by_calling_any_yields_any")
{
    CheckResult result = check(R"(
        local f: any
        local T = {}

        T.prop = f()

        return T
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TableTypeVar* ttv = getMutable<TableTypeVar>(requireType("T"));
    REQUIRE(ttv);
    REQUIRE(ttv->props.count("prop"));

    REQUIRE_EQ("any", toString(ttv->props["prop"].type));
}

TEST_CASE_FIXTURE(Fixture, "type_error_of_unknown_qualified_type")
{
    CheckResult result = check(R"(
        local p: SomeModule.DoesNotExist
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    REQUIRE_EQ(result.errors[0], (TypeError{Location{{1, 17}, {1, 40}}, UnknownSymbol{"SomeModule.DoesNotExist"}}));
}

TEST_CASE_FIXTURE(Fixture, "require_module_that_does_not_export")
{
    const std::string sourceA = R"(
    )";

    const std::string sourceB = R"(
        local Hooty = require(script.Parent.A)
    )";

    fileResolver.source["game/Workspace/A"] = sourceA;
    fileResolver.source["game/Workspace/B"] = sourceB;

    frontend.check("game/Workspace/A");
    frontend.check("game/Workspace/B");

    ModulePtr aModule = frontend.moduleResolver.modules["game/Workspace/A"];
    ModulePtr bModule = frontend.moduleResolver.modules["game/Workspace/B"];

    CHECK(aModule->errors.empty());
    REQUIRE_EQ(1, bModule->errors.size());
    CHECK_MESSAGE(get<IllegalRequire>(bModule->errors[0]), "Should be IllegalRequire: " << toString(bModule->errors[0]));

    auto hootyType = requireType(bModule, "Hooty");

    CHECK_MESSAGE(get<ErrorTypeVar>(follow(hootyType)) != nullptr, "Should be an error: " << toString(hootyType));
}

TEST_CASE_FIXTURE(Fixture, "warn_on_lowercase_parent_property")
{
    CheckResult result = check(R"(
        local M = require(script.parent.DoesNotMatter)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto ed = get<DeprecatedApiUsed>(result.errors[0]);
    REQUIRE(ed);

    REQUIRE_EQ("parent", ed->symbol);
}

TEST_CASE_FIXTURE(Fixture, "quantify_any_does_not_bind_to_itself")
{
    CheckResult result = check(R"(
        local A : any
        function A.B() end
        A:C()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId aType = requireType("A");
    CHECK_EQ(aType, typeChecker.anyType);
}

TEST_CASE_FIXTURE(Fixture, "table_unifies_into_map")
{
    CheckResult result = check(R"(
        local Instance: any
        local UDim2: any

        function Create(instanceType)
            return function(data)
                local obj = Instance.new(instanceType)
                for k, v in pairs(data) do
                    if type(k) == 'number' then
                        --v.Parent = obj
                    else
                        obj[k] = v
                    end
                end
                return obj
            end
        end

        local topbarShadow = Create'ImageLabel'{
            Name = "TopBarShadow";
            Size = UDim2.new(1, 0, 0, 3);
            Position = UDim2.new(0, 0, 1, 0);
            Image = "rbxasset://textures/ui/TopBar/dropshadow.png";
            BackgroundTransparency = 1;
            Active = false;
            Visible = false;
        };

    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "func_expr_doesnt_leak_free")
{
    CheckResult result = check(R"(
        local p = function(x) return x end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    const Luau::FunctionTypeVar* fn = get<FunctionTypeVar>(requireType("p"));
    REQUIRE(fn);
    auto ret = first(fn->retType);
    REQUIRE(ret);
    REQUIRE(get<GenericTypeVar>(follow(*ret)));
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
    CHECK_EQ("((string) -> number, string) -> number", toString(*tm->givenType));
}

TEST_CASE_FIXTURE(Fixture, "string_method")
{
    CheckResult result = check(R"(
        local p = ("tacos"):len()
    )");
    CHECK_EQ(0, result.errors.size());

    CHECK_EQ(*requireType("p"), *typeChecker.numberType);
}

TEST_CASE_FIXTURE(Fixture, "string_function_indirect")
{
    CheckResult result = check(R"(
        local s:string
        local l = s.lower
        local p = l(s)
    )");
    CHECK_EQ(0, result.errors.size());

    CHECK_EQ(*requireType("p"), *typeChecker.stringType);
}

TEST_CASE_FIXTURE(Fixture, "string_function_other")
{
    CheckResult result = check(R"(
        local s:string
        local p = s:match("foo")
    )");
    CHECK_EQ(0, result.errors.size());

    CHECK_EQ(toString(requireType("p")), "string?");
}

TEST_CASE_FIXTURE(Fixture, "weird_case")
{
    CheckResult result = check(R"(
        local function f() return 4 end
        local d = math.deg(f())
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);
}

TEST_CASE_FIXTURE(Fixture, "or_joins_types")
{
    CheckResult result = check(R"(
        local s = "a" or 10
        local x:string|number = s
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(toString(*requireType("s")), "number | string");
    CHECK_EQ(toString(*requireType("x")), "number | string");
}

TEST_CASE_FIXTURE(Fixture, "or_joins_types_with_no_extras")
{
    CheckResult result = check(R"(
        local s = "a" or 10
        local x:number|string = s
        local y = x or "s"
    )");
    CHECK_EQ(0, result.errors.size());
    CHECK_EQ(toString(*requireType("s")), "number | string");
    CHECK_EQ(toString(*requireType("y")), "number | string");
}

TEST_CASE_FIXTURE(Fixture, "or_joins_types_with_no_superfluous_union")
{
    CheckResult result = check(R"(
        local s = "a" or "b"
        local x:string = s
    )");
    CHECK_EQ(0, result.errors.size());
    CHECK_EQ(*requireType("s"), *typeChecker.stringType);
}

TEST_CASE_FIXTURE(Fixture, "and_adds_boolean")
{
    CheckResult result = check(R"(
        local s = "a" and 10
        local x:boolean|number = s
    )");
    CHECK_EQ(0, result.errors.size());
    CHECK_EQ(toString(*requireType("s")), "boolean | number");
}

TEST_CASE_FIXTURE(Fixture, "and_adds_boolean_no_superfluous_union")
{
    CheckResult result = check(R"(
        local s = "a" and true
        local x:boolean = s
    )");
    CHECK_EQ(0, result.errors.size());
    CHECK_EQ(*requireType("x"), *typeChecker.booleanType);
}

TEST_CASE_FIXTURE(Fixture, "and_or_ternary")
{
    CheckResult result = check(R"(
        local s = (1/2) > 0.5 and "a" or 10
    )");
    CHECK_EQ(0, result.errors.size());
    CHECK_EQ(toString(*requireType("s")), "number | string");
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

TEST_CASE_FIXTURE(Fixture, "dont_ice_when_failing_the_occurs_check")
{
    ScopedFastFlag sff{"LuauOccursCheckOkWithRecursiveFunctions", true};

    CheckResult result = check(R"(
        --!strict
        local s
        s(s, 'a')
    )");
    LUAU_REQUIRE_ERROR_COUNT(0, result);
}

TEST_CASE_FIXTURE(Fixture, "occurs_check_does_not_recurse_forever_if_asked_to_traverse_a_cyclic_type")
{
    ScopedFastFlag sff{"LuauOccursCheckOkWithRecursiveFunctions", true};

    CheckResult result = check(R"(
         --!strict
        function u(t, w)
            u(u, t)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

#if 0
// CLI-29798
TEST_CASE_FIXTURE(Fixture, "crazy_complexity")
{
    CheckResult result = check(R"(
        --!nonstrict
        A:A():A():A():A():A():A():A():A():A():A():A()
    )");

    std::cout << "OK!  Allocated " << typeChecker.typeVars.size() << " typevars" << std::endl;
}
#endif

// We had a bug where a cyclic union caused a stack overflow.
// ex type U = number | U
TEST_CASE_FIXTURE(Fixture, "dont_allow_cyclic_unions_to_be_inferred")
{
    CheckResult result = check(R"(
        --!strict

        function f(a, b)
            a:g(b or {})
            a:g(b)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
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

TEST_CASE_FIXTURE(Fixture, "tables_get_names_from_their_locals")
{
    CheckResult result = check(R"(
        local T = {}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("T", toString(requireType("T")));
}

TEST_CASE_FIXTURE(Fixture, "generalize_table_argument")
{
    CheckResult result = check(R"(
        function foo(arr)
            local work = {}
            for i = 1, #arr do
                work[i] = arr[i]
            end

            return arr
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);

    const FunctionTypeVar* fooType = get<FunctionTypeVar>(requireType("foo"));
    REQUIRE(fooType);

    std::optional<TypeId> fooArg1 = first(fooType->argTypes);
    REQUIRE(fooArg1);

    const TableTypeVar* fooArg1Table = get<TableTypeVar>(*fooArg1);
    REQUIRE(fooArg1Table);

    CHECK_EQ(fooArg1Table->state, TableState::Generic);
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

    const FunctionTypeVar* functionType = get<FunctionTypeVar>(requireType("most_of_the_natural_numbers"));

    std::optional<TypeId> retType = first(functionType->retType);
    REQUIRE(retType);
    CHECK(get<UnionTypeVar>(*retType));
}

TEST_CASE_FIXTURE(Fixture, "infer_higher_order_function")
{
    CheckResult result = check(R"(
        function apply(f, x)
            return f(x)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(requireType("apply"));
    REQUIRE(ftv != nullptr);

    std::vector<TypeId> argVec = flatten(ftv->argTypes).first;

    REQUIRE_EQ(2, argVec.size());

    const FunctionTypeVar* fType = get<FunctionTypeVar>(argVec[0]);
    REQUIRE(fType != nullptr);

    std::vector<TypeId> fArgs = flatten(fType->argTypes).first;

    TypeId xType = argVec[1];

    CHECK_EQ(1, fArgs.size());
    CHECK_EQ(xType, fArgs[0]);
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

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(requireType("bottomupmerge"));
    REQUIRE(ftv != nullptr);

    std::vector<TypeId> argVec = flatten(ftv->argTypes).first;

    REQUIRE_EQ(6, argVec.size());

    const FunctionTypeVar* fType = get<FunctionTypeVar>(argVec[0]);
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

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(requireType("swapTwice"));
    REQUIRE(ftv != nullptr);

    std::vector<TypeId> argVec = flatten(ftv->argTypes).first;

    REQUIRE_EQ(1, argVec.size());

    const TableTypeVar* argType = get<TableTypeVar>(follow(argVec[0]));
    REQUIRE(argType != nullptr);

    CHECK(bool(argType->indexer));
}

TEST_CASE_FIXTURE(Fixture, "higher_order_function_4")
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
    dumpErrors(result);

    /*
     * mergesort takes two arguments: an array of some type T and a function that takes two Ts.
     * We must assert that these two types are in fact the same type.
     * In other words, comp(arr[x], arr[y]) is well-typed.
     */

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(requireType("mergesort"));
    REQUIRE(ftv != nullptr);

    std::vector<TypeId> argVec = flatten(ftv->argTypes).first;

    REQUIRE_EQ(2, argVec.size());

    const TableTypeVar* arg0 = get<TableTypeVar>(follow(argVec[0]));
    REQUIRE(arg0 != nullptr);
    REQUIRE(bool(arg0->indexer));

    const FunctionTypeVar* arg1 = get<FunctionTypeVar>(follow(argVec[1]));
    REQUIRE(arg1 != nullptr);
    REQUIRE_EQ(2, size(arg1->argTypes));

    std::vector<TypeId> arg1Args = flatten(arg1->argTypes).first;

    CHECK_EQ(*arg0->indexer->indexResultType, *arg1Args[0]);
    CHECK_EQ(*arg0->indexer->indexResultType, *arg1Args[1]);
}

TEST_CASE_FIXTURE(Fixture, "error_types_propagate")
{
    CheckResult result = check(R"(
        local err = (true).x
        local c = err.Parent.Reward.GetChildren
        local d = err.Parent.Reward
        local e = err.Parent
        local f = err
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownProperty* err = get<UnknownProperty>(result.errors[0]);
    REQUIRE(err != nullptr);
    CHECK_EQ("boolean", toString(err->table));
    CHECK_EQ("x", err->key);

    CHECK(nullptr != get<ErrorTypeVar>(requireType("c")));
    CHECK(nullptr != get<ErrorTypeVar>(requireType("d")));
    CHECK(nullptr != get<ErrorTypeVar>(requireType("e")));
    CHECK(nullptr != get<ErrorTypeVar>(requireType("f")));
}

TEST_CASE_FIXTURE(Fixture, "calling_error_type_yields_error")
{
    CheckResult result = check(R"(
        local a = unknown.Parent.Reward.GetChildren()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownSymbol* err = get<UnknownSymbol>(result.errors[0]);
    REQUIRE(err != nullptr);

    CHECK_EQ("unknown", err->name);

    CHECK(nullptr != get<ErrorTypeVar>(requireType("a")));
}

TEST_CASE_FIXTURE(Fixture, "chain_calling_error_type_yields_error")
{
    CheckResult result = check(R"(
        local a = Utility.Create "Foo" {}
    )");

    TypeId aType = requireType("a");

    REQUIRE_MESSAGE(nullptr != get<ErrorTypeVar>(aType), "Not an error: " << toString(aType));
}

TEST_CASE_FIXTURE(Fixture, "primitive_arith_no_metatable")
{
    CheckResult result = check(R"(
        function add(a: number, b: string)
            return a + tonumber(b), a .. b
        end
        local n, s = add(2,"3")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const FunctionTypeVar* functionType = get<FunctionTypeVar>(requireType("add"));

    std::optional<TypeId> retType = first(functionType->retType);
    CHECK_EQ(std::optional<TypeId>(typeChecker.numberType), retType);
    CHECK_EQ(requireType("n"), typeChecker.numberType);
    CHECK_EQ(requireType("s"), typeChecker.stringType);
}

TEST_CASE_FIXTURE(Fixture, "primitive_arith_no_metatable_with_follows")
{
    CheckResult result = check(R"(
        local PI=3.1415926535897931
        local SOLAR_MASS=4*PI * PI
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(requireType("SOLAR_MASS"), typeChecker.numberType);
}

TEST_CASE_FIXTURE(Fixture, "primitive_arith_possible_metatable")
{
    CheckResult result = check(R"(
        function add(a: number, b: any)
            return a + b
        end
        local t = add(1,2)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("any", toString(requireType("t")));
}

TEST_CASE_FIXTURE(Fixture, "some_primitive_binary_ops")
{
    CheckResult result = check(R"(
        local a = 4 + 8
        local b = a + 9
        local s = 'hotdogs'
        local t = s .. s
        local c = b - a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number", toString(requireType("a")));
    CHECK_EQ("number", toString(requireType("b")));
    CHECK_EQ("string", toString(requireType("s")));
    CHECK_EQ("string", toString(requireType("t")));
    CHECK_EQ("number", toString(requireType("c")));
}

TEST_CASE_FIXTURE(Fixture, "typecheck_overloaded_multiply_that_is_an_intersection")
{
    CheckResult result = check(R"(
        --!strict
        local Vec3 = {}
        Vec3.__index = Vec3
        function Vec3.new()
            return setmetatable({x=0, y=0, z=0}, Vec3)
        end

        export type Vec3 = typeof(Vec3.new())

        local thefun: any = function(self, o) return self end

        local multiply: ((Vec3, Vec3) -> Vec3) & ((Vec3, number) -> Vec3) = thefun

        Vec3.__mul = multiply

        local a = Vec3.new()
        local b = Vec3.new()
        local c = a * b
        local d = a * 2
        local e = a * 'cabbage'
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Vec3", toString(requireType("a")));
    CHECK_EQ("Vec3", toString(requireType("b")));
    CHECK_EQ("Vec3", toString(requireType("c")));
    CHECK_EQ("Vec3", toString(requireType("d")));
    CHECK(get<ErrorTypeVar>(requireType("e")));
}

TEST_CASE_FIXTURE(Fixture, "typecheck_overloaded_multiply_that_is_an_intersection_on_rhs")
{
    CheckResult result = check(R"(
        --!strict
        local Vec3 = {}
        Vec3.__index = Vec3
        function Vec3.new()
            return setmetatable({x=0, y=0, z=0}, Vec3)
        end

        export type Vec3 = typeof(Vec3.new())

        local thefun: any = function(self, o) return self end

        local multiply: ((Vec3, Vec3) -> Vec3) & ((Vec3, number) -> Vec3) = thefun

        Vec3.__mul = multiply

        local a = Vec3.new()
        local b = Vec3.new()
        local c = b * a
        local d = 2 * a
        local e = 'cabbage' * a
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Vec3", toString(requireType("a")));
    CHECK_EQ("Vec3", toString(requireType("b")));
    CHECK_EQ("Vec3", toString(requireType("c")));
    CHECK_EQ("Vec3", toString(requireType("d")));
    CHECK(get<ErrorTypeVar>(requireType("e")));
}

TEST_CASE_FIXTURE(Fixture, "compare_numbers")
{
    CheckResult result = check(R"(
        local a = 441
        local b = 0
        local c = a < b
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "compare_strings")
{
    CheckResult result = check(R"(
        local a = '441'
        local b = '0'
        local c = a < b
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "cannot_indirectly_compare_types_that_do_not_have_a_metatable")
{
    CheckResult result = check(R"(
        local a = {}
        local b = {}
        local c = a < b
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    GenericError* gen = get<GenericError>(result.errors[0]);

    REQUIRE_EQ(gen->message, "Type a cannot be compared with < because it has no metatable");
}

TEST_CASE_FIXTURE(Fixture, "cannot_indirectly_compare_types_that_do_not_offer_overloaded_ordering_operators")
{
    CheckResult result = check(R"(
        local M = {}
        function M.new()
            return setmetatable({}, M)
        end
        type M = typeof(M.new())

        local a = M.new()
        local b = M.new()
        local c = a < b
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    GenericError* gen = get<GenericError>(result.errors[0]);
    REQUIRE(gen != nullptr);
    REQUIRE_EQ(gen->message, "Table M does not offer metamethod __lt");
}

TEST_CASE_FIXTURE(Fixture, "cannot_compare_tables_that_do_not_have_the_same_metatable")
{
    CheckResult result = check(R"(
        --!strict
        local M = {}
        function M.new()
            return setmetatable({}, M)
        end
        function M.__lt(left, right) return true end

        local a = M.new()
        local b = {}
        local c = a < b -- line 10
        local d = b < a -- line 11
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    REQUIRE_EQ((Location{{10, 18}, {10, 23}}), result.errors[0].location);

    REQUIRE_EQ((Location{{11, 18}, {11, 23}}), result.errors[1].location);
}

TEST_CASE_FIXTURE(Fixture, "produce_the_correct_error_message_when_comparing_a_table_with_a_metatable_with_one_that_does_not")
{
    CheckResult result = check(R"(
        --!strict
        local M = {}
        function M.new()
            return setmetatable({}, M)
        end
        function M.__lt(left, right) return true end
        type M = typeof(M.new())

        local a = M.new()
        local b = {}
        local c = a < b -- line 10
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto err = get<GenericError>(result.errors[0]);
    REQUIRE(err != nullptr);

    // Frail. :|
    REQUIRE_EQ("Types M and b cannot be compared with < because they do not have the same metatable", err->message);
}

TEST_CASE_FIXTURE(Fixture, "in_nonstrict_mode_strip_nil_from_intersections_when_considering_relational_operators")
{
    CheckResult result = check(R"(
        --!nonstrict

        function maybe_a_number(): number?
            return 50
        end

        local a = maybe_a_number() < maybe_a_number()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

/*
 * This test case exposed an oversight in the treatment of free tables.
 * Free tables, like free TypeVars, need to record the scope depth where they were created so that
 * we do not erroneously let-generalize them when they are used in a nested lambda.
 *
 * For more information about let-generalization, see <http://okmij.org/ftp/ML/generalization.html>
 *
 * The important idea here is that the return type of Counter.new is a table with some metatable.
 * That metatable *must* be the same TypeVar as the type of Counter.  If it is a copy (produced by
 * the generalization process), then it loses the knowledge that its metatable will have an :incr()
 * method.
 */
TEST_CASE_FIXTURE(Fixture, "dont_quantify_table_that_belongs_to_outer_scope")
{
    CheckResult result = check(R"(
        local Counter = {}
        Counter.__index = Counter

        function Counter.new()
            local self = setmetatable({count=0}, Counter)
            return self
        end

        function Counter:incr()
            self.count = 1
            return self.count
        end

        local self = Counter.new()
        print(self:incr())
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TableTypeVar* counterType = getMutable<TableTypeVar>(requireType("Counter"));
    REQUIRE(counterType);

    const FunctionTypeVar* newType = get<FunctionTypeVar>(follow(counterType->props["new"].type));
    REQUIRE(newType);

    std::optional<TypeId> newRetType = *first(newType->retType);
    REQUIRE(newRetType);

    const MetatableTypeVar* newRet = get<MetatableTypeVar>(follow(*newRetType));
    REQUIRE(newRet);

    const TableTypeVar* newRetMeta = get<TableTypeVar>(newRet->metatable);
    REQUIRE(newRetMeta);

    CHECK(newRetMeta->props.count("incr"));
    CHECK_EQ(follow(newRet->metatable), follow(requireType("Counter")));
}

// TODO: CLI-39624
TEST_CASE_FIXTURE(Fixture, "instantiate_tables_at_scope_level")
{
    CheckResult result = check(R"(
        --!strict
        local Option = {}
        Option.__index = Option
        function Option.Is(obj)
                return (type(obj) == "table" and getmetatable(obj) == Option)
        end
        return Option
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "typeguard_doesnt_leak_to_elseif")
{
    const std::string code = R"(
        function f(a)
           if type(a) == "boolean" then
                local a1 = a
            elseif a.fn() then
                local a2 = a
            else
                local a3 = a
            end
        end
    )";
    CheckResult result = check(code);
    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);
}

TEST_CASE_FIXTURE(Fixture, "should_be_able_to_infer_this_without_stack_overflowing")
{
    CheckResult result = check(R"(
        local function f(x, y)
            return x or y
        end

        local function dont_crash(x, y)
            local z: typeof(f(x, y)) = f(x, y)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "x_or_y_forces_both_x_and_y_to_be_of_same_type_if_either_is_free")
{
    CheckResult result = check(R"(
        local function f(x, y) return x or y end

        local x = f(1, 2)
        local y = f(3, "foo")
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(*requireType("x"), *typeChecker.numberType);

    CHECK_EQ(result.errors[0], (TypeError{Location{{4, 23}, {4, 28}}, TypeMismatch{typeChecker.numberType, typeChecker.stringType}}));
}

TEST_CASE_FIXTURE(Fixture, "inferring_hundreds_of_self_calls_should_not_suffocate_memory")
{
    CheckResult result = check(R"(
        ("foo")
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
    )");

    ModulePtr module = getMainModule();
    CHECK_GE(50, module->internalTypes.typeVars.size());
}

TEST_CASE_FIXTURE(Fixture, "inferring_crazy_table_should_also_be_quick")
{
    CheckResult result = check(R"(
        --!strict
        function f(U)
            U(w:s(an):c()():c():U(s):c():c():U(s):c():U(s):cU()):c():U(s):c():U(s):c():c():U(s):c():U(s):cU() 
        end
    )");

    ModulePtr module = getMainModule();
    CHECK_GE(100, module->internalTypes.typeVars.size());
}

TEST_CASE_FIXTURE(Fixture, "exponential_blowup_from_copying_types")
{
    CheckResult result = check(R"(
        --!strict
        -- An example of exponential blowup in number of types
        -- The problem is that if we define function f(a) return x end
        -- then this has type <t>(t)->T where x:T
        -- *but* it copies T each time f is applied
        -- so { left = f("hi"), right = f(5) }
        -- has type { left : T_L, right : T_R }
        -- where T_L and T_R are copies of T.
        -- x0 : T0 where T0 = {}
        local x0 = {}
        -- f0 : <t>(t)->T0
        local function f0(a) return x0 end
        -- x1 : T1 where T1 = { left : T0_L, right : T0_R }
        local x1 = { left = f0("hi"), right = f0(5) }
        -- f1 : <t>(t)->T1
        local function f1(a) return x1 end
        -- x2 : T2 where T2 = { left : T1_L, right : T1_R }
        local x2 = { left = f1("hi"), right = f1(5) }
        -- f2 : <t>(t)->T2
        local function f2(a) return x2 end
        -- etc etc
        local x3 = { left = f2("hi"), right = f2(5) }
        local function f3(a) return x3 end
        local x4 = { left = f3("hi"), right = f3(5) }
        return x4
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    ModulePtr module = getMainModule();

    // If we're not careful about copying, this ends up with O(2^N) types rather than O(N)
    // (in this case 5 vs 31).
    CHECK_GE(5, module->interfaceTypes.typeVars.size());
}

TEST_CASE_FIXTURE(Fixture, "mutual_recursion")
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
    dumpErrors(result);
}

TEST_CASE_FIXTURE(Fixture, "toposort_doesnt_break_mutual_recursion")
{
    CheckResult result = check(R"(
        --!strict
        local x = nil
        function f() g() end
        -- make sure print(x) doen't get toposorted here, breaking the mutual block
        function g() x = f end
        print(x)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);
}

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_types")
{
    CheckResult result = check(R"(
        --!strict
        type T<a> = { f: a, g: U<a> }
        type U<a> = { h: a, i: T<a>? }
        local x: T<number> = { f = 37, g = { h = 5, i = nil } }
        x.g.i = x
        local y: T<string> = { f = "hi", g = { h = "lo", i = nil } }
        y.g.i = y
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_types_errors")
{
    CheckResult result = check(R"(
        --!strict
        type T<a> = { f: a, g: U<a> }
        type U<b> = { h: b, i: T<b>? }
        local x: T<number> = { f = 37, g = { h = 5, i = nil } }
        x.g.i = x
        local y: T<string> = { f = "hi", g = { h = 5, i = nil } }
        y.g.i = y
    )");

    LUAU_REQUIRE_ERRORS(result);

    // We had a UAF in this example caused by not cloning type function arguments
    ModulePtr module = frontend.moduleResolver.getModule("MainModule");
    unfreeze(module->interfaceTypes);
    copyErrors(module->errors, module->interfaceTypes);
    freeze(module->interfaceTypes);
    module->internalTypes.clear();
    module->astTypes.clear();

    // Make sure the error strings don't include "VALUELESS"
    for (auto error : module->errors)
        CHECK_MESSAGE(toString(error).find("VALUELESS") == std::string::npos, toString(error));
}

TEST_CASE_FIXTURE(Fixture, "object_constructor_can_refer_to_method_of_self")
{
    // CLI-30902
    CheckResult result = check(R"(
        --!strict

        type Foo = {
            fooConn: () -> () | nil
        }

        local Foo = {}
        Foo.__index = Foo

        function Foo.new()
            local self: Foo = {
                fooConn = nil,
            }
            setmetatable(self, Foo)

            self.fooConn = function()
                self:method() -- Key 'method' not found in table self
            end

            return self
        end

        function Foo:method()
            print("foo")
        end

        local foo = Foo.new()

        -- TODO This is the best our current refinement support can offer :(
        local bar = foo.fooConn
        if bar then bar() end

        -- foo.fooConn()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "replace_every_free_type_when_unifying_a_complex_function_with_any")
{
    CheckResult result = check(R"(
        local a: any
        local b
        for _, i in pairs(a) do
            b = i
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("any", toString(requireType("b")));
}

// In these tests, a successful parse is required, so we need the parser to return the AST and then we can test the recursion depth limit in type
// checker. We also want it to somewhat match up with production values, so we push up the parser recursion limit a little bit instead.
TEST_CASE_FIXTURE(Fixture, "check_type_infer_recursion_limit")
{
#if defined(LUAU_ENABLE_ASAN)
    int limit = 250;
#elif defined(_DEBUG) || defined(_NOOPT)
    int limit = 350;
#else
    int limit = 600;
#endif
    ScopedFastInt luauRecursionLimit{"LuauRecursionLimit", limit + 100};
    ScopedFastInt luauTypeInferRecursionLimit{"LuauTypeInferRecursionLimit", limit - 100};
    ScopedFastInt luauCheckRecursionLimit{"LuauCheckRecursionLimit", 0};

    CHECK_NOTHROW(check("print('Hello!')"));
    CHECK_THROWS_AS(check("function f() return " + rep("{a=", limit) + "'a'" + rep("}", limit) + " end"), std::runtime_error);
}

TEST_CASE_FIXTURE(Fixture, "check_block_recursion_limit")
{
#if defined(LUAU_ENABLE_ASAN)
    int limit = 250;
#elif defined(_DEBUG) || defined(_NOOPT)
    int limit = 350;
#else
    int limit = 600;
#endif

    ScopedFastInt luauRecursionLimit{"LuauRecursionLimit", limit + 100};
    ScopedFastInt luauCheckRecursionLimit{"LuauCheckRecursionLimit", limit - 100};

    CheckResult result = check(rep("do ", limit) + "local a = 1" + rep(" end", limit));

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(nullptr != get<CodeTooComplex>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "check_expr_recursion_limit")
{
#if defined(LUAU_ENABLE_ASAN)
    int limit = 250;
#elif defined(_DEBUG) || defined(_NOOPT)
    int limit = 350;
#else
    int limit = 600;
#endif
    ScopedFastInt luauRecursionLimit{"LuauRecursionLimit", limit + 100};
    ScopedFastInt luauCheckRecursionLimit{"LuauCheckRecursionLimit", limit - 100};

    CheckResult result = check(R"(("foo"))" + rep(":lower()", limit));

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(nullptr != get<CodeTooComplex>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "compound_assign_basic")
{
    CheckResult result = check(R"(
        local s = 10
        s += 20
    )");
    CHECK_EQ(0, result.errors.size());
    CHECK_EQ(toString(*requireType("s")), "number");
}

TEST_CASE_FIXTURE(Fixture, "compound_assign_mismatch_op")
{
    CheckResult result = check(R"(
        local s = 10
        s += true
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(result.errors[0], (TypeError{Location{{2, 13}, {2, 17}}, TypeMismatch{typeChecker.numberType, typeChecker.booleanType}}));
}

TEST_CASE_FIXTURE(Fixture, "compound_assign_mismatch_result")
{
    CheckResult result = check(R"(
        local s = 'hello'
        s += 10
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ(result.errors[0], (TypeError{Location{{2, 8}, {2, 9}}, TypeMismatch{typeChecker.numberType, typeChecker.stringType}}));
    CHECK_EQ(result.errors[1], (TypeError{Location{{2, 8}, {2, 15}}, TypeMismatch{typeChecker.stringType, typeChecker.numberType}}));
}

TEST_CASE_FIXTURE(Fixture, "compound_assign_metatable")
{
    CheckResult result = check(R"(
        --!strict
        type V2B = { x: number, y: number }
        local v2b: V2B = { x = 0, y = 0 }
        local VMT = {}
        type V2 = typeof(setmetatable(v2b, VMT))

        function VMT.__add(a: V2, b: V2): V2
            return setmetatable({ x = a.x + b.x, y = a.y + b.y }, VMT)
        end

        local v1: V2 = setmetatable({ x = 1, y = 2 }, VMT)
        local v2: V2 = setmetatable({ x = 3, y = 4 }, VMT)
        v1 += v2
    )");
    CHECK_EQ(0, result.errors.size());
}

TEST_CASE_FIXTURE(Fixture, "compound_assign_mismatch_metatable")
{
    CheckResult result = check(R"(
        --!strict
        type V2B = { x: number, y: number }
        local v2b: V2B = { x = 0, y = 0 }
        local VMT = {}
        type V2 = typeof(setmetatable(v2b, VMT))

        function VMT.__mod(a: V2, b: V2): number
            return a.x * b.x + a.y * b.y
        end

        local v1: V2 = setmetatable({ x = 1, y = 2 }, VMT)
        local v2: V2 = setmetatable({ x = 3, y = 4 }, VMT)
        v1 %= v2
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    CHECK_EQ(*tm->wantedType, *requireType("v2"));
    CHECK_EQ(*tm->givenType, *typeChecker.numberType);
}

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

TEST_CASE_FIXTURE(Fixture, "it_is_ok_to_oversaturate_a_higher_order_function_argument")
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

TEST_CASE_FIXTURE(Fixture, "call_to_any_yields_any")
{
    CheckResult result = check(R"(
        local a: any
        local b = a()
    )");

    REQUIRE_EQ("any", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "globals")
{
    CheckResult result = check(R"(
        --!nonstrict
        foo = true
        foo = "now i'm a string!"
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("any", toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "globals2")
{
    CheckResult result = check(R"(
        --!nonstrict
        foo = function() return 1 end
        foo = "now i'm a string!"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("() -> (...any)", toString(tm->wantedType));
    CHECK_EQ("string", toString(tm->givenType));
    CHECK_EQ("() -> (...any)", toString(requireType("foo")));
}

TEST_CASE_FIXTURE(Fixture, "globals_are_banned_in_strict_mode")
{
    CheckResult result = check(R"(
        --!strict
        foo = true
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownSymbol* us = get<UnknownSymbol>(result.errors[0]);
    REQUIRE(us);
    CHECK_EQ("foo", us->name);
}

TEST_CASE_FIXTURE(Fixture, "globals_everywhere")
{
    CheckResult result = check(R"(
        --!nonstrict
        foo = 1

        if true then
            bar = 2
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("any", toString(requireType("foo")));
    CHECK_EQ("any", toString(requireType("bar")));
}

TEST_CASE_FIXTURE(Fixture, "CheckMethodsOfAny")
{
    CheckResult result = check(R"(
local x: any = {}
function x:y(z: number)
    local s: string = z
end
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "CheckMethodsOfSealed")
{
    CheckResult result = check(R"(
local x: {prop: number} = {prop=9999}
function x:y(z: number)
    local s: string = z
end
)");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
}

TEST_CASE_FIXTURE(Fixture, "CheckMethodsOfNumber")
{
    CheckResult result = check(R"(
local x: number = 9999
function x:y(z: number)
    local s: string = z
end
)");

    LUAU_REQUIRE_ERROR_COUNT(3, result);
}

TEST_CASE_FIXTURE(Fixture, "CheckMethodsOfError")
{
    CheckResult result = check(R"(
local x = (true).foo
function x:y(z: number)
    local s: string = z
end
)");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
}

TEST_CASE_FIXTURE(Fixture, "CallOrOfFunctions")
{
    CheckResult result = check(R"(
function f() return 1; end
function g() return 2; end
(f or g)()
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "CallAndOrOfFunctions")
{
    CheckResult result = check(R"(
function f() return 1; end
function g() return 2; end
local x = false
(x and f or g)()
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "MixedPropertiesAndIndexers")
{
    CheckResult result = check(R"(
local x = {}
x.a = "a"
x[0] = true
x.b = 37
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "correctly_scope_locals_do")
{
    CheckResult result = check(R"(
        do
            local a = 1
        end

        print(a) -- oops!
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownSymbol* us = get<UnknownSymbol>(result.errors[0]);
    REQUIRE(us);
    CHECK_EQ(us->name, "a");
}

TEST_CASE_FIXTURE(Fixture, "correctly_scope_locals_while")
{
    CheckResult result = check(R"(
        while true do
            local a = 1
        end

        print(a) -- oops!
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownSymbol* us = get<UnknownSymbol>(result.errors[0]);
    REQUIRE(us);
    CHECK_EQ(us->name, "a");
}

TEST_CASE_FIXTURE(Fixture, "ipairs_produces_integral_indeces")
{
    CheckResult result = check(R"(
        local key
        for i, e in ipairs({}) do key = i end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    REQUIRE_EQ("number", toString(requireType("key")));
}

TEST_CASE_FIXTURE(Fixture, "checking_should_not_ice")
{
    CHECK_NOTHROW(check(R"(
        --!nonstrict
        f,g = ...
        f(g(...))[...] = nil
        f,xpcall = ...
        local value = g(...)(g(...))
    )"));

    CHECK_EQ("any", toString(requireType("value")));
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

// TEST_CASE_FIXTURE(Fixture, "infer_method_signature_of_argument")
// {
//     CheckResult result = check(R"(
//         function f(a)
//             if a.cond then
//                 return a.method()
//             end
//         end
//     )");

//     LUAU_REQUIRE_NO_ERRORS(result);

//     CHECK_EQ("A", toString(requireType("f")));
// }

TEST_CASE_FIXTURE(Fixture, "warn_if_you_try_to_require_a_non_modulescript")
{
    fileResolver.source["Modules/A"] = "";
    fileResolver.sourceTypes["Modules/A"] = SourceCode::Local;

    fileResolver.source["Modules/B"] = R"(
        local M = require(script.Parent.A)
    )";

    CheckResult result = frontend.check("Modules/B");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK(get<IllegalRequire>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "calling_function_with_incorrect_argument_type_yields_errors_spanning_argument")
{
    CheckResult result = check(R"(
        function foo(a: number, b: string) end

        foo("Test", 123)
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    CHECK_EQ(result.errors[0], (TypeError{Location{Position{3, 12}, Position{3, 18}}, TypeMismatch{
                                                                                          typeChecker.numberType,
                                                                                          typeChecker.stringType,
                                                                                      }}));

    CHECK_EQ(result.errors[1], (TypeError{Location{Position{3, 20}, Position{3, 23}}, TypeMismatch{
                                                                                          typeChecker.stringType,
                                                                                          typeChecker.numberType,
                                                                                      }}));
}

TEST_CASE_FIXTURE(Fixture, "calling_function_with_anytypepack_doesnt_leak_free_types")
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
    CHECK(acm->context == CountMismatch::Result);
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
}

TEST_CASE_FIXTURE(Fixture, "typecheck_unary_minus")
{
    CheckResult result = check(R"(
        --!strict
        local foo = {
            value = 10
        }
        local mt = {}
        setmetatable(foo, mt)

        mt.__unm = function(val: typeof(foo)): string
            return val.value .. "test"
        end

        local a = -foo

        local b = 1+-1

        local bar = {
            value = 10
        }
        local c = -bar -- disallowed
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("string", toString(requireType("a")));
    CHECK_EQ("number", toString(requireType("b")));

    GenericError* gen = get<GenericError>(result.errors[0]);
    REQUIRE_EQ(gen->message, "Unary operator '-' not supported by type 'bar'");
}

TEST_CASE_FIXTURE(Fixture, "unary_not_is_boolean")
{
    CheckResult result = check(R"(
        local b = not "string"
        local c = not (math.random() > 0.5 and "string" or 7)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    REQUIRE_EQ("boolean", toString(requireType("b")));
    REQUIRE_EQ("boolean", toString(requireType("c")));
}

TEST_CASE_FIXTURE(Fixture, "disallow_string_and_types_without_metatables_from_arithmetic_binary_ops")
{
    CheckResult result = check(R"(
        --!strict
        local a = "1.24" + 123 -- not allowed

        local foo = {
            value = 10
        }

        local b = foo + 1 -- not allowed

        local bar = {
            value = 1
        }

        local mt = {}

        setmetatable(bar, mt)

        mt.__add = function(a: typeof(bar), b: number): number
            return a.value + b
        end

        local c = bar + 1 -- allowed

        local d = bar + foo -- not allowed
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE_EQ(*tm->wantedType, *typeChecker.numberType);
    REQUIRE_EQ(*tm->givenType, *typeChecker.stringType);

    TypeMismatch* tm2 = get<TypeMismatch>(result.errors[2]);
    CHECK_EQ(*tm2->wantedType, *typeChecker.numberType);
    CHECK_EQ(*tm2->givenType, *requireType("foo"));

    GenericError* gen2 = get<GenericError>(result.errors[1]);
    REQUIRE_EQ(gen2->message, "Binary operator '+' not supported by types 'foo' and 'number'");
}

// CLI-29033
TEST_CASE_FIXTURE(Fixture, "unknown_type_in_comparison")
{
    CheckResult result = check(R"(
        function merge(lower, greater)
            if lower.y == greater.y then
            end
        end
    )");

    if (FFlag::LuauEqConstraint)
    {
        LUAU_REQUIRE_NO_ERRORS(result);
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);

        REQUIRE(get<CannotInferBinaryOperation>(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "relation_op_on_any_lhs_where_rhs_maybe_has_metatable")
{
    CheckResult result = check(R"(
        local x
        print((x == true and (x .. "y")) .. 1) 
    )");

    if (FFlag::LuauEqConstraint)
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        REQUIRE(get<CannotInferBinaryOperation>(result.errors[0]));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);

        CHECK_EQ("Type 'boolean' could not be converted into 'number | string'", toString(result.errors[0]));
        CHECK_EQ("Type 'boolean | string' could not be converted into 'number | string'", toString(result.errors[1]));
    }
}

TEST_CASE_FIXTURE(Fixture, "concat_op_on_string_lhs_and_free_rhs")
{
    CheckResult result = check(R"(
        local x
        print("foo" .. x)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string", toString(requireType("x")));
}

TEST_CASE_FIXTURE(Fixture, "strict_binary_op_where_lhs_unknown")
{
    std::vector<std::string> ops = {"+", "-", "*", "/", "%", "^", ".."};

    std::string src = R"(
        function foo(a, b)
    )";

    for (const auto& op : ops)
        src += "local _ = a " + op + "b\n";

    src += "end";

    CheckResult result = check(src);
    LUAU_REQUIRE_ERROR_COUNT(ops.size(), result);

    CHECK_EQ("Unknown type used in + operation; consider adding a type annotation to 'a'", toString(result.errors[0]));
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
    CHECK_EQ("(string, *unknown*) -> number", toString(tm1->givenType));

    auto tm2 = get<TypeMismatch>(result.errors[1]);
    REQUIRE(tm2);

    CHECK_EQ("(number, number) -> (number, number)", toString(tm2->wantedType));
    CHECK_EQ("(string, *unknown*) -> number", toString(tm2->givenType));
}

TEST_CASE_FIXTURE(Fixture, "setmetatable_cant_be_used_to_mutate_global_types")
{
    {
        Fixture fix;

        // inherit env from parent fixture checker
        fix.typeChecker.globalScope = typeChecker.globalScope;

        fix.check(R"(
--!nonstrict
type MT = typeof(setmetatable)
function wtf(arg: {MT}): typeof(table)
    arg = wtf(arg)
end
)");
    }

    // validate sharedEnv post-typecheck; valuable for debugging some typeck crashes but slows fuzzing down
    // note: it's important for typeck to be destroyed at this point!
    {
        for (auto& p : typeChecker.globalScope->bindings)
        {
            toString(p.second.typeId); // toString walks the entire type, making sure ASAN catches access to destroyed type arenas
        }
    }
}

TEST_CASE_FIXTURE(Fixture, "evil_table_unification")
{
    // this code re-infers the type of _ while processing fields of _, which can cause use-after-free
    check(R"(
--!nonstrict
_ = ...
_:table(_,string)[_:gsub(_,...,n0)],_,_:gsub(_,string)[""],_:split(_,...,table)._,n0 = nil
do end
)");
}

TEST_CASE_FIXTURE(Fixture, "overload_is_not_a_function")
{
    check(R"(
--!nonstrict
function _(...):((typeof(not _))&(typeof(not _)))&((typeof(not _))&(typeof(not _)))
_(...)(setfenv,_,not _,"")[_] = nil
end
do end
_(...)(...,setfenv,_):_G()
)");
}

TEST_CASE_FIXTURE(Fixture, "use_table_name_and_generic_params_in_errors")
{
    CheckResult result = check(R"(
        type Pair<T, U> = {first: T, second: U}
        local a: Pair<string, number>
        local b: Pair<string, string>

        a = b
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);

    CHECK_EQ("Pair<string, number>", toString(tm->wantedType));
    CHECK_EQ("Pair<string, string>", toString(tm->givenType));
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

TEST_CASE_FIXTURE(Fixture, "cyclic_follow")
{
    check(R"(
--!nonstrict
l0,table,_,_,_ = ...
_,_,_,_.time(...)._.n0,l0,_ = function(l0)
end,_.__index,(_),_.time(_.n0 or _,...)
for l0=...,_,"" do
end
_ += not _
do end
)");

    check(R"(
--!nonstrict
n13,_,table,_,l0,_,_ = ...
_,n0[(_)],_,_._(...)._.n39,l0,_._ = function(l84,...)
end,_.__index,"",_,l0._(nil)
for l0=...,table.n5,_ do
end
_:_(...).n1 /= _
do
_(_ + _)
do end
end
)");
}

TEST_CASE_FIXTURE(Fixture, "and_binexps_dont_unify")
{
    CheckResult result = check(R"(
    --!strict
    local t = {}
    while true and t[1] do
        print(t[1].test)
    end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

struct FindFreeTypeVars
{
    bool foundOne = false;

    template<typename ID>
    void cycle(ID)
    {
    }

    template<typename ID, typename T>
    bool operator()(ID, T)
    {
        return !foundOne;
    }

    template<typename ID>
    bool operator()(ID, Unifiable::Free)
    {
        foundOne = true;
        return false;
    }
};

TEST_CASE_FIXTURE(Fixture, "dont_crash_when_setmetatable_does_not_produce_a_metatabletypevar")
{
    CheckResult result = check("local x = setmetatable({})");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "for_in_loop_where_iteratee_is_free")
{
    // This code doesn't pass typechecking.  We just care that it doesn't crash.
    (void)check(R"(
        --!nonstrict
        function _:_(...)
        end

        repeat
            if _ then
            else
                _ = ...
            end
        until _

        for _ in _() do
        end
    )");
}

TEST_CASE_FIXTURE(Fixture, "dont_stop_typechecking_after_reporting_duplicate_type_definition")
{
    CheckResult result = check(R"(
        type A = number
        type A = string -- Redefinition of type 'A', previously defined at line 1
        local foo: string = 1 -- No "Type 'number' could not be converted into 'string'"
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
}

TEST_CASE_FIXTURE(Fixture, "tc_after_error_recovery")
{
    CheckResult result = check(R"(
        local x =
        local a = 7
    )");
    LUAU_REQUIRE_ERRORS(result);

    TypeId aType = requireType("a");
    CHECK_EQ(getPrimitiveType(aType), PrimitiveTypeVar::Number);
}

// Check that type checker knows about error expressions
TEST_CASE_FIXTURE(Fixture, "tc_after_error_recovery_no_assert")
{
    CheckResult result = check("function +() local _ = true end");
    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "tc_after_error_recovery_no_replacement_name_in_error")
{
    {
        CheckResult result = check(R"(
            --!strict
            local t = { x = 10, y = 20 }
            return t.
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
    }

    {
        CheckResult result = check(R"(
            --!strict
            export type = number
            export type = string
        )");

        LUAU_REQUIRE_ERROR_COUNT(2, result);
    }

    {
        CheckResult result = check(R"(
            --!strict
            function string.() end
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
    }

    {
        CheckResult result = check(R"(
            --!strict
            local function () end
            local function () end
        )");

        LUAU_REQUIRE_ERROR_COUNT(2, result);
    }

    {
        CheckResult result = check(R"(
            --!strict
            local dm = {}
            function dm.() end
            function dm.() end
        )");

        LUAU_REQUIRE_ERROR_COUNT(2, result);
    }
}

TEST_CASE_FIXTURE(Fixture, "error_on_invalid_operand_types_to_relational_operators")
{
    CheckResult result = check(R"(
        local a: boolean = true
        local b: boolean = false
        local foo = a < b
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    GenericError* ge = get<GenericError>(result.errors[0]);
    REQUIRE(ge);
    CHECK_EQ("Type 'boolean' cannot be compared with relational operator <", ge->message);
}

TEST_CASE_FIXTURE(Fixture, "error_on_invalid_operand_types_to_relational_operators2")
{
    CheckResult result = check(R"(
        local a: number | string = ""
        local b: number | string = 1
        local foo = a < b
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    GenericError* ge = get<GenericError>(result.errors[0]);
    REQUIRE(ge);
    CHECK_EQ("Type 'number | string' cannot be compared with relational operator <", ge->message);
}

TEST_CASE_FIXTURE(Fixture, "stringify_type_alias_of_recursive_template_table_type")
{
    CheckResult result = check(R"(
        type Table<T> = { a: T }
        type Wrapped = Table<Wrapped>
        local l: Wrapped = 2
        )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("Wrapped", toString(tm->wantedType));
    CHECK_EQ(typeChecker.numberType, tm->givenType);
}

TEST_CASE_FIXTURE(Fixture, "stringify_type_alias_of_recursive_template_table_type2")
{
    CheckResult result = check(R"(
        type Table<T> = { a: T }
        type Wrapped = (Table<Wrapped>) -> string
        local l: Wrapped = 2
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("t1 where t1 = ({| a: t1 |}) -> string", toString(tm->wantedType));
    CHECK_EQ(typeChecker.numberType, tm->givenType);
}

TEST_CASE_FIXTURE(Fixture, "index_expr_should_be_checked")
{
    CheckResult result = check(R"(
        local foo: any

        print(foo[(true).x])
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    UnknownProperty* up = get<UnknownProperty>(result.errors[0]); // Should probably be NotATable
    REQUIRE(up);
    CHECK_EQ("boolean", toString(up->table));
    CHECK_EQ("x", up->key);
}

TEST_CASE_FIXTURE(Fixture, "unreachable_code_after_infinite_loop")
{
    {
        CheckResult result = check(R"(
            function unreachablecodepath(a): number
                while true do
                    if a then return 10 end
                end
                -- unreachable
            end
            unreachablecodepath(4)
        )");

        LUAU_REQUIRE_ERROR_COUNT(0, result);
    }

    {
        CheckResult result = check(R"(
            function reachablecodepath(a): number
                while true do
                    if a then break end
                    return 10
                end

                print("x") -- correct error
            end
            reachablecodepath(4)
        )");

        LUAU_REQUIRE_ERRORS(result);
        CHECK(get<FunctionExitsWithoutReturning>(result.errors[0]));
    }

    {
        CheckResult result = check(R"(
            function unreachablecodepath(a): number
                repeat
                    if a then return 10 end
                until false

                -- unreachable
            end
            unreachablecodepath(4)
        )");

        LUAU_REQUIRE_ERROR_COUNT(0, result);
    }

    {
        CheckResult result = check(R"(
            function reachablecodepath(a, b): number
                repeat
                    if a then break end

                    if b then return 10 end
                until false

                print("x") -- correct error
            end
            reachablecodepath(4)
        )");

        LUAU_REQUIRE_ERRORS(result);
        CHECK(get<FunctionExitsWithoutReturning>(result.errors[0]));
    }

    {
        CheckResult result = check(R"(
            function unreachablecodepath(a: number?): number
                repeat
                    return 10
                until a ~= nil

                -- unreachable
            end
            unreachablecodepath(4)
        )");

        LUAU_REQUIRE_ERROR_COUNT(0, result);
    }
}

TEST_CASE_FIXTURE(Fixture, "cli_38355_recursive_union")
{
    CheckResult result = check(R"(
        --!strict
        local _
        _ += _ and _ or _ and _ or _ and _
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type contains a self-recursive construct that cannot be resolved", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "stringify_nested_unions_with_optionals")
{
    CheckResult result = check(R"(
        --!strict
        local a: number | (string | boolean) | nil
        local b: number = a
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ(typeChecker.numberType, tm->wantedType);
    CHECK_EQ("(boolean | number | string)?", toString(tm->givenType));
}

// Check that recursive intersection type doesn't generate an OOM
TEST_CASE_FIXTURE(Fixture, "cli_38393_recursive_intersection_oom")
{
    CheckResult result = check(R"(
        function _(l0:(t0)&((t0)&(((t0)&((t0)->()))->(typeof(_),typeof(# _)))),l39,...):any
        end
        type t0<t0> = ((typeof(_))&((t0)&(((typeof(_))&(t0))->typeof(_))),{n163:any,})->(any,typeof(_))
        _(_)
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "UnknownGlobalCompoundAssign")
{
    // In non-strict mode, global definition is still allowed
    {
        CheckResult result = check(R"(
            --!nonstrict
            a = a + 1
            print(a)
        )");

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK_EQ(toString(result.errors[0]), "Unknown global 'a'");
    }

    // In strict mode we no longer generate two errors from lhs
    {
        CheckResult result = check(R"(
            --!strict
            a += 1
            print(a)
        )");

        LUAU_REQUIRE_ERROR_COUNT(2, result);
        CHECK_EQ(toString(result.errors[0]), "Unknown global 'a'");
    }

    // In non-strict mode, compound assignment is not a definition, it's a modification
    {
        CheckResult result = check(R"(
            --!nonstrict
            a += 1
            print(a)
        )");

        LUAU_REQUIRE_ERROR_COUNT(2, result);
        CHECK_EQ(toString(result.errors[0]), "Unknown global 'a'");
    }
}

TEST_CASE_FIXTURE(Fixture, "loop_typecheck_crash_on_empty_optional")
{
    CheckResult result = check(R"(
        local t = {}
        for _ in t do
            for _ in assert(missing()) do
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
}

TEST_CASE_FIXTURE(Fixture, "type_alias_fwd_declaration_is_precise")
{
    CheckResult result = check(R"(
        local foo: Id<number> = 1
        type Id<T> = T
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "cli_39932_use_unifier_in_ensure_methods")
{
    CheckResult result = check(R"(
        local x: {number|number} = {1, 2, 3}
        local y = x[1] - x[2]
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "metatable_of_any_can_be_a_table")
{
    CheckResult result = check(R"(
--!strict
local T: any
T = {}
T.__index = T
function T.new(...)
	local self = {}
	setmetatable(self, T)
	self:construct(...)
	return self
end
function T:construct(index)
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "corecursive_types_generic")
{
    const std::string code = R"(
        type A<T> = {v:T, b:B<T>}
        type B<T> = {v:T, a:A<T>}
        local aa:A<number>
        local bb = aa
    )";

    const std::string expected = R"(
        type A<T> = {v:T, b:B<T>}
        type B<T> = {v:T, a:A<T>}
        local aa:A<number>
        local bb:A<number>=aa
    )";

    CHECK_EQ(expected, decorateWithTypes(code));
    CheckResult result = check(code);

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "corecursive_function_types")
{
    ScopedFastFlag sff{"LuauOccursCheckOkWithRecursiveFunctions", true};

    CheckResult result = check(R"(
        type A = () -> (number, B)
        type B = () -> (string, A)
        local a: A
        local b: B
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("t1 where t1 = () -> (number, () -> (string, t1))", toString(requireType("a")));
    CHECK_EQ("t1 where t1 = () -> (string, () -> (number, t1))", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "generic_param_remap")
{
    const std::string code = R"(
        -- An example of a forwarded use of a type that has different type arguments than parameters
        type A<T,U> = {t:T, u:U, next:A<U,T>?}
        local aa:A<number,string> = { t = 5, u = 'hi', next = { t = 'lo', u = 8 } }
        local bb = aa
    )";

    const std::string expected = R"(

        type A<T,U> = {t:T, u:U, next:A<U,T>?}
        local aa:A<number,string> = { t = 5, u = 'hi', next = { t = 'lo', u = 8 } }
        local bb:A<number,string>=aa
    )";

    CHECK_EQ(expected, decorateWithTypes(code));
    CheckResult result = check(code);

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "export_type_and_type_alias_are_duplicates")
{
    CheckResult result = check(R"(
        export type Foo = number
        type Foo = number
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto dtd = get<DuplicateTypeDefinition>(result.errors[0]);
    REQUIRE(dtd);
    CHECK_EQ(dtd->name, "Foo");
}

TEST_CASE_FIXTURE(Fixture, "dont_report_type_errors_within_an_AstStatError")
{
    CheckResult result = check(R"(
        foo
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "dont_report_type_errors_within_an_AstExprError")
{
    CheckResult result = check(R"(
        local a = foo:
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
}

TEST_CASE_FIXTURE(Fixture, "dont_ice_on_astexprerror")
{
    CheckResult result = check(R"(
        local foo = -;
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "strip_nil_from_lhs_or_operator")
{
    CheckResult result = check(R"(
--!strict
local a: number? = nil
local b: number = a or 1
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "strip_nil_from_lhs_or_operator2")
{
    CheckResult result = check(R"(
--!nonstrict
local a: number? = nil
local b: number = a or 1
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "dont_strip_nil_from_rhs_or_operator")
{
    CheckResult result = check(R"(
--!strict
local a: number? = nil
local b: number = 1 or a
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ(typeChecker.numberType, tm->wantedType);
    CHECK_EQ("number?", toString(tm->givenType));
}

TEST_CASE_FIXTURE(Fixture, "no_lossy_function_type")
{
    ScopedFastFlag sffs2{"LuauGenericFunctions", true};

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
    auto ftv = get<FunctionTypeVar>(type);
    REQUIRE(ftv);
    CHECK(ftv->hasSelf);
}

TEST_CASE_FIXTURE(Fixture, "luau_resolves_symbols_the_same_way_lua_does")
{
    CheckResult result = check(R"(
        --!strict
        function Funky()
            local a: number = foo
        end

        local foo: string = 'hello'
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto e = result.errors.front();
    REQUIRE_MESSAGE(get<UnknownSymbol>(e) != nullptr, "Expected UnknownSymbol, but got " << e);
}

TEST_CASE_FIXTURE(Fixture, "stringify_optional_parameterized_alias")
{
    ScopedFastFlag sffs3{"LuauGenericFunctions", true};
    ScopedFastFlag sffs4{"LuauParseGenericFunctions", true};

    CheckResult result = check(R"(
        type Node<T> = { value: T, child: Node<T>? }

        local function visitor<T>(node: Node<T>?)
            local a: Node<T>

            if node then
                a = node.child -- Observe the output of the error message.
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto e = get<TypeMismatch>(result.errors[0]);
    CHECK_EQ("Node<T>?", toString(e->givenType));
    CHECK_EQ("Node<T>", toString(e->wantedType));
}

TEST_CASE_FIXTURE(Fixture, "operator_eq_verifies_types_do_intersect")
{
    CheckResult result = check(R"(
        type Array<T> = { [number]: T }
        type Fiber = { id: number }
        type null = {}

        local fiberStack: Array<Fiber | null> = {}
        local index = 0

        local function f(fiber: Fiber)
            local a = fiber ~= fiberStack[index]
            local b = fiberStack[index] ~= fiber
        end

        return f
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "general_require_call_expression")
{
    fileResolver.source["game/A"] = R"(
--!strict
return { def = 4 }
    )";

    fileResolver.source["game/B"] = R"(
--!strict
local tbl = { abc = require(game.A) }
local a : string = ""
a = tbl.abc.def
    )";

    CheckResult result = frontend.check("game/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type 'number' could not be converted into 'string'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "general_require_type_mismatch")
{
    fileResolver.source["game/A"] = R"(
return { def = 4 }
    )";

    fileResolver.source["game/B"] = R"(
local tbl: string = require(game.A)
    )";

    CheckResult result = frontend.check("game/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type '{| def: number |}' could not be converted into 'string'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "general_require_multi_assign")
{
    fileResolver.source["workspace/A"] = R"(
        export type myvec2 = {x: number, y: number}
        return {}
    )";

    fileResolver.source["workspace/B"] = R"(
        export type myvec3 = {x: number, y: number, z: number}
        return {}
    )";

    fileResolver.source["workspace/C"] = R"(
        local Foo, Bar = require(workspace.A), require(workspace.B)

        local a: Foo.myvec2
        local b: Bar.myvec3
    )";

    CheckResult result = frontend.check("workspace/C");
    LUAU_REQUIRE_NO_ERRORS(result);
    ModulePtr m = frontend.moduleResolver.modules["workspace/C"];

    REQUIRE(m != nullptr);

    std::optional<TypeId> aTypeId = lookupName(m->getModuleScope(), "a");
    REQUIRE(aTypeId);
    const Luau::TableTypeVar* aType = get<TableTypeVar>(follow(*aTypeId));
    REQUIRE(aType);
    REQUIRE(aType->props.size() == 2);

    std::optional<TypeId> bTypeId = lookupName(m->getModuleScope(), "b");
    REQUIRE(bTypeId);
    const Luau::TableTypeVar* bType = get<TableTypeVar>(follow(*bTypeId));
    REQUIRE(bType);
    REQUIRE(bType->props.size() == 3);
}

TEST_CASE_FIXTURE(Fixture, "type_alias_import_mutation")
{
    CheckResult result = check("type t10<x> = typeof(table)");
    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId ty = getGlobalBinding(frontend.typeChecker, "table");
    CHECK_EQ(toString(ty), "table");

    const TableTypeVar* ttv = get<TableTypeVar>(ty);
    REQUIRE(ttv);

    CHECK(ttv->instantiatedTypeParams.empty());
}

TEST_CASE_FIXTURE(Fixture, "type_alias_local_mutation")
{
    CheckResult result = check(R"(
type Cool = { a: number, b: string }
local c: Cool = { a = 1, b = "s" }
type NotCool<x> = Cool
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    std::optional<TypeId> ty = requireType("c");
    REQUIRE(ty);
    CHECK_EQ(toString(*ty), "Cool");

    const TableTypeVar* ttv = get<TableTypeVar>(*ty);
    REQUIRE(ttv);

    CHECK(ttv->instantiatedTypeParams.empty());
}

TEST_CASE_FIXTURE(Fixture, "type_alias_local_rename")
{
    CheckResult result = check(R"(
type Cool = { a: number, b: string }
type NotCool = Cool
local c: Cool = { a = 1, b = "s" }
local d: NotCool = { a = 1, b = "s" }
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    std::optional<TypeId> ty = requireType("c");
    REQUIRE(ty);
    CHECK_EQ(toString(*ty), "Cool");

    ty = requireType("d");
    REQUIRE(ty);
    CHECK_EQ(toString(*ty), "NotCool");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_local_synthetic_mutation")
{
    CheckResult result = check(R"(
local c = { a = 1, b = "s" }
type Cool = typeof(c)
)");
    LUAU_REQUIRE_NO_ERRORS(result);

    std::optional<TypeId> ty = requireType("c");
    REQUIRE(ty);

    const TableTypeVar* ttv = get<TableTypeVar>(*ty);
    REQUIRE(ttv);
    CHECK_EQ(ttv->name, "Cool");
}

TEST_CASE_FIXTURE(Fixture, "type_alias_of_an_imported_recursive_type")
{
    ScopedFastFlag luauFixTableTypeAliasClone{"LuauFixTableTypeAliasClone", true};

    fileResolver.source["game/A"] = R"(
export type X = { a: number, b: X? }
return {}
    )";

    CheckResult aResult = frontend.check("game/A");
    LUAU_REQUIRE_NO_ERRORS(aResult);

    CheckResult bResult = check(R"(
local Import = require(game.A)
type X = Import.X
    )");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    std::optional<TypeId> ty1 = lookupImportedType("Import", "X");
    REQUIRE(ty1);

    std::optional<TypeId> ty2 = lookupType("X");
    REQUIRE(ty2);

    CHECK_EQ(follow(*ty1), follow(*ty2));
}

TEST_CASE_FIXTURE(Fixture, "type_alias_of_an_imported_recursive_generic_type")
{
    ScopedFastFlag luauFixTableTypeAliasClone{"LuauFixTableTypeAliasClone", true};

    fileResolver.source["game/A"] = R"(
export type X<T, U> = { a: T, b: U, C: X<T, U>? }
return {}
    )";

    CheckResult aResult = frontend.check("game/A");
    LUAU_REQUIRE_NO_ERRORS(aResult);

    CheckResult bResult = check(R"(
local Import = require(game.A)
type X<T, U> = Import.X<T, U>
    )");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    std::optional<TypeId> ty1 = lookupImportedType("Import", "X");
    REQUIRE(ty1);

    std::optional<TypeId> ty2 = lookupType("X");
    REQUIRE(ty2);

    CHECK_EQ(toString(*ty1, {true}), toString(*ty2, {true}));

    bResult = check(R"(
local Import = require(game.A)
type X<T, U> = Import.X<U, T>
    )");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    ty1 = lookupImportedType("Import", "X");
    REQUIRE(ty1);

    ty2 = lookupType("X");
    REQUIRE(ty2);

    CHECK_EQ(toString(*ty1, {true}), "t1 where t1 = {| C: t1?, a: T, b: U |}");
    CHECK_EQ(toString(*ty2, {true}), "{| C: t1, a: U, b: T |} where t1 = {| C: t1, a: U, b: T |}?");
}

TEST_CASE_FIXTURE(Fixture, "nonstrict_self_mismatch_tail")
{
    CheckResult result = check(R"(
--!nonstrict
local f = {}
function f:foo(a: number, b: number) end

function bar(...)
    f.foo(f, 1, ...)
end

bar(2)
)");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "typeof_unresolved_function")
{
    CheckResult result = check(R"(
local function f(a: typeof(f)) end
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Unknown global 'f'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "instantiate_table_cloning")
{
    CheckResult result = check(R"(
--!nonstrict
local l0:any,l61:t0<t32> = _,math
while _ do
_()
end
function _():t0<t0>
end
type t0<t32> = any
)");

    std::optional<TypeId> ty = requireType("math");
    REQUIRE(ty);

    const TableTypeVar* ttv = get<TableTypeVar>(*ty);
    REQUIRE(ttv);
    CHECK(ttv->instantiatedTypeParams.empty());
}

TEST_CASE_FIXTURE(Fixture, "bound_free_table_export_is_ok")
{
    CheckResult result = check(R"(
local n = {}
function n:Clone() end

local m = {}

function m.a(x)
	x:Clone()
end

function m.b()
	m.a(n)
end

return m
)");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "no_persistent_typelevel_change")
{
    TypeId mathTy = requireType(typeChecker.globalScope, "math");
    REQUIRE(mathTy);
    TableTypeVar* ttv = getMutable<TableTypeVar>(mathTy);
    REQUIRE(ttv);
    const FunctionTypeVar* ftv = get<FunctionTypeVar>(ttv->props["frexp"].type);
    REQUIRE(ftv);
    auto original = ftv->level;

    CheckResult result = check("local a = math.frexp");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(ftv->level.level == original.level);
    CHECK(ftv->level.subLevel == original.subLevel);
}

TEST_CASE_FIXTURE(Fixture, "table_indexing_error_location")
{
    CheckResult result = check(R"(
local foo = {42}
local bar: number?
local baz = foo[bar]
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(result.errors[0].location, Location{Position{3, 16}, Position{3, 19}});
}

TEST_CASE_FIXTURE(Fixture, "table_simple_call")
{
    CheckResult result = check(R"(
local a = setmetatable({ x = 2 }, {
    __call = function(self)
        return (self.x :: number) * 2 -- should work without annotation in the future
    end
})
local b = a()
local c = a(2) -- too many arguments
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Argument count mismatch. Function expects 1 argument, but 2 are specified", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "module_export_free_type_leak")
{
    CheckResult result = check(R"(
function get()
    return function(obj) return true end
end

export type f = typeof(get())
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "module_export_wrapped_free_type_leak")
{
    CheckResult result = check(R"(
function get()
    return {a = 1, b = function(obj) return true end}
end

export type f = typeof(get())
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "custom_require_global")
{
    CheckResult result = check(R"(
--!nonstrict
require = function(a) end

local crash = require(game.A)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "operator_eq_operands_are_not_subtypes_of_each_other_but_has_overlap")
{
    ScopedFastFlag sff1{"LuauEqConstraint", true};

    CheckResult result = check(R"(
        local function f(a: string | number, b: boolean | number)
            return a == b
        end
    )");

    // This doesn't produce any errors but for the wrong reasons.
    // This unit test serves as a reminder to not try and unify the operands on `==`/`~=`.
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "access_index_metamethod_that_returns_variadic")
{
    CheckResult result = check(R"(
        type Foo = {x: string}
        local t = {}
        setmetatable(t, {
            __index = function(x: string): ...Foo
                return {x = x}
            end
        })

        local foo = t.bar
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    ToStringOptions o;
    o.exhaustive = true;
    CHECK_EQ("{| x: string |}", toString(requireType("foo"), o));
}

TEST_CASE_FIXTURE(Fixture, "detect_cyclic_typepacks")
{
    CheckResult result = check(R"(
        type ( ... ) ( ) ;
        ( ... ) ( - - ... ) ( - ... )
        type = ( ... ) ;
        ( ... ) (  ) ( ... ) ;
        ( ... ) ""
    )");

    CHECK_LE(0, result.errors.size());
}

TEST_CASE_FIXTURE(Fixture, "detect_cyclic_typepacks2")
{
    CheckResult result = check(R"(
        function _(l0:((typeof((pcall)))|((((t0)->())|(typeof(-67108864)))|(any)))|(any),...):(((typeof(0))|(any))|(any),typeof(-67108864),any)
            xpcall(_,_,_)
            _(_,_,_)
        end
    )");

    CHECK_LE(0, result.errors.size());
}

TEST_CASE_FIXTURE(Fixture, "no_stack_overflow_from_quantifying")
{
    CheckResult result = check(R"(
        function _(l0:t0): (any, ()->())
        end

        type t0 = t0 | {}
    )");

    CHECK_LE(0, result.errors.size());

    std::optional<TypeFun> t0 = getMainModule()->getModuleScope()->lookupType("t0");
    REQUIRE(t0);
    CHECK(get<ErrorTypeVar>(t0->type));

    auto it = std::find_if(result.errors.begin(), result.errors.end(), [](TypeError& err) {
        return get<OccursCheckFailed>(err);
    });
    CHECK(it != result.errors.end());
}

TEST_CASE_FIXTURE(Fixture, "no_stack_overflow_from_isoptional")
{
    CheckResult result = check(R"(
        function _(l0:t0): (any, ()->())
            return 0,_
        end

        type t0 = t0 | {}
        _(nil)
    )");

    CHECK_LE(0, result.errors.size());

    std::optional<TypeFun> t0 = getMainModule()->getModuleScope()->lookupType("t0");
    REQUIRE(t0);
    CHECK(get<ErrorTypeVar>(t0->type));

    auto it = std::find_if(result.errors.begin(), result.errors.end(), [](TypeError& err) {
        return get<OccursCheckFailed>(err);
    });
    CHECK(it != result.errors.end());
}

TEST_CASE_FIXTURE(Fixture, "no_stack_overflow_from_isoptional2")
{
    CheckResult result = check(R"(
        function _(l0:({})|(t0)):((((typeof((xpcall)))|(t96<t0>))|(t13))&(t96<t0>),()->typeof(...))
            return 0,_
        end

        type t0<t107> = ((typeof((_G)))|(({})|(t0)))|(t0)
        _(nil)

        local t: ({})|(t0)
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "no_infinite_loop_when_trying_to_unify_uh_this")
{
    CheckResult result = check(R"(
        function _(l22,l0):((((boolean)|(t0))|(t0))&(()->(()->(()->()->{},(t0<t22>)|(t0)),any)))
            return function():t0<t0>
            end
        end
        type t0<t0> = ((typeof(_))|(any))|(typeof(_))
        _()
    )");

    CHECK_LE(0, result.errors.size());
}

TEST_CASE_FIXTURE(Fixture, "no_stack_overflow_from_flattenintersection")
{
    CheckResult result = check(R"(
        local l0,l0
        repeat
        type t0 = ((any)|((any)&((any)|((any)&((any)|(any))))))&(t0)
        function _(l0):(t0)&(t0)
        while nil do
        end
        end
        until _(_)(_)._
    )");

    CHECK_LE(0, result.errors.size());
}

TEST_CASE_FIXTURE(Fixture, "no_heap_use_after_free_error")
{
    CheckResult result = check(R"(
        --!nonstrict
        _ += _:n0(xpcall,_)
        local l0
        do end
        while _ do
        function _:_()
        _ += _(_._(_:n0(xpcall,_)))
        end
        end
    )");

    CHECK_LE(0, result.errors.size());
}

TEST_CASE_FIXTURE(Fixture, "dont_invalidate_the_properties_iterator_of_free_table_when_rolled_back")
{
    ScopedFastFlag sff{"LuauLogTableTypeVarBoundTo", true};

    fileResolver.source["Module/Backend/Types"] = R"(
        export type Fiber = {
            return_: Fiber?
        }
        return {}
    )";

    fileResolver.source["Module/Backend"] = R"(
        local Types = require(script.Types)
        type Fiber = Types.Fiber
        type ReactRenderer = { findFiberByHostInstance: () -> Fiber? }

        local function attach(renderer): ()
            local function getPrimaryFiber(fiber)
                local alternate = fiber.alternate
                return fiber
            end

            local function getFiberIDForNative()
                local fiber = renderer.findFiberByHostInstance()
                fiber = fiber.return_
                return getPrimaryFiber(fiber)
            end
        end

        function culprit(renderer: ReactRenderer): ()
            attach(renderer)
        end

        return culprit
    )";

    CheckResult result = frontend.check("Module/Backend");
}

TEST_CASE_FIXTURE(Fixture, "recursive_types_restriction_ok")
{
    CheckResult result = check(R"(
        type Tree<T> = { data: T, children: {Tree<T>} }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "recursive_types_restriction_not_ok")
{
    ScopedFastFlag sff{"LuauRecursiveTypeParameterRestriction", true};

    CheckResult result = check(R"(
        -- this would be an infinite type if we allowed it
        type Tree<T> = { data: T, children: {Tree<{T}>} }
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "record_matching_overload")
{
    ScopedFastFlag sffs("LuauStoreMatchingOverloadFnType", true);

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
    REQUIRE(it != module->astOverloadResolvedTypes.end());
    CHECK_EQ(toString(it->second), "(number) -> number");
}

TEST_CASE_FIXTURE(Fixture, "infer_anonymous_function_arguments")
{
    ScopedFastFlag luauInferFunctionArgsFix("LuauInferFunctionArgsFix", true);

    // Simple direct arg to arg propagation
    CheckResult result = check(R"(
type Table = { x: number, y: number }
local function f(a: (Table) -> number) return a({x = 1, y = 2}) end
f(function(a) return a.x + a.y end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // An optional funciton is accepted, but since we already provide a function, nil can be ignored
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

    // Anonymous function has a varyadic pack
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
    CHECK_EQ("Type '(number, number, a) -> number' could not be converted into '(number, number) -> number'", toString(result.errors[0]));

    // Infer from varyadic packs into elements
    result = check(R"(
function f(a: (...number) -> number) return a(1, 2) end
f(function(a, b) return a + b end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // Infer from varyadic packs into varyadic packs
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

TEST_CASE_FIXTURE(Fixture, "infer_generic_function_function_argument")
{
    ScopedFastFlag luauGenericFunctions("LuauGenericFunctions", true);
    ScopedFastFlag luauParseGenericFunctions("LuauParseGenericFunctions", true);
    ScopedFastFlag luauRankNTypes("LuauRankNTypes", true);

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
    REQUIRE_EQ("{| c: number, s: number |}", toString(requireType("r")));
}

TEST_CASE_FIXTURE(Fixture, "infer_generic_function_function_argument_overloaded")
{
    ScopedFastFlag luauGenericFunctions("LuauGenericFunctions", true);
    ScopedFastFlag luauParseGenericFunctions("LuauParseGenericFunctions", true);
    ScopedFastFlag luauRankNTypes("LuauRankNTypes", true);

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

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_types_restriction_ok")
{
    CheckResult result = check(R"(
        type Tree<T> = { data: T, children: Forest<T> }
        type Forest<T> = {Tree<T>}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_types_restriction_not_ok_1")
{
    ScopedFastFlag sff{"LuauRecursiveTypeParameterRestriction", true};

    CheckResult result = check(R"(
        -- OK because forwarded types are used with their parameters.
        type Tree<T> = { data: T, children: Forest<T> }
        type Forest<T> = {Tree<{T}>}
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_types_restriction_not_ok_2")
{
    ScopedFastFlag sff{"LuauRecursiveTypeParameterRestriction", true};

    CheckResult result = check(R"(
        -- Not OK because forwarded types are used with different types than their parameters.
        type Forest<T> = {Tree<{T}>}
        type Tree<T> = { data: T, children: Forest<T> }
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_types_swapsies_ok")
{
    CheckResult result = check(R"(
        type Tree1<T,U> = { data: T, children: {Tree2<U,T>} }
        type Tree2<U,T> = { data: U, children: {Tree1<T,U>} }

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "mutually_recursive_types_swapsies_not_ok")
{
    ScopedFastFlag sff{"LuauRecursiveTypeParameterRestriction", true};

    CheckResult result = check(R"(
        type Tree1<T,U> = { data: T, children: {Tree2<U,T>} }
        type Tree2<T,U> = { data: U, children: {Tree1<T,U>} }
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "free_variables_from_typeof_in_aliases")
{
    CheckResult result = check(R"(
        function f(x) return x[1] end
        -- x has type X? for a free type variable X
        local x = f ({})
        type ContainsFree<a> = { this: a, that: typeof(x) }
        type ContainsContainsFree = { that: ContainsFree<number> }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "infer_generic_lib_function_function_argument")
{
    CheckResult result = check(R"(
local a = {{x=4}, {x=7}, {x=1}}
table.sort(a, function(x, y) return x.x < y.x end)
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

TEST_CASE_FIXTURE(Fixture, "do_not_infer_generic_functions")
{
    ScopedFastFlag luauGenericFunctions("LuauGenericFunctions", true);
    ScopedFastFlag luauParseGenericFunctions("LuauParseGenericFunctions", true);
    ScopedFastFlag luauRankNTypes("LuauRankNTypes", true);

    CheckResult result = check(R"(
local function sum<a>(x: a, y: a, f: (a, a) -> a) return f(x, y) end

local function sumrec(f: typeof(sum))
    return sum(2, 3, function(a, b) return a + b end)
end

local b = sumrec(sum) -- ok
local c = sumrec(function(x, y, f) return f(x, y) end) -- type binders are not inferred
    )");

    LUAU_REQUIRE_ERRORS(result);
    CHECK_EQ("Type '(a, b, (a, b) -> (c...)) -> (c...)' could not be converted into '<a>(a, a, (a, a) -> a) -> a'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "infer_return_value_type")
{
    ScopedFastFlag luauInferReturnAssertAssign("LuauInferReturnAssertAssign", true);

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

TEST_CASE_FIXTURE(Fixture, "infer_type_assertion_value_type")
{
    ScopedFastFlag luauInferReturnAssertAssign("LuauInferReturnAssertAssign", true);

    CheckResult result = check(R"(
local function f()
    return {4, "b", 3} :: {string|number}
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "infer_assignment_value_types")
{
    ScopedFastFlag luauInferReturnAssertAssign("LuauInferReturnAssertAssign", true);

    CheckResult result = check(R"(
local a: (number, number) -> number = function(a, b) return a - b end

a = function(a, b) return a + b end

local b: {number|string}
local c: {number|string}
b, c = {2, "s"}, {"b", 4}
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "infer_assignment_value_types_mutable_lval")
{
    ScopedFastFlag luauInferReturnAssertAssign("LuauInferReturnAssertAssign", true);

    CheckResult result = check(R"(
local a = {}
a.x = 2
a = setmetatable(a, { __call = function(x) end })
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "refine_and_or")
{
    ScopedFastFlag sff{"LuauSlightlyMoreFlexibleBinaryPredicates", true};

    CheckResult result = check(R"(
        local t: {x: number?}? = {x = nil}
        local u = t and t.x or 5
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("number", toString(requireType("u")));
}

TEST_CASE_FIXTURE(Fixture, "checked_prop_too_early")
{
    ScopedFastFlag sffs[] = {
        {"LuauSlightlyMoreFlexibleBinaryPredicates", true},
        {"LuauExtraNilRecovery", true},
    };

    CheckResult result = check(R"(
        local t: {x: number?}? = {x = nil}
        local u = t.x and t or 5
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Value of type '{| x: number? |}?' could be nil", toString(result.errors[0]));
    CHECK_EQ("number | {| x: number? |}", toString(requireType("u")));
}

TEST_CASE_FIXTURE(Fixture, "accidentally_checked_prop_in_opposite_branch")
{
    ScopedFastFlag sffs[] = {
        {"LuauSlightlyMoreFlexibleBinaryPredicates", true},
        {"LuauExtraNilRecovery", true},
    };

    CheckResult result = check(R"(
        local t: {x: number?}? = {x = nil}
        local u = t and t.x == 5 or t.x == 31337
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Value of type '{| x: number? |}?' could be nil", toString(result.errors[0]));
    CHECK_EQ("boolean", toString(requireType("u")));
}

TEST_CASE_FIXTURE(Fixture, "substitution_with_bound_table")
{
    ScopedFastFlag luauFollowInTypeFunApply("LuauFollowInTypeFunApply", true);

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

TEST_CASE_FIXTURE(Fixture, "tc_if_else_expressions1")
{
    ScopedFastFlag sff1{"LuauIfElseExpressionBaseSupport", true};
    ScopedFastFlag sff2{"LuauIfElseExpressionAnalysisSupport", true};

    {
        CheckResult result = check(R"(local a = if true then "true" else "false")");
        LUAU_REQUIRE_NO_ERRORS(result);
        TypeId aType = requireType("a");
        CHECK_EQ(getPrimitiveType(aType), PrimitiveTypeVar::String);
    }
}

TEST_CASE_FIXTURE(Fixture, "tc_if_else_expressions2")
{
    ScopedFastFlag sff1{"LuauIfElseExpressionBaseSupport", true};
    ScopedFastFlag sff2{"LuauIfElseExpressionAnalysisSupport", true};

    {
        // Test expression containing elseif
        CheckResult result = check(R"(
local a = if false then "a" elseif false then "b" else "c"
        )");
        LUAU_REQUIRE_NO_ERRORS(result);
        TypeId aType = requireType("a");
        CHECK_EQ(getPrimitiveType(aType), PrimitiveTypeVar::String);
    }
}

TEST_CASE_FIXTURE(Fixture, "tc_if_else_expressions3")
{
    ScopedFastFlag sff1{"LuauIfElseExpressionBaseSupport", true};
    ScopedFastFlag sff2{"LuauIfElseExpressionAnalysisSupport", true};

    {
        CheckResult result = check(R"(local a = if true then "true" else 42)");
        // We currently require both true/false expressions to unify to the same type.  However, we do intend to lift
        // this restriction in the future.
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        TypeId aType = requireType("a");
        CHECK_EQ(getPrimitiveType(aType), PrimitiveTypeVar::String);
    }
}

TEST_SUITE_END();
