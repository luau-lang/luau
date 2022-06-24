// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"
#include "Luau/Scope.h"

#include <algorithm>

#include "Fixture.h"

#include "doctest.h"

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
        local id2
        local function id1<a>(x:a):a
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
    const FunctionTypeVar* idFun = get<FunctionTypeVar>(idType);
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
    const FunctionTypeVar* idFun = get<FunctionTypeVar>(idType);
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

TEST_CASE_FIXTURE(Fixture, "infer_generic_methods")
{
    CheckResult result = check(R"(
        local x = {}
        function x:id(x) return x end
        function x:f(): string return self:id("hello") end
        function x:g(): number return self:id(37) end
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
    // TODO: Should typecheck but currently errors CLI-39916
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
    LUAU_REQUIRE_ERRORS(result);
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
    LUAU_REQUIRE_ERRORS(result);
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

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    SwappedGenericTypeParameter* fErr = get<SwappedGenericTypeParameter>(result.errors[0]);
    REQUIRE(fErr);
    CHECK_EQ(fErr->name, "T");
    CHECK_EQ(fErr->kind, SwappedGenericTypeParameter::Pack);

    SwappedGenericTypeParameter* gErr = get<SwappedGenericTypeParameter>(result.errors[1]);
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

TEST_CASE_FIXTURE(Fixture, "instantiated_function_argument_names")
{
    CheckResult result = check(R"(
local function f<T, U...>(a: T, ...: U...) end

f(1, 2, 3)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    auto ty = findTypeAtPosition(Position(3, 0));
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

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(toString(result.errors[0]), R"(Type '() -> ()' could not be converted into '<T>() -> ()'; different number of generic type parameters)");
}

TEST_CASE_FIXTURE(Fixture, "error_detailed_function_mismatch_generic_pack")
{
    CheckResult result = check(R"(
type C = () -> ()
type D = <T...>() -> ()

local c: C
local d: D = c
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(toString(result.errors[0]),
        R"(Type '() -> ()' could not be converted into '<T...>() -> ()'; different number of generic type pack parameters)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generic_functions_dont_cache_type_parameters")
{
    CheckResult result = check(R"(
-- See https://github.com/Roblox/luau/issues/332
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

    LUAU_REQUIRE_ERRORS(result);
    CHECK_EQ(toString(result.errors[0]),
        R"(Type 'y' could not be converted into 'T<string>'
caused by:
  Property 'a' is not compatible. Type '{ c: T<string>?, d: number }' could not be converted into 'U<string>'
caused by:
  Property 'd' is not compatible. Type 'number' could not be converted into 'string')");
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
    CheckResult result = check(R"(
function test(a: number)
    return 1
end

function wrapper<A...>(f: (A...) -> number, ...: A...)
end

wrapper(test)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), R"(Argument count mismatch. Function expects 2 arguments, but only 1 is specified)");
}

TEST_CASE_FIXTURE(Fixture, "generic_argument_count_too_many")
{
    CheckResult result = check(R"(
function test2(a: number, b: string)
    return 1
end

function wrapper<A...>(f: (A...) -> number, ...: A...)
end

wrapper(test2, 1, "", 3)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ(toString(result.errors[0]), R"(Argument count mismatch. Function expects 3 arguments, but 4 are specified)");
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
    CHECK_EQ(*typeChecker.numberType, *requireType("a"));
    CHECK_EQ(*typeChecker.nilType, *requireType("b"));
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

    std::optional<TypeId> ret_ = first(foo->retTypes);
    REQUIRE(bool(ret_));
    TypeId ret = follow(*ret_);

    REQUIRE_EQ(getPrimitiveType(ret), PrimitiveTypeVar::Number);
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

TEST_CASE_FIXTURE(Fixture, "self_recursive_instantiated_param")
{
    // Mutability in type function application right now can create strange recursive types
    CheckResult result = check(R"(
type Table = { a: number }
type Self<T> = T
local a: Self<Table>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
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

    std::optional<TypeFun> t0 = getMainModule()->getModuleScope()->lookupType("t0");
    REQUIRE(t0);
    CHECK_EQ("*unknown*", toString(t0->type));

    auto it = std::find_if(result.errors.begin(), result.errors.end(), [](TypeError& err) {
        return get<OccursCheckFailed>(err);
    });
    CHECK(it != result.errors.end());
}

TEST_CASE_FIXTURE(BuiltinsFixture, "infer_generic_function_function_argument")
{
    CheckResult result = check(R"(
        local function sum<a>(x: a, y: a, f: (a, a) -> a)
            return f(x, y)
        end
        return sum(2, 3, function(a, b) return a + b end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    result = check(R"(
        local function map<a, b>(arr: {a}, f: (a) -> b)
            local r = {}
            for i,v in ipairs(arr) do
                table.insert(r, f(v))
            end
            return r
        end
        local a = {1, 2, 3}
        local r = map(a, function(a) return a + a > 100 end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    REQUIRE_EQ("{boolean}", toString(requireType("r")));

    check(R"(
        local function foldl<a, b>(arr: {a}, init: b, f: (b, a) -> b)
            local r = init
            for i,v in ipairs(arr) do
                r = f(r, v)
            end
            return r
        end
        local a = {1, 2, 3}
        local r = foldl(a, {s=0,c=0}, function(a, b) return {s = a.s + b, c = a.c + 1} end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    REQUIRE_EQ("{ c: number, s: number }", toString(requireType("r")));
}

TEST_CASE_FIXTURE(Fixture, "infer_generic_function_function_argument_overloaded")
{
    CheckResult result = check(R"(
        local g12: (<T>(T, (T) -> T) -> T) & (<T>(T, T, (T, T) -> T) -> T)

        g12(1, function(x) return x + x end)
        g12(1, 2, function(x, y) return x + y end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    result = check(R"(
        local g12: (<T>(T, (T) -> T) -> T) & (<T>(T, T, (T, T) -> T) -> T)

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

TEST_CASE_FIXTURE(Fixture, "do_not_infer_generic_functions")
{
    CheckResult result = check(R"(
local function sum<a>(x: a, y: a, f: (a, a) -> a) return f(x, y) end

local function sumrec(f: typeof(sum))
    return sum(2, 3, function(a, b) return a + b end)
end

local b = sumrec(sum) -- ok
local c = sumrec(function(x, y, f) return f(x, y) end) -- type binders are not inferred
    )");

    LUAU_REQUIRE_ERRORS(result);
    CHECK_EQ("Type '(a, b, (a, b) -> (c...)) -> (c...)' could not be converted into '<a>(a, a, (a, a) -> a) -> a'; different number of generic type "
             "parameters",
        toString(result.errors[0]));
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
    // https://github.com/Roblox/luau/issues/484
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
    // https://github.com/Roblox/luau/issues/484
    CheckResult result = check(R"(
--!strict
type MyObject = {
	getReturnValue: <V>(cb: () -> V) -> V
}
type ComplexObject<T> = {
	id: T,
	nested: MyObject
}

local complex2: ComplexObject<string> = nil

local x = complex2.nested.getReturnValue(function(): string
	return ""
end)

local y = complex2.nested.getReturnValue(function()
	return 3
end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "quantify_functions_even_if_they_have_an_explicit_generic")
{
    ScopedFastFlag sff[] = {
        {"LuauAlwaysQuantify", true},
    };

    CheckResult result = check(R"(
        function foo<X>(f, x: X)
            return f(x)
        end
    )");

    CHECK("<X, a...>((X) -> (a...), X) -> (a...)" == toString(requireType("foo")));
}

TEST_SUITE_END();
