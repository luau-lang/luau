// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Parser.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("GenericsTests");

TEST_CASE_FIXTURE(Fixture, "check_generic_function")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs4{"LuauGenericVariadicsUnification", true};
    ScopedFastFlag sffs5{"LuauParseGenericFunctions", true};

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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
    CheckResult result = check(R"(
        function f<a,b...>() end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "local_vars_can_be_polytypes")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
    CheckResult result = check(R"(
        local function id<a>(x:a):a return x end
        local f: <a>(a)->a = id
        local x: string = f("hi")
        local y: number = f(37)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "inferred_local_vars_can_be_polytypes")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    CheckResult result = check(R"(
        local function id(x) return x end
        print("This is bogus") -- TODO: CLI-39916
        local f = id
        local x: string = f("hi")
        local y: number = f(37)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "local_vars_can_be_instantiated_polytypes")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
    CheckResult result = check(R"(
        local t: { m: (number)->number } = { m = function(x:number) return x+1 end }
        local function id<a>(x:a):a return x end
        t.m = id
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "check_nested_generic_function")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
    ScopedFastFlag sffs3{"LuauRankNTypes", true};

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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
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
    auto [rets, varrets] = flatten(idFun->retType);

    CHECK_EQ(idFun->generics.size(), 1);
    CHECK_EQ(idFun->genericPacks.size(), 0);
    CHECK_EQ(args[0], idFun->generics[0]);
    CHECK_EQ(rets[0], idFun->generics[0]);
}

TEST_CASE_FIXTURE(Fixture, "infer_generic_local_function")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
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
    auto [rets, varrets] = flatten(idFun->retType);

    CHECK_EQ(idFun->generics.size(), 1);
    CHECK_EQ(idFun->genericPacks.size(), 0);
    CHECK_EQ(args[0], idFun->generics[0]);
    CHECK_EQ(rets[0], idFun->generics[0]);
}

TEST_CASE_FIXTURE(Fixture, "infer_nested_generic_function")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    CheckResult result = check(R"(
        local x = {}
        function x:id(x) return x end
        function x:f()
            local x: string = self:id("hi")
            local y: number = self:id(37)
        end
    )");
    // TODO: Should typecheck but currently errors CLI-39916
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "infer_generic_property")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauRankNTypes", true};
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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
    ScopedFastFlag sffs3{"LuauRankNTypes", true};
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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
    ScopedFastFlag sffs3{"LuauRankNTypes", true};
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
    ScopedFastFlag sffs1{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
    ScopedFastFlag sffs3{"LuauRankNTypes", true};
    CheckResult result = check(R"(
        local function id<a>(x:a):a return x end
        local f: <a>(a)->a = id(id)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "dont_leak_generic_types")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
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
    LUAU_REQUIRE_ERROR_COUNT(2, result);
}

TEST_CASE_FIXTURE(Fixture, "dont_leak_inferred_generic_types")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
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
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "dont_substitute_bound_types")
{
    ScopedFastFlag sffs[] = {
        {"LuauGenericFunctions", true},
        {"LuauParseGenericFunctions", true},
        {"LuauRankNTypes", true},
    };

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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
    ScopedFastFlag sffs3{"LuauRankNTypes", true};

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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
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
    ScopedFastFlag sffs{"LuauGenericFunctions", false};
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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs2{"LuauParseGenericFunctions", true};
    CheckResult result = check(R"(
        function f<a,a>(x:a):a return x end
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "duplicate_generic_type_packs")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    CheckResult result = check(R"(
        function f<a...,a...>() end
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "typepacks_before_types")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    CheckResult result = check(R"(
        function f<a...,b>() end
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "variadic_generics")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs3{"LuauParseGenericFunctions", true};

    CheckResult result = check(R"(
        function f<a>(...: a) end

        type F<a> = (...a) -> ...a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "generic_type_pack_syntax")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs4{"LuauParseGenericFunctions", true};

    CheckResult result = check(R"(
        function f<a...>(...: a...): (a...) return ... end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(toString(requireType("f")), "<a...>(a...) -> (a...)");
}

TEST_CASE_FIXTURE(Fixture, "generic_type_pack_parentheses")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs4{"LuauGenericVariadicsUnification", true};
    ScopedFastFlag sffs5{"LuauParseGenericFunctions", true};

    CheckResult result = check(R"(
        function f<a...>(...: a...): any return (...) end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(Fixture, "better_mismatch_error_messages")
{
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs5{"LuauParseGenericFunctions", true};

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
    ScopedFastFlag sffs{"LuauGenericFunctions", true};
    ScopedFastFlag sffs3{"LuauParseGenericFunctions", true};

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
    ScopedFastFlag sffs1{"LuauGenericFunctions", true};

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
    ScopedFastFlag sffs1{"LuauGenericFunctions", true};

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
    ScopedFastFlag sffs1{"LuauGenericFunctions", true};

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

TEST_SUITE_END();
