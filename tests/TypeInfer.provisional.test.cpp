// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeInfer.h"
#include "Luau/RecursionCounter.h"

#include "Fixture.h"

#include "doctest.h"

#include <algorithm>

using namespace Luau;

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution);
LUAU_FASTFLAG(DebugLuauSharedSelf);
LUAU_FASTINT(LuauNormalizeCacheLimit);
LUAU_FASTINT(LuauTarjanChildLimit);
LUAU_FASTINT(LuauTypeInferIterationLimit);
LUAU_FASTINT(LuauTypeInferRecursionLimit);
LUAU_FASTINT(LuauTypeInferTypePackLoopLimit);

TEST_SUITE_BEGIN("ProvisionalTests");

// These tests check for behavior that differs from the final behavior we'd
// like to have.  They serve to document the current state of the typechecker.
// When making future improvements, its very likely these tests will break and
// will need to be replaced.

/*
 * This test falls into a sort of "do as I say" pit of consequences:
 * Technically, the type of the type() function is <T>(T) -> string
 *
 * We thus infer that the argument to f is a free type.
 * While we can still learn something about this argument, we can't seem to infer a union for it.
 *
 * Is this good?  Maybe not, but I'm not sure what else we should do.
 */
TEST_CASE_FIXTURE(Fixture, "typeguard_inference_incomplete")
{
    const std::string code = R"(
        function f(a)
            if type(a) == "boolean" then
                local a1 = a
            elseif a.fn() then
                local a2 = a
            end
        end
    )";

    const std::string expected = R"(
        function f(a:{fn:()->(a,b...)}): ()
            if type(a) == 'boolean'then
                local a1:boolean=a
            elseif a.fn()then
                local a2:{fn:()->(a,b...)}=a
            end
        end
    )";

    CHECK_EQ(expected, decorateWithTypes(code));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "luau-polyfill.Array.filter")
{
    // This test exercises the fact that we should reduce sealed/unsealed/free tables
    // res is a unsealed table with type {((T & ~nil)?) & any}
    // Because we do not reduce it fully, we cannot unify it with `Array<T> = { [number] : T}
    // TLDR; reduction needs to reduce the indexer on res so it unifies with Array<T>
    CheckResult result = check(R"(
--!strict
-- Implements Javascript's `Array.prototype.filter` as defined below
-- https://developer.cmozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter
type Array<T> = { [number]: T }
type callbackFn<T> = (element: T, index: number, array: Array<T>) -> boolean
type callbackFnWithThisArg<T, U> = (thisArg: U, element: T, index: number, array: Array<T>) -> boolean
type Object = { [string]: any }
return function<T, U>(t: Array<T>, callback: callbackFn<T> | callbackFnWithThisArg<T, U>, thisArg: U?): Array<T>

	local len = #t
	local res = {}
	if thisArg == nil then
		for i = 1, len do
			local kValue = t[i]
			if kValue ~= nil then
				if (callback :: callbackFn<T>)(kValue, i, t) then
					res[i] = kValue
				end
			end
		end
	else
	end

	return res
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "xpcall_returns_what_f_returns")
{
    const std::string code = R"(
        local a, b, c = xpcall(function() return 1, "foo" end, function() return "foo", 1 end)
    )";

    const std::string expected = R"(
        local a:boolean,b:number,c:string=xpcall(function(): (number,string)return 1,'foo'end,function(): (string,number)return'foo',1 end)
    )";

    CheckResult result = check(code);

    CHECK("boolean" == toString(requireType("a")));
    CHECK("number" == toString(requireType("b")));
    CHECK("string" == toString(requireType("c")));

    CHECK(expected == decorateWithTypes(code));

    LUAU_REQUIRE_NO_ERRORS(result);
}

// We had a bug where if you have two type packs that looks like:
//   { x, y }, ...
//   { x }, ...
// It would infinitely grow the type pack because one WeirdIter is trying to catch up, but can't.
// However, the following snippet is supposed to generate an OccursCheckFailed, but it doesn't.
TEST_CASE_FIXTURE(Fixture, "weirditer_should_not_loop_forever")
{
    // this flag is intentionally here doing nothing to demonstrate that we exit early via case detection
    ScopedFastInt sfis{FInt::LuauTypeInferTypePackLoopLimit, 50};

    CheckResult result = check(R"(
        local function toVertexList(vertices, x, y, ...)
            if not (x and y) then return vertices end  -- no more arguments
            vertices[#vertices + 1] = {x = x, y = y}   -- set vertex
            return toVertexList(vertices, ...)         -- recurse
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

// This should also generate an OccursCheckFailed error too, like the above toVertexList snippet.
// at least up until we can get Luau to recognize this code as a valid function that iterates over a list of values in the pack.
TEST_CASE_FIXTURE(Fixture, "it_should_be_agnostic_of_actual_size")
{
    CheckResult result = check(R"(
        local function f(x, y, ...)
            if not y then return x end
            return f(x, ...)
        end

        f(3, 2, 1, 0)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

// Ideally setmetatable's second argument would be an optional free table.
// For now, infer it as just a free table.
TEST_CASE_FIXTURE(BuiltinsFixture, "setmetatable_constrains_free_type_into_free_table")
{
    CheckResult result = check(R"(
        local a = {}
        local b
        setmetatable(a, b)
        b = 1
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("{-  -}", toString(tm->wantedType));
    CHECK_EQ("number", toString(tm->givenType));
}

// Luau currently doesn't yet know how to allow assignments when the binding was refined.
TEST_CASE_FIXTURE(Fixture, "while_body_are_also_refined")
{
    CheckResult result = check(R"(
        type Node<T> = { value: T, child: Node<T>? }

        local function visitor<T>(node: Node<T>, f: (T) -> ())
            local current = node

            while current do
                f(current.value)
                current = current.child -- TODO: Can't work just yet. It thinks 'current' can never be nil. :(
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ("Type 'Node<T>?' could not be converted into 'Node<T>'", toString(result.errors[0]));
}

// Originally from TypeInfer.test.cpp.
// I dont think type checking the metamethod at every site of == is the correct thing to do.
// We should be type checking the metamethod at the call site of setmetatable.
TEST_CASE_FIXTURE(BuiltinsFixture, "error_on_eq_metamethod_returning_a_type_other_than_boolean")
{
    CheckResult result = check(R"(
        local tab = {a = 1}
        setmetatable(tab, {__eq = function(a, b): number
            return 1
        end})
        local tab2 = tab

        local a = tab2 == tab
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    GenericError* ge = get<GenericError>(result.errors[0]);
    REQUIRE(ge);
    CHECK_EQ("Metamethod '__eq' must return type 'boolean'", ge->message);
}

// Belongs in TypeInfer.refinements.test.cpp.
// We need refine both operands as `never` in the `==` branch.
TEST_CASE_FIXTURE(Fixture, "lvalue_equals_another_lvalue_with_no_overlap")
{
    CheckResult result = check(R"(
        local function f(a: string, b: boolean?)
            if a == b then
                local foo, bar = a, b
            else
                local foo, bar = a, b
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(toString(requireTypeAtPosition({3, 33})), "string");   // a == b
    CHECK_EQ(toString(requireTypeAtPosition({3, 36})), "boolean?"); // a == b

    CHECK_EQ(toString(requireTypeAtPosition({5, 33})), "string");   // a ~= b
    CHECK_EQ(toString(requireTypeAtPosition({5, 36})), "boolean?"); // a ~= b
}

// Also belongs in TypeInfer.refinements.test.cpp.
// Just needs to fully support equality refinement. Which is annoying without type states.
TEST_CASE_FIXTURE(Fixture, "discriminate_from_x_not_equal_to_nil")
{
    CheckResult result = check(R"(
        type T = {x: string, y: number} | {x: nil, y: nil}

        local function f(t: T)
            if t.x ~= nil then
                local foo = t
            else
                local bar = t
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {

        CHECK_EQ("{ x: string, y: number }", toString(requireTypeAtPosition({5, 28})));

        // Should be { x: nil, y: nil }
        CHECK_EQ("{ x: nil, y: nil } | { x: string, y: number }", toString(requireTypeAtPosition({7, 28})));
    }
    else
    {
        CHECK_EQ("{| x: string, y: number |}", toString(requireTypeAtPosition({5, 28})));

        // Should be {| x: nil, y: nil |}
        CHECK_EQ("{| x: nil, y: nil |} | {| x: string, y: number |}", toString(requireTypeAtPosition({7, 28})));
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "bail_early_if_unification_is_too_complicated" * doctest::timeout(0.5))
{
    ScopedFastInt sffi{FInt::LuauTarjanChildLimit, 1};
    ScopedFastInt sffi2{FInt::LuauTypeInferIterationLimit, 1};

    CheckResult result = check(R"LUA(
        local Result
        Result = setmetatable({}, {})
        Result.__index = Result
        function Result.new(okValue)
            local self = setmetatable({}, Result)
            self:constructor(okValue)
            return self
        end
        function Result:constructor(okValue)
            self.okValue = okValue
        end
        function Result:ok(val) return Result.new(val) end
        function Result:a(p0, p1, p2, p3, p4) return Result.new((self.okValue)) or p0 or p1 or p2 or p3 or p4 end
        function Result:b(p0, p1, p2, p3, p4) return Result:ok((self.okValue)) or p0 or p1 or p2 or p3 or p4 end
        function Result:c(p0, p1, p2, p3, p4) return Result:ok((self.okValue)) or p0 or p1 or p2 or p3 or p4 end
        function Result:transpose(a)
            return a and self.okValue:z(function(some)
                return Result:ok(some)
            end) or Result:ok(self.okValue)
        end
    )LUA");

    auto it = std::find_if(result.errors.begin(), result.errors.end(), [](TypeError& a) {
        return nullptr != get<UnificationTooComplex>(a);
    });
    if (it == result.errors.end())
    {
        dumpErrors(result);
        FAIL("Expected a UnificationTooComplex error");
    }
}

TEST_CASE_FIXTURE(Fixture, "do_not_ice_when_trying_to_pick_first_of_generic_type_pack")
{
    // In-place quantification causes these types to have the wrong types but only because of nasty interaction with prototyping.
    // The type of f is initially () -> free1...
    // Then the prototype iterator advances, and checks the function expression assigned to g, which has the type () -> free2...
    // In the body it calls f and returns what f() returns. This binds free2... with free1..., causing f and g to have same types.
    // We then quantify g, leaving it with the final type <a...>() -> a...
    // Because free1... and free2... were bound, in combination with in-place quantification, f's return type was also turned into a...
    // Then the check iterator catches up, and checks the body of f, and attempts to quantify it too.
    // Alas, one of the requirements for quantification is that a type must contain free types. () -> a... has no free types.
    // Thus the quantification for f was no-op, which explains why f does not have any type parameters.
    // Calling f() will attempt to instantiate the function type, which turns generics in type binders into to free types.
    // However, instantiations only converts generics contained within the type binders of a function, so instantiation was also no-op.
    // Which means that calling f() simply returned a... rather than an instantiation of it. And since the call site was not in tail position,
    // picking first element in a... triggers an ICE because calls returning generic packs are unexpected.
    CheckResult result = check(R"(
        local function f() end

        local g = function() return f() end

        local x = (f()) -- should error: no return values to assign from the call to f
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // f and g should have the type () -> ()
    CHECK_EQ("() -> (a...)", toString(requireType("f")));
    CHECK_EQ("<a...>() -> (a...)", toString(requireType("g")));
    CHECK_EQ("any", toString(requireType("x"))); // any is returned instead of ICE for now
}

TEST_CASE_FIXTURE(Fixture, "specialization_binds_with_prototypes_too_early")
{
    CheckResult result = check(R"(
        local function id(x) return x end
        local n2n: (number) -> number = id
        local s2s: (string) -> string = id
    )");

    LUAU_REQUIRE_ERRORS(result); // Should not have any errors.
}

TEST_CASE_FIXTURE(Fixture, "weird_fail_to_unify_type_pack")
{
    ScopedFastFlag sff[] = {
        // I'm not sure why this is broken without DCR, but it seems to be fixed
        // when DCR is enabled.
        {FFlag::DebugLuauDeferredConstraintResolution, false},
    };

    CheckResult result = check(R"(
        local function f() return end
        local g = function() return f() end
    )");

    LUAU_REQUIRE_ERRORS(result); // Should not have any errors.
}

// Belongs in TypeInfer.builtins.test.cpp.
TEST_CASE_FIXTURE(BuiltinsFixture, "pcall_returns_at_least_two_value_but_function_returns_nothing")
{
    CheckResult result = check(R"(
        local function f(): () end
        local ok, res = pcall(f)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Function only returns 1 value, but 2 are required here", toString(result.errors[0]));
    // LUAU_REQUIRE_NO_ERRORS(result);
    // CHECK_EQ("boolean", toString(requireType("ok")));
    // CHECK_EQ("any", toString(requireType("res")));
}

// Belongs in TypeInfer.builtins.test.cpp.
TEST_CASE_FIXTURE(BuiltinsFixture, "choose_the_right_overload_for_pcall")
{
    CheckResult result = check(R"(
        local function f(): number
            if math.random() > 0.5 then
                return 5
            else
                error("something")
            end
        end

        local ok, res = pcall(f)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("boolean", toString(requireType("ok")));
    CHECK_EQ("number", toString(requireType("res")));
    // CHECK_EQ("any", toString(requireType("res")));
}

// Belongs in TypeInfer.builtins.test.cpp.
TEST_CASE_FIXTURE(BuiltinsFixture, "function_returns_many_things_but_first_of_it_is_forgotten")
{
    CheckResult result = check(R"(
        local function f(): (number, string, boolean)
            if math.random() > 0.5 then
                return 5, "hello", true
            else
                error("something")
            end
        end

        local ok, res, s, b = pcall(f)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("boolean", toString(requireType("ok")));
    CHECK_EQ("number", toString(requireType("res")));
    // CHECK_EQ("any", toString(requireType("res")));
    CHECK_EQ("string", toString(requireType("s")));
    CHECK_EQ("boolean", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "free_is_not_bound_to_any")
{
    CheckResult result = check(R"(
        local function foo(f: (any) -> (), x)
            f(x)
        end
    )");

    CHECK_EQ("((any) -> (), any) -> ()", toString(requireType("foo")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "greedy_inference_with_shared_self_triggers_function_with_no_returns")
{
    ScopedFastFlag sff{FFlag::DebugLuauSharedSelf, true};

    CheckResult result = check(R"(
        local T = {}
        T.__index = T

        function T.new()
            local self = setmetatable({}, T)
            return self:ctor() or self
        end

        function T:ctor()
            -- oops, no return!
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Not all codepaths in this function return 'self, a...'.", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "dcr_can_partially_dispatch_a_constraint")
{
    ScopedFastFlag sff[] = {
        {FFlag::DebugLuauDeferredConstraintResolution, true},
    };

    CheckResult result = check(R"(
        local function hasDivisors(value: number)
        end

        function prime_iter(state, index)
            hasDivisors(index)
            index += 1
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // Solving this requires recognizing that we can't dispatch a constraint
    // like this without doing further work:
    //
    //     (*blocked*) -> () <: (number) -> (b...)
    //
    // We solve this by searching both types for BlockedTypes and block the
    // constraint on any we find.  It also gets the job done, but I'm worried
    // about the efficiency of doing so many deep type traversals and it may
    // make us more prone to getting stuck on constraint cycles.
    //
    // If this doesn't pan out, a possible solution is to go further down the
    // path of supporting partial constraint dispatch.  The way it would work is
    // that we'd dispatch the above constraint by binding b... to (), but we
    // would append a new constraint number <: *blocked* to the constraint set
    // to be solved later.  This should be faster and theoretically less prone
    // to cyclic constraint dependencies.

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK("(unknown, number) -> ()" == toString(requireType("prime_iter")));
    else
        CHECK("<a>(a, number) -> ()" == toString(requireType("prime_iter")));
}

TEST_CASE_FIXTURE(Fixture, "free_options_cannot_be_unified_together")
{
    TypeArena arena;
    TypeId nilType = builtinTypes->nilType;

    std::unique_ptr scope = std::make_unique<Scope>(builtinTypes->anyTypePack);

    TypeId free1 = arena.addType(FreeType{scope.get()});
    TypeId option1 = arena.addType(UnionType{{nilType, free1}});

    TypeId free2 = arena.addType(FreeType{scope.get()});
    TypeId option2 = arena.addType(UnionType{{nilType, free2}});

    InternalErrorReporter iceHandler;
    UnifierSharedState sharedState{&iceHandler};
    Normalizer normalizer{&arena, builtinTypes, NotNull{&sharedState}};
    Unifier u{NotNull{&normalizer}, NotNull{scope.get()}, Location{}, Variance::Covariant};

    if (FFlag::DebugLuauDeferredConstraintResolution)
        u.enableNewSolver();

    u.tryUnify(option1, option2);

    CHECK(!u.failure);

    u.log.commit();

    ToStringOptions opts;
    CHECK("a?" == toString(option1, opts));

    // CHECK("a?" == toString(option2, opts)); // This should hold, but does not.
    CHECK("b?" == toString(option2, opts)); // This should not hold.
}

TEST_CASE_FIXTURE(BuiltinsFixture, "for_in_loop_with_zero_iterators")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, false};

    CheckResult result = check(R"(
        function no_iter() end
        for key in no_iter() do end -- This should not be ok
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

// Ideally, we would not try to export a function type with generic types from incorrect scope
TEST_CASE_FIXTURE(BuiltinsFixture, "generic_type_leak_to_module_interface")
{
    fileResolver.source["game/A"] = R"(
local wrapStrictTable

local metatable = {
    __index = function(self, key)
        local value = self.__tbl[key]
        if type(value) == "table" then
            -- unification of the free 'wrapStrictTable' with this function type causes generics of this function to leak out of scope
            return wrapStrictTable(value, self.__name .. "." .. key)
        end
        return value
    end,
}

return wrapStrictTable
    )";

    frontend.check("game/A");

    fileResolver.source["game/B"] = R"(
local wrapStrictTable = require(game.A)

local Constants = {}

return wrapStrictTable(Constants, "Constants")
    )";

    frontend.check("game/B");

    ModulePtr m = frontend.moduleResolver.getModule("game/B");
    REQUIRE(m);

    std::optional<TypeId> result = first(m->returnType);
    REQUIRE(result);
    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("(any & ~(*error-type* | table))?", toString(*result));
    else
        CHECK_MESSAGE(get<AnyType>(*result), *result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generic_type_leak_to_module_interface_variadic")
{
    fileResolver.source["game/A"] = R"(
local wrapStrictTable

local metatable = {
    __index = function<T>(self, key, ...: T)
        local value = self.__tbl[key]
        if type(value) == "table" then
            -- unification of the free 'wrapStrictTable' with this function type causes generics of this function to leak out of scope
            return wrapStrictTable(value, self.__name .. "." .. key)
        end
        return ...
    end,
}

return wrapStrictTable
    )";

    frontend.check("game/A");

    fileResolver.source["game/B"] = R"(
local wrapStrictTable = require(game.A)

local Constants = {}

return wrapStrictTable(Constants, "Constants")
    )";

    frontend.check("game/B");

    ModulePtr m = frontend.moduleResolver.getModule("game/B");
    REQUIRE(m);

    std::optional<TypeId> result = first(m->returnType);
    REQUIRE(result);
    CHECK(get<AnyType>(*result));
}

namespace
{
struct IsSubtypeFixture : Fixture
{
    bool isSubtype(TypeId a, TypeId b)
    {
        ModulePtr module = getMainModule();
        REQUIRE(module);

        if (!module->hasModuleScope())
            FAIL("isSubtype: module scope data is not available");

        return ::Luau::isSubtype(a, b, NotNull{module->getModuleScope().get()}, builtinTypes, ice);
    }
};
} // namespace

TEST_CASE_FIXTURE(IsSubtypeFixture, "intersection_of_functions_of_different_arities")
{
    check(R"(
        type A = (any) -> ()
        type B = (any, any) -> ()
        type T = A & B

        local a: A
        local b: B
        local t: T
    )");

    [[maybe_unused]] TypeId a = requireType("a");
    [[maybe_unused]] TypeId b = requireType("b");

    // CHECK(!isSubtype(a, b)); // !!
    // CHECK(!isSubtype(b, a));

    CHECK("((any) -> ()) & ((any, any) -> ())" == toString(requireType("t")));
}

TEST_CASE_FIXTURE(IsSubtypeFixture, "functions_with_mismatching_arity")
{
    check(R"(
        local a: (number) -> ()
        local b: () -> ()

        local c: () -> number
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");
    TypeId c = requireType("c");

    // CHECK(!isSubtype(b, a));
    // CHECK(!isSubtype(c, a));

    CHECK(!isSubtype(a, b));
    // CHECK(!isSubtype(c, b));

    CHECK(!isSubtype(a, c));
    CHECK(!isSubtype(b, c));
}

TEST_CASE_FIXTURE(IsSubtypeFixture, "functions_with_mismatching_arity_but_optional_parameters")
{
    /*
     * (T0..TN) <: (T0..TN, A?)
     * (T0..TN) <: (T0..TN, any)
     * (T0..TN, A?) </: (T0..TN)            We don't technically need to spell this out, but it's quite important.
     * T <: T
     * if A <: B and B <: C then A <: C
     * T -> R <: U -> S if U <: T and R <: S
     * A | B <: T if A <: T and B <: T
     * T <: A | B if T <: A or T <: B
     */
    check(R"(
        local a: (number?) -> ()
        local b: (number) -> ()
        local c: (number, number?) -> ()
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");
    TypeId c = requireType("c");

    /*
     * (number) -> () </: (number?) -> ()
     *      because number? </: number (because number <: number, but nil </: number)
     */
    CHECK(!isSubtype(b, a));

    /*
     * (number, number?) </: (number?) -> ()
     *      because number? </: number (as above)
     */
    CHECK(!isSubtype(c, a));

    /*
     * (number?) -> () <: (number) -> ()
     *      because number <: number? (because number <: number)
     */
    CHECK(isSubtype(a, b));

    /*
     * (number, number?) -> () <: (number) -> (number)
     *      The packs have inequal lengths, but (number) <: (number, number?)
     *      and number <: number
     */
    // CHECK(!isSubtype(c, b));

    /*
     * (number?) -> () </: (number, number?) -> ()
     *      because (number, number?) </: (number)
     */
    // CHECK(!isSubtype(a, c));

    /*
     * (number) -> () </: (number, number?) -> ()
     *      because (number, number?) </: (number)
     */
    // CHECK(!isSubtype(b, c));
}

TEST_CASE_FIXTURE(IsSubtypeFixture, "functions_with_mismatching_arity_but_any_is_an_optional_param")
{
    check(R"(
        local a: (number?) -> ()
        local b: (number) -> ()
        local c: (number, any) -> ()
    )");

    TypeId a = requireType("a");
    TypeId b = requireType("b");
    TypeId c = requireType("c");

    /*
     * (number) -> () </: (number?) -> ()
     *      because number? </: number (because number <: number, but nil </: number)
     */
    CHECK(!isSubtype(b, a));

    /*
     * (number, any) </: (number?) -> ()
     *      because number? </: number (as above)
     */
    CHECK(!isSubtype(c, a));

    /*
     * (number?) -> () <: (number) -> ()
     *      because number <: number? (because number <: number)
     */
    CHECK(isSubtype(a, b));

    /*
     * (number, any) -> () </: (number) -> (number)
     *      The packs have inequal lengths
     */
    // CHECK(!isSubtype(c, b));

    /*
     * (number?) -> () </: (number, any) -> ()
     *      The packs have inequal lengths
     */
    // CHECK(!isSubtype(a, c));

    /*
     * (number) -> () </: (number, any) -> ()
     *      The packs have inequal lengths
     */
    // CHECK(!isSubtype(b, c));
}

TEST_CASE_FIXTURE(Fixture, "assign_table_with_refined_property_with_a_similar_type_is_illegal")
{
    CheckResult result = check(R"(
        local t: {x: number?} = {x = nil}

        if t.x then
            local u: {x: number} = t
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const std::string expected = R"(Type
    '{| x: number? |}'
could not be converted into
    '{| x: number |}'
caused by:
  Property 'x' is not compatible.
Type 'number?' could not be converted into 'number' in an invariant context)";
    CHECK_EQ(expected, toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_insert_with_a_singleton_argument")
{
    CheckResult result = check(R"(
        local function foo(t, x)
            if x == "hi" or x == "bye" then
                table.insert(t, x)
            end

            return t
        end

        local t = foo({}, "hi")
        table.insert(t, "totally_unrelated_type" :: "totally_unrelated_type")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
        CHECK_EQ("{string}", toString(requireType("t")));
    else
    {
        // We'd really like for this to be {string}
        CHECK_EQ("{string | string}", toString(requireType("t")));
    }
}

// We really should be warning on this.  We have no guarantee that T has any properties.
TEST_CASE_FIXTURE(Fixture, "lookup_prop_of_intersection_containing_unions_of_tables_that_have_the_prop")
{
    CheckResult result = check(R"(
        local function mergeOptions<T>(options: T & ({variable: string} | {variable: number}))
            return options.variable
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // LUAU_REQUIRE_ERROR_COUNT(1, result);

    // const UnknownProperty* unknownProp = get<UnknownProperty>(result.errors[0]);
    // REQUIRE(unknownProp);

    // CHECK("variable" == unknownProp->key);
}

TEST_CASE_FIXTURE(Fixture, "expected_type_should_be_a_helpful_deduction_guide_for_function_calls")
{
    CheckResult result = check(R"(
        type Ref<T> = { val: T }

        local function useRef<T>(x: T): Ref<T?>
            return { val = x }
        end

        local x: Ref<number?> = useRef(nil)
    )");

    // This is actually wrong! Sort of. It's doing the wrong thing, it's actually asking whether
    //  `{| val: number? |} <: {| val: nil |}`
    // instead of the correct way, which is
    //  `{| val: nil |} <: {| val: number? |}`
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "floating_generics_should_not_be_allowed")
{
    CheckResult result = check(R"(
        local assign : <T, U, V, W>(target: T, source0: U?, source1: V?, source2: W?, ...any) -> T & U & V & W = (nil :: any)

        -- We have a big problem here: The generics U, V, and W are not bound to anything!
        -- Things get strange because of this.
        local benchmark = assign({})
        local options = benchmark.options
        do
            local resolve2: any = nil
            options.fn({
                resolve = function(...)
                    resolve2(...)
                end,
            })
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "free_options_can_be_unified_together")
{
    TypeArena arena;
    TypeId nilType = builtinTypes->nilType;

    std::unique_ptr scope = std::make_unique<Scope>(builtinTypes->anyTypePack);

    TypeId free1 = arena.addType(FreeType{scope.get()});
    TypeId option1 = arena.addType(UnionType{{nilType, free1}});

    TypeId free2 = arena.addType(FreeType{scope.get()});
    TypeId option2 = arena.addType(UnionType{{nilType, free2}});

    InternalErrorReporter iceHandler;
    UnifierSharedState sharedState{&iceHandler};
    Normalizer normalizer{&arena, builtinTypes, NotNull{&sharedState}};
    Unifier u{NotNull{&normalizer}, NotNull{scope.get()}, Location{}, Variance::Covariant};

    if (FFlag::DebugLuauDeferredConstraintResolution)
        u.enableNewSolver();

    u.tryUnify(option1, option2);

    CHECK(!u.failure);

    u.log.commit();

    ToStringOptions opts;
    CHECK("a?" == toString(option1, opts));
    CHECK("b?" == toString(option2, opts)); // should be `a?`.
}

TEST_CASE_FIXTURE(Fixture, "unify_more_complex_unions_that_include_nil")
{
    CheckResult result = check(R"(
        type Record = {prop: (string | boolean)?}

        function concatPagination(prop: (string | boolean | nil)?): Record
            return {prop = prop}
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "optional_class_instances_are_invariant")
{
    createSomeClasses(&frontend);

    CheckResult result = check(R"(
        function foo(ref: {current: Parent?})
        end

        function bar(ref: {current: Child?})
            foo(ref)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "luau-polyfill.Map.entries")
{

    fileResolver.source["Module/Map"] = R"(
--!strict

type Object = { [any]: any }
type Array<T> = { [number]: T }
type Table<T, V> = { [T]: V }
type Tuple<T, V> = Array<T | V>

local Map = {}

export type Map<K, V> = {
	size: number,
	-- method definitions
	set: (self: Map<K, V>, K, V) -> Map<K, V>,
	get: (self: Map<K, V>, K) -> V | nil,
	clear: (self: Map<K, V>) -> (),
	delete: (self: Map<K, V>, K) -> boolean,
	has: (self: Map<K, V>, K) -> boolean,
	keys: (self: Map<K, V>) -> Array<K>,
	values: (self: Map<K, V>) -> Array<V>,
	entries: (self: Map<K, V>) -> Array<Tuple<K, V>>,
	ipairs: (self: Map<K, V>) -> any,
	[K]: V,
	_map: { [K]: V },
	_array: { [number]: K },
}

function Map:entries()
	return {}
end

local function coerceToTable(mapLike: Map<any, any> | Table<any, any>): Array<Tuple<any, any>>
    local e = mapLike:entries();
    return e
end

    )";

    CheckResult result = frontend.check("Module/Map");

    LUAU_REQUIRE_NO_ERRORS(result);
}

// We would prefer this unification to be able to complete, but at least it should not crash
TEST_CASE_FIXTURE(BuiltinsFixture, "table_unification_infinite_recursion")
{
#if defined(_NOOPT) || defined(_DEBUG)
    ScopedFastInt LuauTypeInferRecursionLimit{FInt::LuauTypeInferRecursionLimit, 100};
#endif

    fileResolver.source["game/A"] = R"(
local tbl = {}

function tbl:f1(state)
    self.someNonExistentvalue2 = state
end

function tbl:f2()
    self.someNonExistentvalue:Dc()
end

function tbl:f3()
    self:f2()
    self:f1(false)
end
return tbl
    )";

    fileResolver.source["game/B"] = R"(
local tbl = require(game.A)
tbl:f3()
    )";

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        // TODO: DCR should transform RecursionLimitException into a CodeTooComplex error (currently it rethows it as InternalCompilerError)
        CHECK_THROWS_AS(frontend.check("game/B"), Luau::InternalCompilerError);
    }
    else
    {
        CheckResult result = frontend.check("game/B");
        LUAU_REQUIRE_ERROR_COUNT(1, result);
    }
}

// Ideally, unification with any will not cause a 2^n normalization of a function overload
TEST_CASE_FIXTURE(BuiltinsFixture, "normalization_limit_in_unify_with_any")
{
    ScopedFastFlag sff[] = {
        {FFlag::DebugLuauDeferredConstraintResolution, true},
    };

    // With default limit, this test will take 10 seconds in NoOpt
    ScopedFastInt luauNormalizeCacheLimit{FInt::LuauNormalizeCacheLimit, 1000};

    // Build a function type with a large overload set
    const int parts = 100;
    std::string source;

    for (int i = 0; i < parts; i++)
        formatAppend(source, "type T%d = { f%d: number }\n", i, i);

    source += "type Instance = { new: (('s0', extra: Instance?) -> T0)";

    for (int i = 1; i < parts; i++)
        formatAppend(source, " & (('s%d', extra: Instance?) -> T%d)", i, i);

    source += " }\n";

    source += R"(
local Instance: Instance = {} :: any

local function foo(a: typeof(Instance.new)) return if a then 2 else 3 end

foo(1 :: any)
)";

    CheckResult result = check(source);

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "luau_roact_useState_nilable_state_1")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        type Dispatch<A> = (A) -> ()
        type BasicStateAction<S> = ((S) -> S) | S

        type ScriptConnection = { Disconnect: (ScriptConnection) -> () }

        local blah = nil :: any

        local function useState<S>(
            initialState: (() -> S) | S,
            ...
        ): (S, Dispatch<BasicStateAction<S>>)
            return blah, blah
        end

        local a, b = useState(nil :: ScriptConnection?)

        if a then
            a:Disconnect()
            b(nil :: ScriptConnection?)
        end
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
        LUAU_REQUIRE_NO_ERRORS(result);
    else
    {
        // This is a known bug in the old solver.

        LUAU_REQUIRE_ERROR_COUNT(1, result);

        CHECK(Location{{19, 14}, {19, 41}} == result.errors[0].location);
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "luau_roact_useState_minimization")
{
    // We don't expect this test to work on the old solver, but it also does not yet work on the new solver.
    // So, we can't just put a scoped fast flag here, or it would block CI.
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        type BasicStateAction<S> = ((S) -> S) | S
        type Dispatch<A> = (A) -> ()

        local function useState<S>(
            initialState: (() -> S) | S
        ): (S, Dispatch<BasicStateAction<S>>)
            -- fake impl that obeys types
            local val = if type(initialState) == "function" then initialState() else initialState
            return val, function(value)
                return value
            end
        end

        local test, setTest = useState(nil :: string?)

        setTest(nil) -- this line causes the type to be narrowed in the old solver!!!

        local function update(value: string)
            print(test)
            setTest(value)
        end

        update("hello")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "bin_prov")
{
    CheckResult result = check(R"(
        local Bin = {}

        function Bin:add(item)
            self.head = { item = item}
            return item
        end

        function Bin:destroy()
            while self.head do
                local item = self.head.item
                if type(item) == "function" then
                    item()
                elseif item.Destroy ~= nil then
                end
                self.head = self.head.next
            end
        end
    )");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "update_phonemes_minimized")
{
    CheckResult result = check(R"(
        local video
        function(response)
            for index = 1, #response do
                video = video
            end
            return video
        end
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "table_containing_non_final_type_is_erroneously_cached")
{
    TypeArena arena;
    Scope globalScope(builtinTypes->anyTypePack);
    UnifierSharedState sharedState{&ice};
    Normalizer normalizer{&arena, builtinTypes, NotNull{&sharedState}};

    TypeId tableTy = arena.addType(TableType{});
    TableType* table = getMutable<TableType>(tableTy);
    REQUIRE(table);

    TypeId freeTy = arena.freshType(&globalScope);

    table->props["foo"] = Property::rw(freeTy);

    std::shared_ptr<const NormalizedType> n1 = normalizer.normalize(tableTy);
    std::shared_ptr<const NormalizedType> n2 = normalizer.normalize(tableTy);

    // This should not hold
    CHECK(n1 == n2);
}

// This is doable with the new solver, but there are some problems we have to work out first.
// CLI-111113
TEST_CASE_FIXTURE(Fixture, "we_cannot_infer_functions_that_return_inconsistently")
{
    CheckResult result = check(R"(
        function find_first<T>(tbl: {T}, el)
            for i, e in tbl do
                if e == el then
                    return i
                end
            end
            return nil
        end
    )");

#if 0
    // This #if block describes what should happen.
    LUAU_CHECK_NO_ERRORS(result);

    // The second argument has type unknown because the == operator does not
    // constrain the type of el.
    CHECK("<T>({T}, unknown) -> number?" == toString(requireType("find_first")));
#else
    // This is what actually happens right now.

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        LUAU_CHECK_ERROR_COUNT(2, result);

        // The second argument should be unknown.  CLI-111111
        CHECK("<T>({T}, 'b) -> number" == toString(requireType("find_first")));
    }
    else
    {
        LUAU_CHECK_ERROR_COUNT(1, result);

        CHECK("<T, b>({T}, b) -> number" == toString(requireType("find_first")));
    }
#endif
}

TEST_SUITE_END();
