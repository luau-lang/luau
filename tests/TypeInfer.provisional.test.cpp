// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeInfer.h"

#include "Fixture.h"

#include "doctest.h"

#include <algorithm>

using namespace Luau;

LUAU_FASTFLAG(LuauTypeMismatchInvarianceInError)

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
    ScopedFastInt sfis{"LuauTypeInferTypePackLoopLimit", 50};

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
    ScopedFastFlag sff{"LuauIntersectionTestForEquality", true};

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

    CHECK_EQ("{| x: string, y: number |}", toString(requireTypeAtPosition({5, 28})));

    // Should be {| x: nil, y: nil |}
    CHECK_EQ("{| x: nil, y: nil |} | {| x: string, y: number |}", toString(requireTypeAtPosition({7, 28})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "bail_early_if_unification_is_too_complicated" * doctest::timeout(0.5))
{
    ScopedFastInt sffi{"LuauTarjanChildLimit", 1};
    ScopedFastInt sffi2{"LuauTypeInferIterationLimit", 1};

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

// FIXME: Move this test to another source file when removing FFlag::LuauLowerBoundsCalculation
TEST_CASE_FIXTURE(Fixture, "do_not_ice_when_trying_to_pick_first_of_generic_type_pack")
{
    ScopedFastFlag sff[]{
        {"LuauReturnAnyInsteadOfICE", true},
    };

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
        {"DebugLuauDeferredConstraintResolution", false},
    };

    CheckResult result = check(R"(
        local function f() return end
        local g = function() return f() end
    )");

    LUAU_REQUIRE_ERRORS(result); // Should not have any errors.
}

TEST_CASE_FIXTURE(Fixture, "weird_fail_to_unify_variadic_pack")
{
    ScopedFastFlag sff[] = {
        // I'm not sure why this is broken without DCR, but it seems to be fixed
        // when DCR is enabled.
        {"DebugLuauDeferredConstraintResolution", false},
    };

    CheckResult result = check(R"(
        --!strict
        local function f(...) return ... end
        local g = function(...) return f(...) end
    )");

    LUAU_REQUIRE_ERRORS(result); // Should not have any errors.
}

// Belongs in TypeInfer.builtins.test.cpp.
TEST_CASE_FIXTURE(BuiltinsFixture, "pcall_returns_at_least_two_value_but_function_returns_nothing")
{
    ScopedFastFlag sff{"LuauBetterMessagingOnCountMismatch", true};

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
    ScopedFastFlag sff{"DebugLuauSharedSelf", true};

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
        {"DebugLuauDeferredConstraintResolution", true},
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
    CHECK("<a>(a, number) -> ()" == toString(requireType("prime_iter")));
}

TEST_CASE_FIXTURE(Fixture, "free_options_cannot_be_unified_together")
{
    TypeArena arena;
    TypeId nilType = builtinTypes->nilType;

    std::unique_ptr scope = std::make_unique<Scope>(builtinTypes->anyTypePack);

    TypeId free1 = arena.addType(FreeTypePack{scope.get()});
    TypeId option1 = arena.addType(UnionType{{nilType, free1}});

    TypeId free2 = arena.addType(FreeTypePack{scope.get()});
    TypeId option2 = arena.addType(UnionType{{nilType, free2}});

    InternalErrorReporter iceHandler;
    UnifierSharedState sharedState{&iceHandler};
    Normalizer normalizer{&arena, builtinTypes, NotNull{&sharedState}};
    Unifier u{NotNull{&normalizer}, Mode::Strict, NotNull{scope.get()}, Location{}, Variance::Covariant};

    u.tryUnify(option1, option2);

    CHECK(u.errors.empty());

    u.log.commit();

    ToStringOptions opts;
    CHECK("a?" == toString(option1, opts));

    // CHECK("a?" == toString(option2, opts)); // This should hold, but does not.
    CHECK("b?" == toString(option2, opts)); // This should not hold.
}

TEST_CASE_FIXTURE(BuiltinsFixture, "for_in_loop_with_zero_iterators")
{
    ScopedFastFlag sff{"DebugLuauDeferredConstraintResolution", false};

    CheckResult result = check(R"(
        function no_iter() end
        for key in no_iter() do end -- This should not be ok
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

// Ideally, we would not try to export a function type with generic types from incorrect scope
TEST_CASE_FIXTURE(BuiltinsFixture, "generic_type_leak_to_module_interface")
{
    ScopedFastFlag luauScopelessModule{"LuauScopelessModule", true};

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

    ModulePtr m = frontend.moduleResolver.modules["game/B"];
    REQUIRE(m);

    std::optional<TypeId> result = first(m->returnType);
    REQUIRE(result);
    CHECK(get<AnyType>(*result));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "generic_type_leak_to_module_interface_variadic")
{
    ScopedFastFlag luauScopelessModule{"LuauScopelessModule", true};

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

    ModulePtr m = frontend.moduleResolver.modules["game/B"];
    REQUIRE(m);

    std::optional<TypeId> result = first(m->returnType);
    REQUIRE(result);
    CHECK(get<AnyType>(*result));
}

// We need a simplification step to make this do the right thing. ("normalization-lite")
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

    // We'd really like for this to be {string}
    CHECK_EQ("{string | string}", toString(requireType("t")));
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

    if (FFlag::LuauTypeMismatchInvarianceInError)
    {
        CHECK_EQ(R"(Type '{| x: number? |}' could not be converted into '{| x: number |}'
caused by:
  Property 'x' is not compatible. Type 'number?' could not be converted into 'number' in an invariant context)",
            toString(result.errors[0]));
    }
    else
    {
        CHECK_EQ(R"(Type '{| x: number? |}' could not be converted into '{| x: number |}'
caused by:
  Property 'x' is not compatible. Type 'number?' could not be converted into 'number')",
            toString(result.errors[0]));
    }
}

TEST_SUITE_END();
