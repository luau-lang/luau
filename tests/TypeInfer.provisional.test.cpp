// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Parser.h"
#include "Luau/TypeInfer.h"

#include "Fixture.h"

#include "doctest.h"

#include <algorithm>

LUAU_FASTFLAG(LuauEqConstraint)
LUAU_FASTFLAG(LuauQuantifyInPlace2)

using namespace Luau;

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

    const std::string old_expected = R"(
        function f(a:{fn:()->(free,free...)}): ()
            if type(a) == 'boolean'then
                local a1:boolean=a
            elseif a.fn()then
                local a2:{fn:()->(free,free...)}=a
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

    if (FFlag::LuauQuantifyInPlace2)
        CHECK_EQ(expected, decorateWithTypes(code));
    else
        CHECK_EQ(old_expected, decorateWithTypes(code));
}

TEST_CASE_FIXTURE(Fixture, "xpcall_returns_what_f_returns")
{
    const std::string code = R"(
        local a, b, c = xpcall(function() return 1, "foo" end, function() return "foo", 1 end)
    )";

    const std::string expected = R"(
        local a:boolean,b:number,c:string=xpcall(function(): (number,string)return 1,'foo'end,function(): (string,number)return'foo',1 end)
    )";

    CHECK_EQ(expected, decorateWithTypes(code));
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
TEST_CASE_FIXTURE(Fixture, "setmetatable_constrains_free_type_into_free_table")
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

TEST_CASE_FIXTURE(Fixture, "pass_a_union_of_tables_to_a_function_that_requires_a_table")
{
    CheckResult result = check(R"(
        local a: {x: number, y: number, [any]: any} | {y: number}

        function f(t)
            t.y = 1
            return t
        end

        local b = f(a)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // :(
    // Should be the same as the type of a
    REQUIRE_EQ("{| y: number |}", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "pass_a_union_of_tables_to_a_function_that_requires_a_table_2")
{
    CheckResult result = check(R"(
        local a: {y: number} | {x: number, y: number, [any]: any}

        function f(t)
            t.y = 1
            return t
        end

        local b = f(a)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // :(
    // Should be the same as the type of a
    REQUIRE_EQ("{| [any]: any, x: number, y: number |}", toString(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "normal_conditional_expression_has_refinements")
{
    CheckResult result = check(R"(
        local foo: {x: number}? = nil
        local bar = foo and foo.x -- TODO: Geez. We are inferring the wrong types here. Should be 'number?'.
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    // Binary and/or return types are straight up wrong. JIRA: CLI-40300
    CHECK_EQ("boolean | number", toString(requireType("bar")));
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
TEST_CASE_FIXTURE(Fixture, "error_on_eq_metamethod_returning_a_type_other_than_boolean")
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

// Requires success typing to confidently determine that this expression has no overlap.
TEST_CASE_FIXTURE(Fixture, "operator_eq_completely_incompatible")
{
    CheckResult result = check(R"(
        local a: string | number = "hi"
        local b: {x: string}? = {x = "bye"}

        local r1 = a == b
        local r2 = b == a
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

// Belongs in TypeInfer.refinements.test.cpp.
// We'll need to not only report an error on `a == b`, but also to refine both operands as `never` in the `==` branch.
TEST_CASE_FIXTURE(Fixture, "lvalue_equals_another_lvalue_with_no_overlap")
{
    ScopedFastFlag sff1{"LuauEqConstraint", true};

    CheckResult result = check(R"(
        local function f(a: string, b: boolean?)
            if a == b then
                local foo, bar = a, b
            else
                local foo, bar = a, b
            end
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireTypeAtPosition({3, 33})), "string");   // a == b
    CHECK_EQ(toString(requireTypeAtPosition({3, 36})), "boolean?"); // a == b

    CHECK_EQ(toString(requireTypeAtPosition({5, 33})), "string");   // a ~= b
    CHECK_EQ(toString(requireTypeAtPosition({5, 36})), "boolean?"); // a ~= b
}

// Also belongs in TypeInfer.refinements.test.cpp.
// Just needs to fully support equality refinement. Which is annoying without type states.
TEST_CASE_FIXTURE(Fixture, "discriminate_from_x_not_equal_to_nil")
{
    ScopedFastFlag sff{"LuauDiscriminableUnions", true};

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

TEST_CASE_FIXTURE(Fixture, "bail_early_if_unification_is_too_complicated" * doctest::timeout(0.5))
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

TEST_CASE_FIXTURE(Fixture, "bail_early_on_typescript_port_of_Result_type" * doctest::timeout(1.0))
{
    ScopedFastInt sffi{"LuauTarjanChildLimit", 400};

    CheckResult result = check(R"LUA(
        --!strict
        local TS = _G[script]
        local lazyGet = TS.import(script, script.Parent.Parent, "util", "lazyLoad").lazyGet
        local unit = TS.import(script, script.Parent.Parent, "util", "Unit").unit
        local Iterator
        lazyGet("Iterator", function(c)
            Iterator = c
        end)
        local Option
        lazyGet("Option", function(c)
            Option = c
        end)
        local Vec
        lazyGet("Vec", function(c)
            Vec = c
        end)
        local Result
        do
            Result = setmetatable({}, {
                __tostring = function()
                    return "Result"
                end,
            })
            Result.__index = Result
            function Result.new(...)
                local self = setmetatable({}, Result)
                self:constructor(...)
                return self
            end
            function Result:constructor(okValue, errValue)
                self.okValue = okValue
                self.errValue = errValue
            end
            function Result:ok(val)
                return Result.new(val, nil)
            end
            function Result:err(val)
                return Result.new(nil, val)
            end
            function Result:fromCallback(c)
                local _0 = c
                local _1, _2 = pcall(_0)
                local result = _1 and {
                    success = true,
                    value = _2,
                } or {
                    success = false,
                    error = _2,
                }
                return result.success and Result:ok(result.value) or Result:err(Option:wrap(result.error))
            end
            function Result:fromVoidCallback(c)
                local _0 = c
                local _1, _2 = pcall(_0)
                local result = _1 and {
                    success = true,
                    value = _2,
                } or {
                    success = false,
                    error = _2,
                }
                return result.success and Result:ok(unit()) or Result:err(Option:wrap(result.error))
            end
            Result.fromPromise = TS.async(function(self, p)
                local _0, _1 = TS.try(function()
                    return TS.TRY_RETURN, { Result:ok(TS.await(p)) }
                end, function(e)
                    return TS.TRY_RETURN, { Result:err(Option:wrap(e)) }
                end)
                if _0 then
                    return unpack(_1)
                end
            end)
            Result.fromVoidPromise = TS.async(function(self, p)
                local _0, _1 = TS.try(function()
                    TS.await(p)
                    return TS.TRY_RETURN, { Result:ok(unit()) }
                end, function(e)
                    return TS.TRY_RETURN, { Result:err(Option:wrap(e)) }
                end)
                if _0 then
                    return unpack(_1)
                end
            end)
            function Result:isOk()
                return self.okValue ~= nil
            end
            function Result:isErr()
                return self.errValue ~= nil
            end
            function Result:contains(x)
                return self.okValue == x
            end
            function Result:containsErr(x)
                return self.errValue == x
            end
            function Result:okOption()
                return Option:wrap(self.okValue)
            end
            function Result:errOption()
                return Option:wrap(self.errValue)
            end
            function Result:map(func)
                return self:isOk() and Result:ok(func(self.okValue)) or Result:err(self.errValue)
            end
            function Result:mapOr(def, func)
                local _0
                if self:isOk() then
                    _0 = func(self.okValue)
                else
                    _0 = def
                end
                return _0
            end
            function Result:mapOrElse(def, func)
                local _0
                if self:isOk() then
                    _0 = func(self.okValue)
                else
                    _0 = def(self.errValue)
                end
                return _0
            end
            function Result:mapErr(func)
                return self:isErr() and Result:err(func(self.errValue)) or Result:ok(self.okValue)
            end
            Result["and"] = function(self, other)
                return self:isErr() and Result:err(self.errValue) or other
            end
            function Result:andThen(func)
                return self:isErr() and Result:err(self.errValue) or func(self.okValue)
            end
            Result["or"] = function(self, other)
                return self:isOk() and Result:ok(self.okValue) or other
            end
            function Result:orElse(other)
                return self:isOk() and Result:ok(self.okValue) or other(self.errValue)
            end
            function Result:expect(msg)
                if self:isOk() then
                    return self.okValue
                else
                    error(msg)
                end
            end
            function Result:unwrap()
                return self:expect("called `Result.unwrap()` on an `Err` value: " .. tostring(self.errValue))
            end
            function Result:unwrapOr(def)
                local _0
                if self:isOk() then
                    _0 = self.okValue
                else
                    _0 = def
                end
                return _0
            end
            function Result:unwrapOrElse(gen)
                local _0
                if self:isOk() then
                    _0 = self.okValue
                else
                    _0 = gen(self.errValue)
                end
                return _0
            end
            function Result:expectErr(msg)
                if self:isErr() then
                    return self.errValue
                else
                    error(msg)
                end
            end
            function Result:unwrapErr()
                return self:expectErr("called `Result.unwrapErr()` on an `Ok` value: " .. tostring(self.okValue))
            end
            function Result:transpose()
                return self:isOk() and self.okValue:map(function(some)
                    return Result:ok(some)
                end) or Option:some(Result:err(self.errValue))
            end
            function Result:flatten()
                return self:isOk() and Result.new(self.okValue.okValue, self.okValue.errValue) or Result:err(self.errValue)
            end
            function Result:match(ifOk, ifErr)
                local _0
                if self:isOk() then
                    _0 = ifOk(self.okValue)
                else
                    _0 = ifErr(self.errValue)
                end
                return _0
            end
            function Result:asPtr()
                local _0 = (self.okValue)
                if _0 == nil then
                    _0 = (self.errValue)
                end
                return _0
            end
        end
        local resultMeta = Result
        resultMeta.__eq = function(a, b)
            return b:match(function(ok)
                return a:contains(ok)
            end, function(err)
                return a:containsErr(err)
            end)
        end
        resultMeta.__tostring = function(result)
            return result:match(function(ok)
                return "Result.ok(" .. tostring(ok) .. ")"
            end, function(err)
                return "Result.err(" .. tostring(err) .. ")"
            end)
        end
        return {
            Result = Result,
        }
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

TEST_CASE_FIXTURE(Fixture, "table_subtyping_shouldn't_add_optional_properties_to_sealed_tables")
{
    CheckResult result = check(R"(
        --!strict
        local function setNumber(t: { p: number? }, x:number) t.p = x end
        local function getString(t: { p: string? }):string return t.p or "" end
        -- This shouldn't type-check!
        local function oh(x:number): string
          local t: {} = {}
          setNumber(t, x)
          return getString(t)
        end
        local s: string = oh(37)
    )");

    // Really this should return an error, but it doesn't
    LUAU_REQUIRE_NO_ERRORS(result);
}

// Should be in TypeInfer.tables.test.cpp
// It's unsound to instantiate tables containing generic methods,
// since mutating properties means table properties should be invariant.
// We currently allow this but we shouldn't!
TEST_CASE_FIXTURE(Fixture, "invariant_table_properties_means_instantiating_tables_in_call_is_unsound")
{
    CheckResult result = check(R"(
        --!strict
        local t = {}
        function t.m(x) return x end
        local a : string = t.m("hi")
        local b : number = t.m(5)
        function f(x : { m : (number)->number })
            x.m = function(x) return 1+x end
        end
        f(t) -- This shouldn't typecheck
        local c : string = t.m("hi")
    )");

    // TODO: this should error!
    // This should be fixed by replacing generic tables by generics with type bounds.
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "self_recursive_instantiated_param")
{
    // Mutability in type function application right now can create strange recursive types
    // TODO: instantiation right now is problematic, in this example should either leave the Table type alone
    // or it should rename the type to 'Self' so that the result will be 'Self<Table>'
    CheckResult result = check(R"(
type Table = { a: number }
type Self<T> = T
local a: Self<Table>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ(toString(requireType("a")), "Table<Table>");
}

TEST_SUITE_END();
