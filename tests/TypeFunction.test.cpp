// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TypeFunction.h"

#include "Luau/ConstraintSolver.h"
#include "Luau/NotNull.h"
#include "Luau/Type.h"

#include "ClassFixture.h"
#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)
LUAU_DYNAMIC_FASTINT(LuauTypeFamilyApplicationCartesianProductLimit)

struct TypeFunctionFixture : Fixture
{
    TypeFunction swapFunction;

    TypeFunctionFixture()
        : Fixture(false)
    {
        swapFunction = TypeFunction{
            /* name */ "Swap",
            /* reducer */
            [](TypeId instance, const std::vector<TypeId>& tys, const std::vector<TypePackId>& tps, NotNull<TypeFunctionContext> ctx
            ) -> TypeFunctionReductionResult<TypeId>
            {
                LUAU_ASSERT(tys.size() == 1);
                TypeId param = follow(tys.at(0));

                if (isString(param))
                {
                    return TypeFunctionReductionResult<TypeId>{ctx->builtins->numberType, false, {}, {}};
                }
                else if (isNumber(param))
                {
                    return TypeFunctionReductionResult<TypeId>{ctx->builtins->stringType, false, {}, {}};
                }
                else if (is<BlockedType>(param) || is<PendingExpansionType>(param) || is<TypeFunctionInstanceType>(param) ||
                         (ctx->solver && ctx->solver->hasUnresolvedConstraints(param)))
                {
                    return TypeFunctionReductionResult<TypeId>{std::nullopt, false, {param}, {}};
                }
                else
                {
                    return TypeFunctionReductionResult<TypeId>{std::nullopt, true, {}, {}};
                }
            }
        };

        unfreeze(frontend.globals.globalTypes);
        TypeId t = frontend.globals.globalTypes.addType(GenericType{"T"});
        GenericTypeDefinition genericT{t};

        ScopePtr globalScope = frontend.globals.globalScope;
        globalScope->exportedTypeBindings["Swap"] =
            TypeFun{{genericT}, frontend.globals.globalTypes.addType(TypeFunctionInstanceType{NotNull{&swapFunction}, {t}, {}})};
        freeze(frontend.globals.globalTypes);
    }
};

TEST_SUITE_BEGIN("TypeFunctionTests");

TEST_CASE_FIXTURE(TypeFunctionFixture, "basic_type_function")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type A = Swap<number>
        type B = Swap<string>
        type C = Swap<boolean>

        local x = 123
        local y: Swap<typeof(x)> = "foo"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK("string" == toString(requireTypeAlias("A")));
    CHECK("number" == toString(requireTypeAlias("B")));
    CHECK("Swap<boolean>" == toString(requireTypeAlias("C")));
    CHECK("string" == toString(requireType("y")));
    CHECK("Type function instance Swap<boolean> is uninhabited" == toString(result.errors[0]));
};

TEST_CASE_FIXTURE(TypeFunctionFixture, "function_as_fn_ret")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local swapper: <T>(T) -> Swap<T>
        local a = swapper(123)
        local b = swapper("foo")
        local c = swapper(false)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK("string" == toString(requireType("a")));
    CHECK("number" == toString(requireType("b")));
    CHECK("Swap<boolean>" == toString(requireType("c")));
    CHECK("Type function instance Swap<boolean> is uninhabited" == toString(result.errors[0]));
}

TEST_CASE_FIXTURE(TypeFunctionFixture, "function_as_fn_arg")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local swapper: <T>(Swap<T>) -> T
        local a = swapper(123)
        local b = swapper(false)
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK("unknown" == toString(requireType("a")));
    CHECK("unknown" == toString(requireType("b")));
    CHECK("Type 'number' could not be converted into 'never'" == toString(result.errors[0]));
    CHECK("Type 'boolean' could not be converted into 'never'" == toString(result.errors[1]));
}

TEST_CASE_FIXTURE(TypeFunctionFixture, "resolve_deep_functions")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local x: Swap<Swap<Swap<string>>>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("number" == toString(requireType("x")));
}

TEST_CASE_FIXTURE(TypeFunctionFixture, "unsolvable_function")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local impossible: <T>(Swap<T>) -> Swap<Swap<T>>
        local a = impossible(123)
        local b = impossible(true)
    )");

    LUAU_REQUIRE_ERROR_COUNT(6, result);
    CHECK(toString(result.errors[0]) == "Type function instance Swap<Swap<T>> is uninhabited");
    CHECK(toString(result.errors[1]) == "Type function instance Swap<T> is uninhabited");
    CHECK(toString(result.errors[2]) == "Type function instance Swap<Swap<T>> is uninhabited");
    CHECK(toString(result.errors[3]) == "Type function instance Swap<T> is uninhabited");
    CHECK(toString(result.errors[4]) == "Type function instance Swap<Swap<T>> is uninhabited");
    CHECK(toString(result.errors[5]) == "Type function instance Swap<T> is uninhabited");
}

TEST_CASE_FIXTURE(TypeFunctionFixture, "table_internal_functions")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local t: <T>({T}) -> {Swap<T>}
        local a = t({1, 2, 3})
        local b = t({"a", "b", "c"})
        local c = t({true, false, true})
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(requireType("a")) == "{string}");
    CHECK(toString(requireType("b")) == "{number}");
    // FIXME: table types are constructing a trivial union here.
    CHECK(toString(requireType("c")) == "{Swap<boolean | boolean | boolean>}");
    CHECK(toString(result.errors[0]) == "Type function instance Swap<boolean | boolean | boolean> is uninhabited");
}

TEST_CASE_FIXTURE(TypeFunctionFixture, "function_internal_functions")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local f0: <T>(T) -> (() -> T)
        local f: <T>(T) -> (() -> Swap<T>)
        local a = f(1)
        local b = f("a")
        local c = f(true)
        local d = f0(1)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(requireType("a")) == "() -> string");
    CHECK(toString(requireType("b")) == "() -> number");
    CHECK(toString(requireType("c")) == "() -> Swap<boolean>");
    CHECK(toString(result.errors[0]) == "Type function instance Swap<boolean> is uninhabited");
}

TEST_CASE_FIXTURE(Fixture, "add_function_at_work")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local function add(a, b)
            return a + b
        end

        local a = add(1, 2)
        local b = add(1, "foo")
        local c = add("foo", 1)
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(toString(requireType("a")) == "number");
    CHECK(toString(requireType("b")) == "add<number, string>");
    CHECK(toString(requireType("c")) == "add<string, number>");
    CHECK(
        toString(result.errors[0]) ==
        "Operator '+' could not be applied to operands of types number and string; there is no corresponding overload for __add"
    );
    CHECK(
        toString(result.errors[1]) ==
        "Operator '+' could not be applied to operands of types string and number; there is no corresponding overload for __add"
    );
}

TEST_CASE_FIXTURE(BuiltinsFixture, "cyclic_add_function_at_work")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type T = add<number | T, number>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireTypeAlias("T")) == "number");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "mul_function_with_union_of_multiplicatives")
{
    if (!FFlag::LuauSolverV2)
        return;

    loadDefinition(R"(
        declare class Vec2
            function __mul(self, rhs: number): Vec2
        end

        declare class Vec3
            function __mul(self, rhs: number): Vec3
        end
    )");

    CheckResult result = check(R"(
        type T = mul<Vec2 | Vec3, number>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireTypeAlias("T")) == "Vec2 | Vec3");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "mul_function_with_union_of_multiplicatives_2")
{
    if (!FFlag::LuauSolverV2)
        return;

    loadDefinition(R"(
        declare class Vec3
            function __mul(self, rhs: number): Vec3
            function __mul(self, rhs: Vec3): Vec3
        end
    )");

    CheckResult result = check(R"(
        type T = mul<number | Vec3, Vec3>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireTypeAlias("T")) == "Vec3");
}

TEST_CASE_FIXTURE(Fixture, "internal_functions_raise_errors")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local function innerSum(a, b)
            local _ = a + b
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(
        toString(result.errors[0]) ==
        "Operator '+' could not be applied to operands of types unknown and unknown; there is no corresponding overload for __add"
    );
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_functions_can_be_shadowed")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type add<T> = string -- shadow add

        -- this should be ok
        function hi(f: add<unknown>)
            return string.format("hi %s", f)
        end

        -- this should still work totally fine (and use the real type function)
        function plus(a, b)
            return a + b
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(toString(requireType("hi")) == "(string) -> string");
    CHECK(toString(requireType("plus")) == "<a, b>(a, b) -> add<a, b>");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_functions_inhabited_with_normalization")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local useGridConfig : any
        local columns = useGridConfig("columns", {}) or 1
        local gutter = useGridConfig('gutter', {}) or 0
        local margin = useGridConfig('margin', {}) or 0
        return function(frameAbsoluteWidth: number)
            local cellAbsoluteWidth = (frameAbsoluteWidth - 2 * margin + gutter) / columns - gutter
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "keyof_type_function_works")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = { x: number, y: number, z: number }
        type KeysOfMyObject = keyof<MyObject>

        local function ok(idx: KeysOfMyObject): "x" | "y" | "z" return idx end
        local function err(idx: KeysOfMyObject): "x" | "y" return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK_EQ("\"x\" | \"y\"", toString(tpm->wantedTp));
    CHECK_EQ("\"x\" | \"y\" | \"z\"", toString(tpm->givenTp));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "keyof_type_function_works_with_metatables")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local metatable = { __index = {w = 1} }
        local obj = setmetatable({x = 1, y = 2, z = 3}, metatable)
        type MyObject = typeof(obj)
        type KeysOfMyObject = keyof<MyObject>

        local function ok(idx: KeysOfMyObject): "w" | "x" | "y" | "z" return idx end
        local function err(idx: KeysOfMyObject): "x" | "y" | "z" return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK_EQ("\"x\" | \"y\" | \"z\"", toString(tpm->wantedTp));
    CHECK_EQ("\"w\" | \"x\" | \"y\" | \"z\"", toString(tpm->givenTp));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "keyof_single_entry_no_uniontype")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local tbl_A = { abc = "value" }
        local tbl_B = { a1 = nil, ["a2"] = nil }

        type keyof_A = keyof<typeof(tbl_A)>
        type keyof_B = keyof<typeof(tbl_B)>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(toString(requireTypeAlias("keyof_A")) == "\"abc\"");
    CHECK(toString(requireTypeAlias("keyof_B")) == "\"a1\" | \"a2\"");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "keyof_type_function_errors_if_it_has_nontable_part")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = { x: number, y: number, z: number }
        type KeysOfMyObject = keyof<MyObject | boolean>

        local function err(idx: KeysOfMyObject): "x" | "y" | "z" return idx end
    )");

    // FIXME(CLI-95289): we should actually only report the type function being uninhabited error at its first use, I think?
    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(toString(result.errors[0]) == "Type 'MyObject | boolean' does not have keys, so 'keyof<MyObject | boolean>' is invalid");
    CHECK(toString(result.errors[1]) == "Type 'MyObject | boolean' does not have keys, so 'keyof<MyObject | boolean>' is invalid");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "keyof_type_function_string_indexer")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = { x: number, y: number, z: number }
        type MyOtherObject = { [string]: number }
        type KeysOfMyOtherObject = keyof<MyOtherObject>
        type KeysOfMyObjects = keyof<MyObject | MyOtherObject>

        local function ok(idx: KeysOfMyOtherObject): "z" return idx end
        local function err(idx: KeysOfMyObjects): "z" return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK_EQ("\"z\"", toString(tpm->wantedTp));
    CHECK_EQ("string", toString(tpm->givenTp));

    tpm = get<TypePackMismatch>(result.errors[1]);
    REQUIRE(tpm);
    CHECK_EQ("\"z\"", toString(tpm->wantedTp));
    CHECK_EQ("\"x\" | \"y\" | \"z\"", toString(tpm->givenTp));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "keyof_type_function_common_subset_if_union_of_differing_tables")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = { x: number, y: number, z: number }
        type MyOtherObject = { w: number, y: number, z: number }
        type KeysOfMyObject = keyof<MyObject | MyOtherObject>

        local function err(idx: KeysOfMyObject): "z" return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK_EQ("\"z\"", toString(tpm->wantedTp));
    CHECK_EQ("\"y\" | \"z\"", toString(tpm->givenTp));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "keyof_type_function_never_for_empty_table")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type KeyofEmpty = keyof<{}>

        local foo = ((nil :: any) :: KeyofEmpty)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireType("foo")) == "never");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rawkeyof_type_function_works")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = { x: number, y: number, z: number }
        type KeysOfMyObject = rawkeyof<MyObject>

        local function ok(idx: KeysOfMyObject): "x" | "y" | "z" return idx end
        local function err(idx: KeysOfMyObject): "x" | "y" return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK_EQ("\"x\" | \"y\"", toString(tpm->wantedTp));
    CHECK_EQ("\"x\" | \"y\" | \"z\"", toString(tpm->givenTp));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rawkeyof_type_function_ignores_metatables")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local metatable = { __index = {w = 1} }
        local obj = setmetatable({x = 1, y = 2, z = 3}, metatable)
        type MyObject = typeof(obj)
        type KeysOfMyObject = rawkeyof<MyObject>

        local function ok(idx: KeysOfMyObject): "x" | "y" | "z" return idx end
        local function err(idx: KeysOfMyObject): "x" | "y" return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK_EQ("\"x\" | \"y\"", toString(tpm->wantedTp));
    CHECK_EQ("\"x\" | \"y\" | \"z\"", toString(tpm->givenTp));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rawkeyof_type_function_errors_if_it_has_nontable_part")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = { x: number, y: number, z: number }
        type KeysOfMyObject = rawkeyof<MyObject | boolean>

        local function err(idx: KeysOfMyObject): "x" | "y" | "z" return idx end
    )");

    // FIXME(CLI-95289): we should actually only report the type function being uninhabited error at its first use, I think?
    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(toString(result.errors[0]) == "Type 'MyObject | boolean' does not have keys, so 'rawkeyof<MyObject | boolean>' is invalid");
    CHECK(toString(result.errors[1]) == "Type 'MyObject | boolean' does not have keys, so 'rawkeyof<MyObject | boolean>' is invalid");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rawkeyof_type_function_common_subset_if_union_of_differing_tables")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = { x: number, y: number, z: number }
        type MyOtherObject = { w: number, y: number, z: number }
        type KeysOfMyObject = rawkeyof<MyObject | MyOtherObject>

        local function err(idx: KeysOfMyObject): "z" return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK_EQ("\"z\"", toString(tpm->wantedTp));
    CHECK_EQ("\"y\" | \"z\"", toString(tpm->givenTp));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rawkeyof_type_function_never_for_empty_table")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type RawkeyofEmpty = rawkeyof<{}>

        local foo = ((nil :: any) :: RawkeyofEmpty)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireType("foo")) == "never");
}

TEST_CASE_FIXTURE(ClassFixture, "keyof_type_function_works_on_classes")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type KeysOfMyObject = keyof<BaseClass>

        local function ok(idx: KeysOfMyObject): "BaseMethod" | "BaseField" | "Touched" return idx end
        local function err(idx: KeysOfMyObject): "BaseMethod" return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK_EQ("\"BaseMethod\"", toString(tpm->wantedTp));
    CHECK_EQ("\"BaseField\" | \"BaseMethod\" | \"Touched\"", toString(tpm->givenTp));
}

TEST_CASE_FIXTURE(ClassFixture, "keyof_type_function_errors_if_it_has_nonclass_part")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type KeysOfMyObject = keyof<BaseClass | boolean>

        local function err(idx: KeysOfMyObject): "BaseMethod" | "BaseField" return idx end
    )");

    // FIXME(CLI-95289): we should actually only report the type function being uninhabited error at its first use, I think?
    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(toString(result.errors[0]) == "Type 'BaseClass | boolean' does not have keys, so 'keyof<BaseClass | boolean>' is invalid");
    CHECK(toString(result.errors[1]) == "Type 'BaseClass | boolean' does not have keys, so 'keyof<BaseClass | boolean>' is invalid");
}

TEST_CASE_FIXTURE(ClassFixture, "keyof_type_function_common_subset_if_union_of_differing_classes")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type KeysOfMyObject = keyof<BaseClass | Vector2>

        local function ok(idx: KeysOfMyObject): never return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "keyof_type_function_works_with_parent_classes_too")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type KeysOfMyObject = keyof<ChildClass>

        local function ok(idx: KeysOfMyObject): "BaseField" | "BaseMethod" | "Method" | "Touched" return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "binary_type_function_works_with_default_argument")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type result = mul<number>

        local function thunk(): result return 5 * 4 end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK("() -> number" == toString(requireType("thunk")));
}

TEST_CASE_FIXTURE(ClassFixture, "vector2_multiply_is_overloaded")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local v = Vector2.New(1, 2)

        local v2 = v * 1.5
        local v3 = v * v
        local v4 = v * "Hello" -- line 5
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK(5 == result.errors[0].location.begin.line);
    CHECK(5 == result.errors[0].location.end.line);

    CHECK("Vector2" == toString(requireType("v2")));
    CHECK("Vector2" == toString(requireType("v3")));
    CHECK("mul<Vector2, string>" == toString(requireType("v4")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "keyof_rfc_example")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local animals = {
            cat = { speak = function() print "meow" end },
            dog = { speak = function() print "woof woof" end },
            monkey = { speak = function() print "oo oo" end },
            fox = { speak = function() print "gekk gekk" end }
        }

        type AnimalType = keyof<typeof(animals)>

        function speakByType(animal: AnimalType)
            animals[animal].speak()
        end

        speakByType("dog") -- ok
        speakByType("cactus") -- errors
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("\"cat\" | \"dog\" | \"fox\" | \"monkey\"", toString(tm->wantedType));
    CHECK_EQ("\"cactus\"", toString(tm->givenType));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "keyof_oss_crash_gh1161")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local EnumVariants = {
            ["a"] = 1, ["b"] = 2, ["c"] = 3
        }

        type EnumKey = keyof<typeof(EnumVariants)>

        function fnA<T>(i: T): keyof<T> end

        function fnB(i: EnumKey) end

        local result = fnA(EnumVariants)
        fnB(result)
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(get<ConstraintSolvingIncompleteError>(result.errors[0]));
    CHECK(get<FunctionExitsWithoutReturning>(result.errors[1]));
}

TEST_CASE_FIXTURE(TypeFunctionFixture, "fuzzer_numeric_binop_doesnt_assert_on_generalizeFreeType")
{
    CheckResult result = check(R"(
Module 'l0':
local _ = (67108864)(_ >= _).insert
do end
do end
_(...,_(_,_(_()),_()))
(67108864)()()
_(_ ~= _ // _,l0)(_(_({n0,})),_(_),_)
_(setmetatable(_,{[...]=_,}))

)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "cyclic_concat_function_at_work")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type T = concat<string | T, string>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireTypeAlias("T")) == "string");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "exceeded_distributivity_limits")
{
    if (!FFlag::LuauSolverV2)
        return;

    ScopedFastInt sfi{DFInt::LuauTypeFamilyApplicationCartesianProductLimit, 10};

    loadDefinition(R"(
        declare class A
            function __mul(self, rhs: unknown): A
        end

        declare class B
            function __mul(self, rhs: unknown): B
        end

        declare class C
            function __mul(self, rhs: unknown): C
        end

        declare class D
            function __mul(self, rhs: unknown): D
        end
    )");

    CheckResult result = check(R"(
        type T = mul<A | B | C | D, A | B | C | D>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(get<UninhabitedTypeFunction>(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "didnt_quite_exceed_distributivity_limits")
{
    if (!FFlag::LuauSolverV2)
        return;

    // We duplicate the test here because we want to make sure the test failed
    // due to exceeding the limits specifically, rather than any possible reasons.
    ScopedFastInt sfi{DFInt::LuauTypeFamilyApplicationCartesianProductLimit, 20};

    loadDefinition(R"(
        declare class A
            function __mul(self, rhs: unknown): A
        end

        declare class B
            function __mul(self, rhs: unknown): B
        end

        declare class C
            function __mul(self, rhs: unknown): C
        end

        declare class D
            function __mul(self, rhs: unknown): D
        end
    )");

    CheckResult result = check(R"(
        type T = mul<A | B | C | D, A | B | C | D>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "ensure_equivalence_with_distributivity")
{
    if (!FFlag::LuauSolverV2)
        return;

    loadDefinition(R"(
        declare class A
            function __mul(self, rhs: unknown): A
        end

        declare class B
            function __mul(self, rhs: unknown): B
        end

        declare class C
            function __mul(self, rhs: unknown): C
        end

        declare class D
            function __mul(self, rhs: unknown): D
        end
    )");

    CheckResult result = check(R"(
        type T = mul<A | B, C | D>
        type U = mul<A, C> | mul<A, D> | mul<B, C> | mul<B, D>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireTypeAlias("T")) == "A | B");
    CHECK(toString(requireTypeAlias("U")) == "A | A | B | B");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "we_shouldnt_warn_that_a_reducible_type_function_is_uninhabited")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(

local Debounce = false
local Active = false

local function Use(Mode)

	if Mode ~= nil then

		if Mode == false and Active == false then
			return
		else
			Active = not Mode
		end

		Debounce = false
	end
	Active = not Active

end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_type_function_works")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = {a: string, b: number, c: boolean}
        type IdxAType = index<MyObject, "a">
        type IdxBType = index<MyObject, keyof<MyObject>>

        local function ok(idx: IdxAType): string return idx end
        local function ok2(idx: IdxBType): string | number | boolean return idx end
        local function err(idx: IdxAType): boolean return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK_EQ("boolean", toString(tpm->wantedTp));
    CHECK_EQ("string", toString(tpm->givenTp));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_wait_for_pending_no_crash")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local PlayerData = {
            Coins = 0,
            Level = 1,
            Exp = 0,
            MaxExp = 100
        }
        type Keys = index<typeof(PlayerData), keyof<typeof(PlayerData)>>
        -- This function makes it think that there's going to be a pending expansion
        local function UpdateData(key: Keys, value)
            PlayerData[key] = value
        end
        UpdateData("Coins", 2)
    )");

    // Should not crash!
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_type_function_works_w_array")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local MyObject = {"hello", 1, true}
        type IdxAType = index<typeof(MyObject), number>

        local function ok(idx: IdxAType): string | number | boolean return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_type_function_works_w_generic_types")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local function access<T, K>(tbl: T & {}, key: K): index<T, K>
            return tbl[key]
        end

        local subjects = {
            english = "boring",
            math = "fun"
        }

        local key: "english" = "english"
        local a: string = access(subjects, key)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_type_function_errors_w_bad_indexer")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = {a: string, b: number, c: boolean}
        type errType1 = index<MyObject, "d">
        type errType2 = index<MyObject, boolean>
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(toString(result.errors[0]) == "Property '\"d\"' does not exist on type 'MyObject'");
    CHECK(toString(result.errors[1]) == "Property 'boolean' does not exist on type 'MyObject'");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_type_function_errors_w_var_indexer")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = {a: string, b: number, c: boolean}
        local key = "a"

        type errType1 = index<MyObject, key>
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(toString(result.errors[0]) == "Second argument to index<MyObject, _> is not a valid index type");
    CHECK(toString(result.errors[1]) == "Unknown type 'key'");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_type_function_works_w_union_type_indexer")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = {a: string, b: number, c: boolean}

        type idxType = index<MyObject, "a" | "b">
        local function ok(idx: idxType): string | number return idx end

        type errType = index<MyObject, "a" | "d">
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == "Property '\"a\" | \"d\"' does not exist on type 'MyObject'");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_type_function_works_w_union_type_indexee")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = {a: string, b: number, c: boolean}
        type MyObject2 = {a: number}

        type idxTypeA = index<MyObject | MyObject2, "a">
        local function ok(idx: idxTypeA): string | number return idx end

        type errType = index<MyObject | MyObject2, "b">
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == "Property '\"b\"' does not exist on type 'MyObject | MyObject2'");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_type_function_rfc_alternative_section")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = {a: string}
        type MyObject2 = {a: string, b: number}

        local function edgeCase(param: MyObject) 
            type unknownType = index<typeof(param), "b">
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == "Property '\"b\"' does not exist on type 'MyObject'");
}

TEST_CASE_FIXTURE(ClassFixture, "index_type_function_works_on_classes")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type KeysOfMyObject = index<BaseClass, "BaseField">

        local function ok(idx: KeysOfMyObject): number return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "index_type_function_works_on_classes_with_parents")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type KeysOfMyObject = index<ChildClass, "BaseField">

        local function ok(idx: KeysOfMyObject): number return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_type_function_works_w_index_metatables")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local exampleClass = { Foo = "text", Bar = true }

        local exampleClass2 = setmetatable({ Foo = 8 }, { __index = exampleClass })
        type exampleTy2 = index<typeof(exampleClass2), "Foo">
        local function ok(idx: exampleTy2): number return idx end

        local exampleClass3 = setmetatable({ Bar = 5 }, { __index = exampleClass })
        type exampleTy3 = index<typeof(exampleClass3), "Foo">
        local function ok2(idx: exampleTy3): string return idx end

        type exampleTy4 = index<typeof(exampleClass3), "Foo" | "Bar">
        local function ok3(idx: exampleTy4): string | number return idx end

        type errTy = index<typeof(exampleClass2), "Car">
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == "Property '\"Car\"' does not exist on type 'exampleClass2'");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rawget_type_function_works")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = {a: string, b: number, c: boolean}
        type RawAType = rawget<MyObject, "a">
        type RawBType = rawget<MyObject, keyof<MyObject>>
        local function ok(idx: RawAType): string return idx end
        local function ok2(idx: RawBType): string | number | boolean return idx end
        local function err(idx: RawAType): boolean return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK_EQ("boolean", toString(tpm->wantedTp));
    CHECK_EQ("string", toString(tpm->givenTp));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rawget_type_function_works_w_array")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local MyObject = {"hello", 1, true}
        type RawAType = rawget<typeof(MyObject), number>
        local function ok(idx: RawAType): string | number | boolean return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rawget_type_function_errors_w_var_indexer")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = {a: string, b: number, c: boolean}
        local key = "a"
        type errType1 = rawget<MyObject, key>
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(toString(result.errors[0]) == "Second argument to rawget<MyObject, _> is not a valid index type");
    CHECK(toString(result.errors[1]) == "Unknown type 'key'");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rawget_type_function_works_w_union_type_indexer")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = {a: string, b: number, c: boolean}
        type rawType = rawget<MyObject, "a" | "b">
        local function ok(idx: rawType): string | number return idx end
        type errType = rawget<MyObject, "a" | "d">
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == "Property '\"a\" | \"d\"' does not exist on type 'MyObject'");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rawget_type_function_works_w_union_type_indexee")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type MyObject = {a: string, b: number, c: boolean}
        type MyObject2 = {a: number}
        type rawTypeA = rawget<MyObject | MyObject2, "a">
        local function ok(idx: rawTypeA): string | number return idx end
        type errType = rawget<MyObject | MyObject2, "b">
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == "Property '\"b\"' does not exist on type 'MyObject | MyObject2'");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rawget_type_function_works_w_index_metatables")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local exampleClass = { Foo = "text", Bar = true }
        local exampleClass2 = setmetatable({ Foo = 8 }, { __index = exampleClass })
        type exampleTy2 = rawget<typeof(exampleClass2), "Foo">
        local function ok(idx: exampleTy2): number return idx end
        local exampleClass3 = setmetatable({ Bar = 5 }, { __index = exampleClass })
        type errType = rawget<typeof(exampleClass3), "Foo">
        type errType2 = rawget<typeof(exampleClass3), "Bar" | "Foo">
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(toString(result.errors[0]) == "Property '\"Foo\"' does not exist on type 'exampleClass3'");
    CHECK(toString(result.errors[1]) == "Property '\"Bar\" | \"Foo\"' does not exist on type 'exampleClass3'");
}

TEST_CASE_FIXTURE(ClassFixture, "rawget_type_function_errors_w_classes")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type PropsOfMyObject = rawget<BaseClass, "BaseField">
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == "Property '\"BaseField\"' does not exist on type 'BaseClass'");
}

TEST_CASE_FIXTURE(Fixture, "fuzz_len_type_function_follow")
{
    // Should not fail assertions
    check(R"(
        local _
        _ = true
        for l0=_,_,# _ do
        end
        for l0=_,_ do
        if _ then
        _ += _
        end
        end
    )");
}

TEST_SUITE_END();
