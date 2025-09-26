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
LUAU_FASTFLAG(DebugLuauAssertOnForcedConstraint)
LUAU_FASTFLAG(LuauRefineOccursCheckDirectRecursion)
LUAU_FASTFLAG(LuauNoMoreComparisonTypeFunctions)
LUAU_FASTFLAG(LuauNameConstraintRestrictRecursiveTypes)
LUAU_FASTFLAG(LuauRawGetHandlesNil)
LUAU_FASTFLAG(LuauBuiltinTypeFunctionsArentGlobal)

struct TypeFunctionFixture : Fixture
{
    TypeFunction swapFunction;

    TypeFunctionFixture()
        : Fixture(false)
    {
        swapFunction =
            TypeFunction{/* name */ "Swap",
                         /* reducer */
                         [](TypeId instance, const std::vector<TypeId>& tys, const std::vector<TypePackId>& tps, NotNull<TypeFunctionContext> ctx)
                             -> TypeFunctionReductionResult<TypeId>
                         {
                             LUAU_ASSERT(tys.size() == 1);
                             TypeId param = follow(tys.at(0));

                             if (isString(param))
                             {
                                 return TypeFunctionReductionResult<TypeId>{ctx->builtins->numberType, Reduction::MaybeOk, {}, {}};
                             }
                             else if (isNumber(param))
                             {
                                 return TypeFunctionReductionResult<TypeId>{ctx->builtins->stringType, Reduction::MaybeOk, {}, {}};
                             }
                             else if (is<BlockedType>(param) || is<PendingExpansionType>(param) || is<TypeFunctionInstanceType>(param) ||
                                      (ctx->solver && ctx->solver->hasUnresolvedConstraints(param)))
                             {
                                 return TypeFunctionReductionResult<TypeId>{std::nullopt, Reduction::MaybeOk, {param}, {}};
                             }
                             else
                             {
                                 return TypeFunctionReductionResult<TypeId>{std::nullopt, Reduction::Erroneous, {}, {}};
                             }
                         }};

        unfreeze(getFrontend().globals.globalTypes);
        TypeId t = getFrontend().globals.globalTypes.addType(GenericType{"T"});
        GenericTypeDefinition genericT{t};

        ScopePtr globalScope = getFrontend().globals.globalScope;
        globalScope->exportedTypeBindings["Swap"] =
            TypeFun{{genericT}, getFrontend().globals.globalTypes.addType(TypeFunctionInstanceType{NotNull{&swapFunction}, {t}, {}})};
        freeze(getFrontend().globals.globalTypes);
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

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(toString(result.errors[0]) == "Type 'number' could not be converted into 'never'");
    CHECK(toString(result.errors[1]) == "Type 'boolean' could not be converted into 'never'");
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
    CHECK(toString(requireType("c")) == "{Swap<boolean>}");
    CHECK(toString(result.errors[0]) == "Type function instance Swap<boolean> is uninhabited");
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

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("\"x\" | \"y\"", toString(tm->wantedType));
    CHECK_EQ("\"x\" | \"y\" | \"z\"", toString(tm->givenType));
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

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("\"x\" | \"y\" | \"z\"", toString(tm->wantedType));
    CHECK_EQ("\"w\" | \"x\" | \"y\" | \"z\"", toString(tm->givenType));
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

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("\"z\"", toString(tm->wantedType));
    CHECK_EQ("string", toString(tm->givenType));

    tm = get<TypeMismatch>(result.errors[1]);
    REQUIRE(tm);
    CHECK_EQ("\"z\"", toString(tm->wantedType));
    CHECK_EQ("\"x\" | \"y\" | \"z\"", toString(tm->givenType));
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

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("\"z\"", toString(tm->wantedType));
    CHECK_EQ("\"y\" | \"z\"", toString(tm->givenType));
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

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("\"x\" | \"y\"", toString(tm->wantedType));
    CHECK_EQ("\"x\" | \"y\" | \"z\"", toString(tm->givenType));
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

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("\"x\" | \"y\"", toString(tm->wantedType));
    CHECK_EQ("\"x\" | \"y\" | \"z\"", toString(tm->givenType));
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

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("\"z\"", toString(tm->wantedType));
    CHECK_EQ("\"y\" | \"z\"", toString(tm->givenType));
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

TEST_CASE_FIXTURE(ExternTypeFixture, "keyof_type_function_works_on_extern_types")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type KeysOfMyObject = keyof<BaseClass>

        local function ok(idx: KeysOfMyObject): "BaseMethod" | "BaseField" | "Touched" return idx end
        local function err(idx: KeysOfMyObject): "BaseMethod" return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("\"BaseMethod\"", toString(tm->wantedType));
    CHECK_EQ("\"BaseField\" | \"BaseMethod\" | \"Touched\"", toString(tm->givenType));
}

TEST_CASE_FIXTURE(ExternTypeFixture, "keyof_type_function_errors_if_it_has_nonclass_part")
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

TEST_CASE_FIXTURE(ExternTypeFixture, "keyof_type_function_common_subset_if_union_of_differing_extern_types")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type KeysOfMyObject = keyof<BaseClass | Vector2>

        local function ok(idx: KeysOfMyObject): never return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "keyof_type_function_works_with_parent_extern_types_too")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type KeysOfMyObject = keyof<ChildClass>

        local function ok(idx: KeysOfMyObject): "BaseField" | "BaseMethod" | "Method" | "Touched" return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "binary_type_function_works_with_default_argument")
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

TEST_CASE_FIXTURE(ExternTypeFixture, "vector2_multiply_is_overloaded")
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

    LUAU_CHECK_ERROR_COUNT(1, result);
    LUAU_CHECK_ERROR(result, FunctionExitsWithoutReturning);
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

TEST_CASE_FIXTURE(BuiltinsFixture, "index_of_any_is_any")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type T = index<any, "a">
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireTypeAlias("T")) == "any");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_should_not_crash_on_cyclic_stuff")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local PlayerData = {}

        type Keys = index<typeof(PlayerData), true>

        local function UpdateData(key: Keys)
            PlayerData[key] = 4
        end
    )");

    LUAU_REQUIRE_ERRORS(result);
    CHECK(toString(requireTypeAlias("Keys")) == "index<PlayerData, true>");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_should_not_crash_on_cyclic_stuff2")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local PlayerData = {}

        type Keys = index<typeof(PlayerData), number>

        local function UpdateData(key: Keys)
            PlayerData[key] = 4
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireTypeAlias("Keys")) == "number");
}

#if 0
// CLI-148701
TEST_CASE_FIXTURE(BuiltinsFixture, "index_should_not_crash_on_cyclic_stuff3")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local PlayerData = {
            Coins = 0,
            Level = 1,
            Exp = 0,
            MapExp = 100,
        }

        type Keys = index<typeof(PlayerData), true>

        local function UpdateData(key: Keys, value)
            PlayerData[key] = value
        end

        UpdateData("Coins", 2)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireTypeAlias("Keys")) == "unknown");
}
#endif

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

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("boolean", toString(tm->wantedType));
    CHECK_EQ("string", toString(tm->givenType));
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

TEST_CASE_FIXTURE(BuiltinsFixture, "cyclic_metatable_should_not_crash_index")
{
    if (!FFlag::LuauSolverV2)
        return;

    // t :: t1 where t1 = {metatable {__index: t1, __tostring: (t1) -> string}}
    CheckResult result = check(R"(
        local mt = {}
        local t = setmetatable({}, mt)
        mt.__index = t

        function mt:__tostring()
            return t.p
        end

        type IndexFromT = index<typeof(t), "p">
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ("Type 't' does not have key 'p'", toString(result.errors[0]));
    CHECK_EQ("Property '\"p\"' does not exist on type 't'", toString(result.errors[1]));
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

TEST_CASE_FIXTURE(BuiltinsFixture, "index_type_function_works_on_function_metamethods")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type Foo = {x: string}
        local t = {}
        setmetatable(t, {
            __index = function(x: string): Foo
                return {x = x}
            end
        })

        type Bar = index<typeof(t), "bar">
    )");

    LUAU_REQUIRE_NO_ERRORS(result);


    CHECK_EQ(toString(requireTypeAlias("Bar"), {true}), "{ x: string }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "index_type_function_works_on_function_metamethods2")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type Foo = {x: string}
        local t = {}
        setmetatable(t, {
            __index = function(x: string): Foo
                return {x = x}
            end
        })

        type Bar = index<typeof(t), number>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
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

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == "Unknown type 'key'");
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

TEST_CASE_FIXTURE(ExternTypeFixture, "index_type_function_works_on_extern_types")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type KeysOfMyObject = index<BaseClass, "BaseField">

        local function ok(idx: KeysOfMyObject): number return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "index_type_function_works_on_extern_types_with_parents")
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

    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK_EQ("boolean", toString(tm->wantedType));
    CHECK_EQ("string", toString(tm->givenType));
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


    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == "Unknown type 'key'");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rawget_type_function_works_w_union_type_indexer")
{
    if (!FFlag::LuauSolverV2)
        return;

    ScopedFastFlag sff{FFlag::LuauRawGetHandlesNil, true};

    CheckResult result = check(R"(
        type MyObject = {a: string, b: number, c: boolean}
        type rawType = rawget<MyObject, "a" | "b">
        local function ok(idx: rawType): string | number return idx end
        type stringType = rawget<MyObject, "a" | "d">
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireTypeAlias("stringType")) == "string?");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rawget_type_function_works_w_union_type_indexee")
{
    if (!FFlag::LuauSolverV2)
        return;

    ScopedFastFlag sff{FFlag::LuauRawGetHandlesNil, true};

    CheckResult result = check(R"(
        type MyObject = {a: string, b: number, c: boolean}
        type MyObject2 = {a: number}
        type rawTypeA = rawget<MyObject | MyObject2, "a">
        local function ok(idx: rawTypeA): string | number return idx end
        type numberType = rawget<MyObject | MyObject2, "b">
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireTypeAlias("numberType")) == "number?");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rawget_type_function_works_w_index_metatables")
{
    if (!FFlag::LuauSolverV2)
        return;

    ScopedFastFlag sff{FFlag::LuauRawGetHandlesNil, true};

    CheckResult result = check(R"(
        local exampleClass = { Foo = "text", Bar = true }
        local exampleClass2 = setmetatable({ Foo = 8 }, { __index = exampleClass })
        type exampleTy2 = rawget<typeof(exampleClass2), "Foo">
        local function ok(idx: exampleTy2): number return idx end
        local exampleClass3 = setmetatable({ Bar = 5 }, { __index = exampleClass })
        type nilType = rawget<typeof(exampleClass3), "Foo">
        type numberType = rawget<typeof(exampleClass3), "Bar" | "Foo">
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireTypeAlias("nilType")) == "nil");
    CHECK(toString(requireTypeAlias("numberType")) == "number?");
}

TEST_CASE_FIXTURE(ExternTypeFixture, "rawget_type_function_errors_w_extern_types")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type PropsOfMyObject = rawget<BaseClass, "BaseField">
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == "Property '\"BaseField\"' does not exist on type 'BaseClass'");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "rawget_type_function_works_w_queried_key_absent")
{
    if (!FFlag::LuauSolverV2)
        return;

    ScopedFastFlag sff{FFlag::LuauRawGetHandlesNil, true};

    CheckResult result = check(R"(
        type MyObject = {a: string}
        type T = rawget<MyObject, "b">
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK(toString(requireTypeAlias("T")) == "nil");
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

TEST_CASE_FIXTURE(BuiltinsFixture, "setmetatable_type_function_assigns_correct_metatable")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type Identity = setmetatable<{}, { __index: {} }>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId id = requireTypeAlias("Identity");
    CHECK_EQ(toString(id, {true}), "{ @metatable { __index: {  } }, {  } }");
    const MetatableType* mt = get<MetatableType>(id);
    REQUIRE(mt);
    CHECK_EQ(toString(mt->metatable), "{ __index: {  } }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "setmetatable_type_function_assigns_correct_metatable_2")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type Identity = setmetatable<{}, { __index: {} }>
        type FooBar = setmetatable<{}, Identity>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    TypeId id = requireTypeAlias("Identity");
    CHECK_EQ(toString(id, {true}), "{ @metatable { __index: {  } }, {  } }");
    const MetatableType* mt = get<MetatableType>(id);
    REQUIRE(mt);
    CHECK_EQ(toString(mt->metatable), "{ __index: {  } }");

    TypeId foobar = requireTypeAlias("FooBar");
    const MetatableType* mt2 = get<MetatableType>(foobar);
    REQUIRE(mt2);
    CHECK_EQ(toString(mt2->metatable, {true}), "{ @metatable { __index: {  } }, {  } }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "setmetatable_type_function_errors_on_metatable_with_metatable_metamethod")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type Identity = setmetatable<{}, { __metatable: "blocked" }>
        type Bad = setmetatable<Identity, { __index: {} }>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    TypeId id = requireTypeAlias("Identity");
    CHECK_EQ(toString(id, {true}), "{ @metatable { __metatable: \"blocked\" }, {  } }");
    const MetatableType* mt = get<MetatableType>(id);
    REQUIRE(mt);
    CHECK_EQ(toString(mt->metatable), "{ __metatable: \"blocked\" }");
}


TEST_CASE_FIXTURE(BuiltinsFixture, "setmetatable_type_function_errors_on_invalid_set")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type Identity = setmetatable<string, {}>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "setmetatable_type_function_errors_on_nontable_metatable")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type Identity = setmetatable<{}, string>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "getmetatable_type_function_returns_nil_if_no_metatable")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type TableWithNoMetatable = getmetatable<{}>
        type NumberWithNoMetatable = getmetatable<number>
        type BooleanWithNoMetatable = getmetatable<boolean>
        type BooleanLiteralWithNoMetatable = getmetatable<true>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    auto tableResult = requireTypeAlias("TableWithNoMetatable");
    CHECK_EQ(toString(tableResult), "nil");

    auto numberResult = requireTypeAlias("NumberWithNoMetatable");
    CHECK_EQ(toString(numberResult), "nil");

    auto booleanResult = requireTypeAlias("BooleanWithNoMetatable");
    CHECK_EQ(toString(booleanResult), "nil");

    auto booleanLiteralResult = requireTypeAlias("BooleanLiteralWithNoMetatable");
    CHECK_EQ(toString(booleanLiteralResult), "nil");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "getmetatable_returns_correct_metatable")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local metatable = { __index = { w = 4 } }
        local obj = setmetatable({x = 1, y = 2, z = 3}, metatable)
        type Metatable = getmetatable<typeof(obj)>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireTypeAlias("Metatable"), {true}), "{ __index: { w: number } }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "getmetatable_returns_correct_metatable_for_union")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type Identity = setmetatable<{}, {}>
        type Metatable = getmetatable<string | Identity>
        type IntersectMetatable = getmetatable<string & Identity>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const PrimitiveType* stringType = get<PrimitiveType>(getBuiltins()->stringType);
    REQUIRE(stringType->metatable);

    TypeArena arena = TypeArena{};

    std::string expected1 = toString(arena.addType(UnionType{{*stringType->metatable, getBuiltins()->emptyTableType}}), {true});
    CHECK_EQ(toString(requireTypeAlias("Metatable"), {true}), expected1);

    std::string expected2 = toString(arena.addType(IntersectionType{{*stringType->metatable, getBuiltins()->emptyTableType}}), {true});
    CHECK_EQ(toString(requireTypeAlias("IntersectMetatable"), {true}), expected2);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "getmetatable_returns_correct_metatable_for_string")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type Metatable = getmetatable<string>
        type Metatable2 = getmetatable<"foo">
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    const PrimitiveType* stringType = get<PrimitiveType>(getBuiltins()->stringType);
    REQUIRE(stringType->metatable);

    std::string expected = toString(*stringType->metatable, {true});

    CHECK_EQ(toString(requireTypeAlias("Metatable"), {true}), expected);
    CHECK_EQ(toString(requireTypeAlias("Metatable2"), {true}), expected);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "getmetatable_respects_metatable_metamethod")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local metatable = { __metatable = "Test" }
        local obj = setmetatable({x = 1, y = 2, z = 3}, metatable)
        type Metatable = getmetatable<typeof(obj)>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ(toString(requireTypeAlias("Metatable")), "string");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_function_correct_cycle_check")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
type foo<T> = { a: add<T, T>, b : add<T, T> }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "len_typefun_on_metatable")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
local t = setmetatable({}, { __mode = "v" })

local function f()
    table.insert(t, {})
    print(#t * 100)
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "has_prop_on_irreducible_type_function")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
local test = "a" + "b"
print(test.a)
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(
        "Operator '+' could not be applied to operands of types string and string; there is no corresponding overload for __add" ==
        toString(result.errors[0])
    );
    CHECK("Type 'add<string, string>' does not have key 'a'" == toString(result.errors[1]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "error_suppression_should_work_on_type_functions")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local Colours = {
            Red = 1,
            Blue = 2,
            Green = 3,
            Taupe = 4,
        }

        -- namespace mixup here, Colours isn't a type, it's a normal identifier
        export type Colour = keyof<Colours>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK("Unknown type 'Colours'" == toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fully_dispatch_type_function_that_is_parameterized_on_a_stuck_type_function")
{
    // In this test, we infer
    //
    // (c + d) : add<add<nil, nil>, *error-type*>
    //
    // This type function is stuck because it is parameterized on a stuck type
    // function.  The call constraint must be able to dispatch.

    CheckResult result = check(R"(
        --!strict

        local function f()
            local a
            local b

            local c = a + b

            print(c + d)
        end
    )");

    LUAU_REQUIRE_ERRORS(result);
    LUAU_CHECK_NO_ERROR(result, ConstraintSolvingIncompleteError);

    CHECK("() -> ()" == toString(requireType("f")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "undefined_add_application")
{
    ScopedFastFlag sff[] = {
        {FFlag::LuauSolverV2, true},
    };

    CheckResult result = check(R"(
        function add<A, B>(a: A, b: B): add<A, B>
            return a + b
        end

        local s = add(5, "hello")
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    LUAU_CHECK_ERROR(result, UninhabitedTypeFunction);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "keyof_should_not_assert_on_empty_string_props")
{
    if (!FFlag::LuauSolverV2)
        return;

    loadDefinition(R"(
        declare class Foobar
            one: boolean
            [""]: number
        end
    )");

    CheckResult results = check(R"(
        export type FoobarKeys = keyof<Foobar>;
        export type TableKeys = keyof<{ [""]: string, two: boolean }>
    )");

    LUAU_REQUIRE_NO_ERRORS(results);
    CHECK_EQ(R"("" | "one")", toString(requireTypeAlias("FoobarKeys")));
    CHECK_EQ(R"("" | "two")", toString(requireTypeAlias("TableKeys")));
}

struct TFFixture
{
    TypeArena arena_;
    NotNull<TypeArena> arena{&arena_};
    BuiltinTypes builtinTypes_;
    NotNull<BuiltinTypes> getBuiltins()
    {
        return NotNull{&builtinTypes_};
    }

    NotNull<BuiltinTypeFunctions> getBuiltinTypeFunctions()
    {
        return FFlag::LuauBuiltinTypeFunctionsArentGlobal ? NotNull{builtinTypes_.typeFunctions.get()} : NotNull{&builtinTypeFunctions};
    }

    ScopePtr globalScope = std::make_shared<Scope>(getBuiltins()->anyTypePack);

    InternalErrorReporter ice;
    UnifierSharedState unifierState{&ice};
    SimplifierPtr simplifier = EqSatSimplification::newSimplifier(arena, getBuiltins());
    Normalizer normalizer{arena, getBuiltins(), NotNull{&unifierState}, SolverMode::New};
    TypeCheckLimits limits;
    TypeFunctionRuntime runtime{NotNull{&ice}, NotNull{&limits}};

    BuiltinTypeFunctions builtinTypeFunctions;

    TypeFunctionContext tfc_{
        arena,
        getBuiltins(),
        NotNull{globalScope.get()},
        NotNull{simplifier.get()},
        NotNull{&normalizer},
        NotNull{&runtime},
        NotNull{&ice},
        NotNull{&limits}
    };

    NotNull<TypeFunctionContext> tfc{&tfc_};
};

TEST_CASE_FIXTURE(TFFixture, "refine<G, ~(false?)>")
{
    TypeId g = arena->addType(GenericType{globalScope.get(), Polarity::Negative});

    TypeId refineTy = arena->addType(TypeFunctionInstanceType{getBuiltinTypeFunctions()->refineFunc, {g, getBuiltins()->truthyType}});

    FunctionGraphReductionResult res = reduceTypeFunctions(refineTy, Location{}, tfc);

    CHECK(res.reducedTypes.size() == 1);

    CHECK(res.errors.size() == 0);
    CHECK(res.irreducibleTypes.size() == 0);
    CHECK(res.blockedTypes.size() == 0);
}

TEST_CASE_FIXTURE(TFFixture, "or<'a, 'b>")
{
    TypeId aType = arena->freshType(getBuiltins(), globalScope.get());
    TypeId bType = arena->freshType(getBuiltins(), globalScope.get());

    TypeId orType = arena->addType(TypeFunctionInstanceType{getBuiltinTypeFunctions()->orFunc, {aType, bType}});

    FunctionGraphReductionResult res = reduceTypeFunctions(orType, Location{}, tfc);

    CHECK(res.reducedTypes.size() == 1);
}

TEST_CASE_FIXTURE(TFFixture, "a_type_function_parameterized_on_generics_is_solved")
{
    TypeId a = arena->addType(GenericType{"A"});
    TypeId b = arena->addType(GenericType{"B"});

    TypeId addTy = arena->addType(TypeFunctionInstanceType{getBuiltinTypeFunctions()->addFunc, {a, b}});

    reduceTypeFunctions(addTy, Location{}, tfc);

    const auto tfit = get<TypeFunctionInstanceType>(addTy);
    REQUIRE(tfit);

    CHECK(tfit->state == TypeFunctionInstanceState::Solved);
}

TEST_CASE_FIXTURE(TFFixture, "a_tf_parameterized_on_a_solved_tf_is_solved")
{
    TypeId a = arena->addType(GenericType{"A"});
    TypeId b = arena->addType(GenericType{"B"});

    TypeId innerAddTy = arena->addType(TypeFunctionInstanceType{getBuiltinTypeFunctions()->addFunc, {a, b}});

    TypeId outerAddTy = arena->addType(TypeFunctionInstanceType{getBuiltinTypeFunctions()->addFunc, {builtinTypes_.numberType, innerAddTy}});

    reduceTypeFunctions(outerAddTy, Location{}, tfc);

    const auto tfit = get<TypeFunctionInstanceType>(outerAddTy);
    REQUIRE(tfit);

    CHECK(tfit->state == TypeFunctionInstanceState::Solved);
}

TEST_CASE_FIXTURE(TFFixture, "a_tf_parameterized_on_a_stuck_tf_is_stuck")
{
    TypeId innerAddTy = arena->addType(TypeFunctionInstanceType{getBuiltinTypeFunctions()->addFunc, {builtinTypes_.bufferType, builtinTypes_.booleanType}});

    TypeId outerAddTy = arena->addType(TypeFunctionInstanceType{getBuiltinTypeFunctions()->addFunc, {builtinTypes_.numberType, innerAddTy}});

    reduceTypeFunctions(outerAddTy, Location{}, tfc);

    const auto tfit = get<TypeFunctionInstanceType>(outerAddTy);
    REQUIRE(tfit);

    CHECK(tfit->state == TypeFunctionInstanceState::Stuck);
}

// We want to make sure that `t1 where t1 = refine<t1, unknown>` becomes `unknown`, not a cyclic type.
TEST_CASE_FIXTURE(TFFixture, "reduce_degenerate_refinement")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauRefineOccursCheckDirectRecursion, true},
    };

    TypeId root = arena->addType(BlockedType{});
    TypeId refinement = arena->addType(
        TypeFunctionInstanceType{
            getBuiltinTypeFunctions()->refineFunc,
            {
                root,
                builtinTypes_.unknownType,
            }
        }
    );

    emplaceType<BoundType>(asMutable(root), refinement);
    reduceTypeFunctions(refinement, Location{}, tfc, true);
    CHECK_EQ("unknown", toString(refinement));
}

TEST_CASE_FIXTURE(Fixture, "generic_type_functions_should_not_get_stuck_or")
{
    ScopedFastFlag sffs[] = {{FFlag::LuauSolverV2, true}, {FFlag::LuauNoMoreComparisonTypeFunctions, false}};

    CheckResult result = check(R"(
        local function init(data)
          return not data or data == ''
        end
    )");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(get<ExplicitFunctionAnnotationRecommended>(result.errors[0]));
}

TEST_CASE_FIXTURE(TypeFunctionFixture, "recursive_restraint_violation")
{
    ScopedFastFlag _ = {FFlag::LuauNameConstraintRestrictRecursiveTypes, true};

    CheckResult result = check(R"(
        type a<T> = {a<{T}>}
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(get<RecursiveRestraintViolation>(result.errors[0]));
}

TEST_CASE_FIXTURE(TypeFunctionFixture, "recursive_restraint_violation1")
{
    ScopedFastFlag _ = {FFlag::LuauNameConstraintRestrictRecursiveTypes, true};

    CheckResult result = check(R"(
        type b<T> = {b<T | string>}
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(get<RecursiveRestraintViolation>(result.errors[0]));
}

TEST_CASE_FIXTURE(TypeFunctionFixture, "recursive_restraint_violation2")
{
    ScopedFastFlag _ = {FFlag::LuauNameConstraintRestrictRecursiveTypes, true};

    CheckResult result = check(R"(
        type c<T> = {c<T & string>}
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(get<RecursiveRestraintViolation>(result.errors[0]));
}

TEST_CASE_FIXTURE(TypeFunctionFixture, "recursive_restraint_violation3")
{
    ScopedFastFlag _ = {FFlag::LuauNameConstraintRestrictRecursiveTypes, true};

    CheckResult result = check(R"(
        type d<T> = (d<T | string>) -> ()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(get<RecursiveRestraintViolation>(result.errors[0]));
}

TEST_SUITE_END();
