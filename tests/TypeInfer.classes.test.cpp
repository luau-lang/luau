// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"
#include "ClassFixture.h"

#include "ScopedFlags.h"
#include "doctest.h"

using namespace Luau;
using std::nullopt;

LUAU_FASTFLAG(LuauSolverV2);

TEST_SUITE_BEGIN("TypeInferClasses");

TEST_CASE_FIXTURE(ClassFixture, "Luau.Analyze.CLI_crashes_on_this_test")
{
    CheckResult result = check(R"(
        local CircularQueue = {}
CircularQueue.__index = CircularQueue

function CircularQueue:new()
	local newCircularQueue = {
		head = nil,
	}
	setmetatable(newCircularQueue, CircularQueue)

	return newCircularQueue
end

function CircularQueue:push()
	local newListNode

	if self.head then
		newListNode = {
			prevNode = self.head.prevNode,
			nextNode = self.head,
		}
		newListNode.prevNode.nextNode = newListNode
		newListNode.nextNode.prevNode = newListNode
	end
end

return CircularQueue

    )");
}

TEST_CASE_FIXTURE(ClassFixture, "call_method_of_a_class")
{
    CheckResult result = check(R"(
        local m = BaseClass.StaticMethod()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    REQUIRE_EQ("number", toString(requireType("m")));
}

TEST_CASE_FIXTURE(ClassFixture, "call_method_of_a_child_class")
{
    CheckResult result = check(R"(
        local m = ChildClass.StaticMethod()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    REQUIRE_EQ("number", toString(requireType("m")));
}

TEST_CASE_FIXTURE(ClassFixture, "call_instance_method")
{
    CheckResult result = check(R"(
        local i = ChildClass.New()
        local result = i:Method()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("string", toString(requireType("result")));
}

TEST_CASE_FIXTURE(ClassFixture, "call_base_method")
{
    CheckResult result = check(R"(
        local i = ChildClass.New()
        i:BaseMethod(41)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "cannot_call_unknown_method_of_a_class")
{
    CheckResult result = check(R"(
        local m = BaseClass.Nope()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(ClassFixture, "cannot_call_method_of_child_on_base_instance")
{
    CheckResult result = check(R"(
        local i = BaseClass.New()
        i:Method()
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(ClassFixture, "we_can_infer_that_a_parameter_must_be_a_particular_class")
{
    CheckResult result = check(R"(
        function makeClone(o)
            return BaseClass.Clone(o)
        end

        local a = makeClone(ChildClass.New())
    )");

    CHECK_EQ("BaseClass", toString(requireType("a")));
}

TEST_CASE_FIXTURE(ClassFixture, "we_can_report_when_someone_is_trying_to_use_a_table_rather_than_a_class")
{
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        function makeClone(o)
            return BaseClass.Clone(o)
        end

        type Oopsies = { BaseMethod: (Oopsies, number) -> ()}

        local oopsies: Oopsies = {
            BaseMethod = function (self: Oopsies, i: number)
                print('gadzooks!')
            end
        }

        makeClone(oopsies)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors.at(0));
    REQUIRE(tm != nullptr);

    CHECK_EQ("Oopsies", toString(tm->givenType));
    CHECK_EQ("BaseClass", toString(tm->wantedType));
}

TEST_CASE_FIXTURE(ClassFixture, "we_can_report_when_someone_is_trying_to_use_a_table_rather_than_a_class_using_new_solver")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        function makeClone(o)
            return BaseClass.Clone(o)
        end

        type Oopsies = { read BaseMethod: (Oopsies, number) -> ()}

        local oopsies: Oopsies = {
            BaseMethod = function (self: Oopsies, i: number)
                print('gadzooks!')
            end
        }

        makeClone(oopsies)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors.at(0));
    REQUIRE(tm != nullptr);

    CHECK_EQ("Oopsies", toString(tm->givenType));
    CHECK_EQ("BaseClass", toString(tm->wantedType));
}

TEST_CASE_FIXTURE(ClassFixture, "assign_to_prop_of_class")
{
    CheckResult result = check(R"(
        local v = Vector2.New(0, 5)
        v.X = 55
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "can_read_prop_of_base_class")
{
    CheckResult result = check(R"(
        local c = ChildClass.New()
        local x = 1 + c.BaseField
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "can_assign_to_prop_of_base_class")
{
    CheckResult result = check(R"(
        local c = ChildClass.New()
        c.BaseField = 444
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "can_read_prop_of_base_class_using_string")
{
    CheckResult result = check(R"(
        local c = ChildClass.New()
        local x = 1 + c["BaseField"]
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "can_assign_to_prop_of_base_class_using_string")
{
    CheckResult result = check(R"(
        local c = ChildClass.New()
        c["BaseField"] = 444
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "cannot_unify_class_instance_with_primitive")
{
    // This is allowed in the new solver
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
        local v = Vector2.New(0, 5)
        v = 444
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(ClassFixture, "warn_when_prop_almost_matches")
{
    CheckResult result = check(R"(
        Vector2.new(0, 0)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto err = get<UnknownPropButFoundLikeProp>(result.errors.at(0));
    REQUIRE(err != nullptr);

    REQUIRE_EQ(1, err->candidates.size());
    CHECK_EQ("New", *err->candidates.begin());
}

TEST_CASE_FIXTURE(ClassFixture, "classes_can_have_overloaded_operators")
{
    CheckResult result = check(R"(
        local a = Vector2.New(1, 2)
        local b = Vector2.New(3, 4)
        local c = a + b
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK_EQ("Vector2", toString(requireType("c")));
}

TEST_CASE_FIXTURE(ClassFixture, "classes_without_overloaded_operators_cannot_be_added")
{
    CheckResult result = check(R"(
        local a = BaseClass.New()
        local b = BaseClass.New()
        local c = a + b
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(ClassFixture, "function_arguments_are_covariant")
{
    CheckResult result = check(R"(
        function f(b: BaseClass) end

        f(ChildClass.New())
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "higher_order_function_arguments_are_contravariant")
{
    CheckResult result = check(R"(
        function apply(f: (BaseClass) -> ())
            f(ChildClass.New()) -- 2
        end

        apply(function (c: ChildClass) end) -- 5
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(ClassFixture, "higher_order_function_return_values_are_covariant")
{
    CheckResult result = check(R"(
        function apply(f: () -> BaseClass)
            return f()
        end

        apply(function ()
            return ChildClass.New()
        end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "higher_order_function_return_type_is_not_contravariant")
{
    CheckResult result = check(R"(
        function apply(f: () -> BaseClass)
            return f()
        end

        apply(function ()
            return ChildClass.New()
        end)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "table_properties_are_invariant")
{
    CheckResult result = check(R"(
        function f(a: {foo: BaseClass})
            a.foo = AnotherChild.New()
        end

        local t: {foo: ChildClass}
        f(t) -- line 6.  Breaks soundness.

        function g(t: {foo: ChildClass})
        end

        local t2: {foo: BaseClass} = {foo=BaseClass.New()}
        t2.foo = AnotherChild.New()
        g(t2) -- line 13.  Breaks soundness
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ(6, result.errors.at(0).location.begin.line);
    CHECK_EQ(13, result.errors[1].location.begin.line);
}

TEST_CASE_FIXTURE(ClassFixture, "table_indexers_are_invariant")
{
    CheckResult result = check(R"(
        function f(a: {[number]: BaseClass})
            a[1] = AnotherChild.New()
        end

        local t: {[number]: ChildClass}
        f(t) -- line 6.  Breaks soundness.

        function g(t: {[number]: ChildClass})
        end

        local t2: {[number]: BaseClass} = {BaseClass.New()}
        t2[1] = AnotherChild.New()
        g(t2) -- line 13.  Breaks soundness
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK_EQ(6, result.errors.at(0).location.begin.line);
    CHECK_EQ(13, result.errors[1].location.begin.line);
}

TEST_CASE_FIXTURE(ClassFixture, "table_class_unification_reports_sane_errors_for_missing_properties")
{
    CheckResult result = check(R"(
        function foo(bar)
            bar.Y = 1 -- valid
            bar.x = 2 -- invalid, wanted 'X'
            bar.w = 2 -- invalid
        end

        local a: Vector2
        foo(a)
    )");

    if (FFlag::LuauSolverV2)
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK("Type 'Vector2' could not be converted into '{ Y: number, w: number, x: number }'" == toString(result.errors[0]));
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        REQUIRE_EQ("Key 'w' not found in class 'Vector2'", toString(result.errors.at(0)));
        REQUIRE_EQ("Key 'x' not found in class 'Vector2'.  Did you mean 'X'?", toString(result.errors[1]));
    }
}

TEST_CASE_FIXTURE(ClassFixture, "class_unification_type_mismatch_is_correct_order")
{
    CheckResult result = check(R"(
        local p: BaseClass
        local foo: number = p
        local foo2: BaseClass = 1
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);

    REQUIRE_EQ("Type 'BaseClass' could not be converted into 'number'", toString(result.errors.at(0)));
    REQUIRE_EQ("Type 'number' could not be converted into 'BaseClass'", toString(result.errors[1]));
}

TEST_CASE_FIXTURE(ClassFixture, "optional_class_field_access_error")
{
    CheckResult result = check(R"(
local b: Vector2? = nil
local a = b.X + b.Z

b.X = 2 -- real Vector2.X is also read-only
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK_EQ("Value of type 'Vector2?' could be nil", toString(result.errors.at(0)));
    CHECK_EQ("Value of type 'Vector2?' could be nil", toString(result.errors[1]));
    CHECK_EQ("Key 'Z' not found in class 'Vector2'", toString(result.errors[2]));
    CHECK_EQ("Value of type 'Vector2?' could be nil", toString(result.errors[3]));
}

TEST_CASE_FIXTURE(ClassFixture, "detailed_class_unification_error")
{
    CheckResult result = check(R"(
local function foo(v)
    return v.X :: number + string.len(v.Y)
end

local a: Vector2
local b = foo
b(a)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    if (FFlag::LuauSolverV2)
    {
        CHECK("Type 'number' could not be converted into 'string'" == toString(result.errors.at(0)));
    }
    else
    {
        const std::string expected = R"(Type 'Vector2' could not be converted into '{- X: number, Y: string -}'
caused by:
  Property 'Y' is not compatible.
Type 'number' could not be converted into 'string')";

        CHECK_EQ(expected, toString(result.errors.at(0)));
    }
}

TEST_CASE_FIXTURE(ClassFixture, "class_type_mismatch_with_name_conflict")
{
    // CLI-116433
    DOES_NOT_PASS_NEW_SOLVER_GUARD();

    CheckResult result = check(R"(
local i = ChildClass.New()
type ChildClass = { x: number }
local a: ChildClass = i
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type 'ChildClass' from 'Test' could not be converted into 'ChildClass' from 'MainModule'", toString(result.errors.at(0)));
}

TEST_CASE_FIXTURE(ClassFixture, "intersections_of_unions_of_classes")
{
    CheckResult result = check(R"(
        local x : (BaseClass | Vector2) & (ChildClass | AnotherChild)
        local y : (ChildClass | AnotherChild)
        x = y
        y = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "unions_of_intersections_of_classes")
{
    CheckResult result = check(R"(
        local x : (BaseClass & ChildClass) | (BaseClass & AnotherChild) | (BaseClass & Vector2)
        local y : (ChildClass | AnotherChild)
        x = y
        y = x
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "index_instance_property")
{
    CheckResult result = check(R"(
        local function execute(object: BaseClass, name: string)
            print(object[name])
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Attempting a dynamic property access on type 'BaseClass' is unsafe and may cause exceptions at runtime", toString(result.errors.at(0)));
}

TEST_CASE_FIXTURE(ClassFixture, "index_instance_property_nonstrict")
{
    CheckResult result = check(R"(
        --!nonstrict

        local function execute(object: BaseClass, name: string)
            print(object[name])
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "type_mismatch_invariance_required_for_error")
{
    CheckResult result = check(R"(
type A = { x: ChildClass }
type B = { x: BaseClass }

local a: A = { x = ChildClass.New() }
local b: B = a
    )");

    LUAU_REQUIRE_ERRORS(result);

    if (FFlag::LuauSolverV2)
        CHECK(toString(result.errors.at(0)) == "Type 'A' could not be converted into 'B'; at [read \"x\"], ChildClass is not exactly BaseClass");
    else
    {
        const std::string expected = R"(Type 'A' could not be converted into 'B'
caused by:
  Property 'x' is not compatible.
Type 'ChildClass' could not be converted into 'BaseClass' in an invariant context)";
        CHECK_EQ(expected, toString(result.errors.at(0)));
    }
}

TEST_CASE_FIXTURE(ClassFixture, "optional_class_casts_work_in_new_solver")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type A = { x: ChildClass }
        type B = { x: BaseClass }

        local a = { x = ChildClass.New() } :: A
        local opt_a = a :: A?
        local b = { x = BaseClass.New() } :: B
        local opt_b = b :: B?
        local b_from_a = a :: B
        local b_from_opt_a = opt_a :: B
        local opt_b_from_a = a :: B?
        local opt_b_from_opt_a = opt_a :: B?
        local a_from_b = b :: A
        local a_from_opt_b = opt_b :: A
        local opt_a_from_b = b :: A?
        local opt_a_from_opt_b = opt_b :: A?
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "callable_classes")
{
    CheckResult result = check(R"(
        local x : CallableClass
        local y = x("testing")
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
    CHECK_EQ("number", toString(requireType("y")));
}

TEST_CASE_FIXTURE(ClassFixture, "indexable_classes")
{
    // Test reading from an index
    {
        CheckResult result = check(R"(
            local x : IndexableClass
            local y = x.stringKey
        )");
        LUAU_REQUIRE_NO_ERRORS(result);
    }
    {
        CheckResult result = check(R"(
            local x : IndexableClass
            local y = x["stringKey"]
        )");
        LUAU_REQUIRE_NO_ERRORS(result);
    }
    {
        CheckResult result = check(R"(
            local x : IndexableClass
            local str : string
            local y = x[str]            -- Index with a non-const string
        )");
        LUAU_REQUIRE_NO_ERRORS(result);
    }
    {
        CheckResult result = check(R"(
            local x : IndexableClass
            local y = x[7]              -- Index with a numeric key
        )");
        LUAU_REQUIRE_NO_ERRORS(result);
    }

    // Test writing to an index
    {
        CheckResult result = check(R"(
            local x : IndexableClass
            x.stringKey = 42
        )");
        LUAU_REQUIRE_NO_ERRORS(result);
    }
    {
        CheckResult result = check(R"(
            local x : IndexableClass
            x["stringKey"] = 42
        )");
        LUAU_REQUIRE_NO_ERRORS(result);
    }
    {
        CheckResult result = check(R"(
            local x : IndexableClass
            local str : string
            x[str] = 42                 -- Index with a non-const string
        )");
        LUAU_REQUIRE_NO_ERRORS(result);
    }
    {
        CheckResult result = check(R"(
            local x : IndexableClass
            x[1] = 42                   -- Index with a numeric key
        )");
        LUAU_REQUIRE_NO_ERRORS(result);
    }

    // Try to index the class using an invalid type for the key (key type is 'number | string'.)
    {
        CheckResult result = check(R"(
            local x : IndexableClass
            local y = x[true]
        )");

        if (FFlag::LuauSolverV2)
            CHECK(
                "Type 'boolean' could not be converted into 'number | string'" == toString(result.errors.at(0))
            );
        else
            CHECK_EQ(
                toString(result.errors.at(0)), "Type 'boolean' could not be converted into 'number | string'; none of the union options are compatible"
            );
    }
    {
        CheckResult result = check(R"(
            local x : IndexableClass
            x[true] = 42
        )");

        if (FFlag::LuauSolverV2)
            CHECK(
                "Type 'boolean' could not be converted into 'number | string'" == toString(result.errors.at(0))
            );
        else
            CHECK_EQ(
                toString(result.errors.at(0)), "Type 'boolean' could not be converted into 'number | string'; none of the union options are compatible"
            );
    }

    // Test type checking for the return type of the indexer (i.e. a number)
    {
        CheckResult result = check(R"(
            local x : IndexableClass
            x.key = "string value"
        )");

        if (FFlag::LuauSolverV2)
        {
            // Disabled for now.  CLI-115686
        }
        else
            CHECK_EQ(toString(result.errors.at(0)), "Type 'string' could not be converted into 'number'");
    }
    {
        CheckResult result = check(R"(
            local x : IndexableClass
            local str : string = x.key
        )");
        CHECK_EQ(toString(result.errors.at(0)), "Type 'number' could not be converted into 'string'");
    }

    // Check that we string key are rejected if the indexer's key type is not compatible with string
    {
        CheckResult result = check(R"(
            local x : IndexableNumericKeyClass
            x.key = 1
        )");
        CHECK_EQ(toString(result.errors.at(0)), "Key 'key' not found in class 'IndexableNumericKeyClass'");
    }
    {
        CheckResult result = check(R"(
            local x : IndexableNumericKeyClass
            x["key"] = 1
        )");
        if (FFlag::LuauSolverV2)
            CHECK_EQ(toString(result.errors.at(0)), "Key 'key' not found in class 'IndexableNumericKeyClass'");
        else
            CHECK_EQ(toString(result.errors.at(0)), "Type 'string' could not be converted into 'number'");
    }
    {
        CheckResult result = check(R"(
            local x : IndexableNumericKeyClass
            local str : string
            x[str] = 1                  -- Index with a non-const string
        )");
        CHECK_EQ(toString(result.errors.at(0)), "Type 'string' could not be converted into 'number'");
    }
    {
        CheckResult result = check(R"(
            local x : IndexableNumericKeyClass
            local y = x.key
        )");
        CHECK_EQ(toString(result.errors.at(0)), "Key 'key' not found in class 'IndexableNumericKeyClass'");
    }
    {
        CheckResult result = check(R"(
            local x : IndexableNumericKeyClass
            local y = x["key"]
        )");
        if (FFlag::LuauSolverV2)
            CHECK(toString(result.errors.at(0)) == "Key 'key' not found in class 'IndexableNumericKeyClass'");
        else
            CHECK_EQ(toString(result.errors.at(0)), "Type 'string' could not be converted into 'number'");
    }
    {
        CheckResult result = check(R"(
            local x : IndexableNumericKeyClass
            local str : string
            local y = x[str]            -- Index with a non-const string
        )");
        CHECK_EQ(toString(result.errors.at(0)), "Type 'string' could not be converted into 'number'");
    }
}

TEST_CASE_FIXTURE(Fixture, "read_write_class_properties")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    TypeArena& arena = frontend.globals.globalTypes;

    unfreeze(arena);

    TypeId instanceType = arena.addType(ClassType{"Instance", {}, nullopt, nullopt, {}, {}, "Test", {}});
    getMutable<ClassType>(instanceType)->props = {{"Parent", Property::rw(instanceType)}};

    //

    TypeId workspaceType = arena.addType(ClassType{"Workspace", {}, nullopt, nullopt, {}, {}, "Test", {}});

    TypeId scriptType =
        arena.addType(ClassType{"Script", {{"Parent", Property::rw(workspaceType, instanceType)}}, instanceType, nullopt, {}, {}, "Test", {}});

    TypeId partType = arena.addType(ClassType{
        "Part",
        {{"BrickColor", Property::rw(builtinTypes->stringType)}, {"Parent", Property::rw(workspaceType, instanceType)}},
        instanceType,
        nullopt,
        {},
        {},
        "Test",
        {}
    });

    getMutable<ClassType>(workspaceType)->props = {{"Script", Property::readonly(scriptType)}, {"Part", Property::readonly(partType)}};

    frontend.globals.globalScope->bindings[frontend.globals.globalNames.names->getOrAdd("script")] = Binding{scriptType};

    freeze(arena);

    CheckResult result = check(R"(
        script.Parent.Part.BrickColor = 0xFFFFFF
        script.Parent.Part.Parent = script
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK(Location{{1, 40}, {1, 48}} == result.errors[0].location);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(builtinTypes->stringType == tm->wantedType);
    CHECK(builtinTypes->numberType == tm->givenType);
}

TEST_CASE_FIXTURE(ClassFixture, "cannot_index_a_class_with_no_indexer")
{
    CheckResult result = check(R"(
        local a = BaseClass.New()

        local c = a[1]
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_MESSAGE(
        get<DynamicPropertyLookupOnClassesUnsafe>(result.errors[0]), "Expected DynamicPropertyLookupOnClassesUnsafe but got " << result.errors[0]
    );

    CHECK(builtinTypes->errorType == requireType("c"));
}

TEST_CASE_FIXTURE(ClassFixture, "cyclic_tables_are_assumed_to_be_compatible_with_classes")
{
    /*
     * This is technically documenting a case where we are intentionally
     * unsound.
     *
     * Our builtins are essentially defined like so:
     *
     * declare class BaseClass
     *     BaseField: number
     *     function BaseMethod(self, number): ()
     *     read Touched: Connection
     * end
     *
     * declare class Connection
     *     Connect: (Connection, (BaseClass) -> ()) -> ()
     * end
     *
     * The type we infer for `onTouch` is
     *
     * (t1) -> () where t1 = { read BaseField: unknown, read BaseMethod: (t1, number) -> () }
     *
     * In order to validate that onTouch can be passed to Connect, we must
     * verify the following relation:
     *
     * BaseClass <: t1 where t1 = { read BaseField: unknown, read BaseMethod: (t1, number) -> () }
     *
     * However, the cycle between the table and the function gums up the works
     * here and the worst thing is that it's perfectly reasonable in principle.
     * Just from these types, we cannot see that BaseMethod will only be passed
     * t1.  Without that guarantee, BaseClass cannot be used as a subtype of t1.
     *
     * I think the theoretically-correct way to untangle this would be to infer
     * t1 as a bounded existential type.
     *
     * For now, we have a subtyping has a rule that provisionally substitutes
     * the table for the class type when performing the subtyping test.  We
     * essentially assume that, for all cyclic functions, that the table and the
     * class are mutually subtypes of one another.
     *
     * For more information, read uses of Subtyping::substitutions.
     */

    CheckResult result = check(R"(
        local c = BaseClass.New()

        function requiresNothing() end

        function onTouch(other)
            requiresNothing(other:BaseMethod(0))
            print(other.BaseField)
        end

        c.Touched:Connect(onTouch)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
