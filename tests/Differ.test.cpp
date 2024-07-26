// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/Differ.h"
#include "Luau/Common.h"
#include "Luau/Error.h"
#include "Luau/Frontend.h"

#include "Fixture.h"
#include "ClassFixture.h"

#include "Luau/Symbol.h"
#include "Luau/Type.h"
#include "ScopedFlags.h"
#include "doctest.h"
#include <iostream>

using namespace Luau;

LUAU_FASTFLAG(DebugLuauDeferredConstraintResolution)

TEST_SUITE_BEGIN("Differ");

TEST_CASE_FIXTURE(DifferFixture, "equal_numbers")
{
    CheckResult result = check(R"(
    local foo = 5
    local almostFoo = 78
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_strings")
{
    CheckResult result = check(R"(
    local foo = "hello"
    local almostFoo = "world"
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_tables")
{
    CheckResult result = check(R"(
    local foo = { x = 1, y = "where" }
    local almostFoo = { x = 5, y = "when" }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "a_table_missing_property")
{
    CheckResult result = check(R"(
    local foo = { x = 1, y = 2 }
    local almostFoo = { x = 1, z = 3 }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        "DiffError: these two types are not equal because the left type at foo.y has type number, while the right type at almostFoo is missing "
        "the property y");
}

TEST_CASE_FIXTURE(DifferFixture, "left_table_missing_property")
{
    CheckResult result = check(R"(
    local foo = { x = 1 }
    local almostFoo = { x = 1, z = 3 }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        "DiffError: these two types are not equal because the left type at foo is missing the property z, while the right type at almostFoo.z "
        "has type number");
}

TEST_CASE_FIXTURE(DifferFixture, "a_table_wrong_type")
{
    CheckResult result = check(R"(
    local foo = { x = 1, y = 2 }
    local almostFoo = { x = 1, y = "two" }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        "DiffError: these two types are not equal because the left type at foo.y has type number, while the right type at almostFoo.y has type "
        "string");
}

TEST_CASE_FIXTURE(DifferFixture, "a_table_wrong_type")
{
    CheckResult result = check(R"(
    local foo: string
    local almostFoo: number
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        "DiffError: these two types are not equal because the left type at <unlabeled-symbol> has type string, while the right type at "
        "<unlabeled-symbol> has type number");
}

TEST_CASE_FIXTURE(DifferFixture, "a_nested_table_wrong_type")
{
    CheckResult result = check(R"(
    local foo = { x = 1, inner = { table = { has = { wrong = { value = 5 } } } } }
    local almostFoo = { x = 1, inner = { table = { has = { wrong = { value = "five" } } } } }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        "DiffError: these two types are not equal because the left type at foo.inner.table.has.wrong.value has type number, while the right "
        "type at almostFoo.inner.table.has.wrong.value has type string");
}

TEST_CASE_FIXTURE(DifferFixture, "a_nested_table_wrong_match")
{
    CheckResult result = check(R"(
    local foo = { x = 1, inner = { table = { has = { wrong = { variant = { because = { it = { goes = { on = "five" } } } } } } } } }
    local almostFoo = { x = 1, inner = { table = { has = { wrong = { variant = "five" } } } } }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        "DiffError: these two types are not equal because the left type at foo.inner.table.has.wrong.variant has type { because: { it: { goes: "
        "{ on: string } } } }, while the right type at almostFoo.inner.table.has.wrong.variant has type string");
}

TEST_CASE_FIXTURE(DifferFixture, "left_cyclic_table_right_table_missing_property")
{
    CheckResult result = check(R"(
    local function id<a>(x: a): a
      return x
    end

    -- Remove name from cyclic table
    local foo = id({})
    foo.foo = foo
    local almostFoo = { x = 2 }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.foo has type t1 where t1 = { foo: t1 }, while the right type at almostFoo is missing the property foo)");
}

TEST_CASE_FIXTURE(DifferFixture, "left_cyclic_table_right_table_property_wrong")
{
    CheckResult result = check(R"(
    local function id<a>(x: a): a
      return x
    end

    -- Remove name from cyclic table
    local foo = id({})
    foo.foo = foo
    local almostFoo = { foo = 2 }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.foo has type t1 where t1 = { foo: t1 }, while the right type at almostFoo.foo has type number)");
}

TEST_CASE_FIXTURE(DifferFixture, "right_cyclic_table_left_table_missing_property")
{
    CheckResult result = check(R"(
    local function id<a>(x: a): a
      return x
    end

    -- Remove name from cyclic table
    local foo = id({})
    foo.foo = foo
    local almostFoo = { x = 2 }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("almostFoo", "foo",
        R"(DiffError: these two types are not equal because the left type at almostFoo.x has type number, while the right type at <unlabeled-symbol> is missing the property x)");
}

TEST_CASE_FIXTURE(DifferFixture, "right_cyclic_table_left_table_property_wrong")
{
    CheckResult result = check(R"(
    local function id<a>(x: a): a
      return x
    end

    -- Remove name from cyclic table
    local foo = id({})
    foo.foo = foo
    local almostFoo = { foo = 2 }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("almostFoo", "foo",
        R"(DiffError: these two types are not equal because the left type at almostFoo.foo has type number, while the right type at <unlabeled-symbol>.foo has type t1 where t1 = { foo: t1 })");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_table_two_cyclic_tables_are_not_different")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, false};

    CheckResult result = check(R"(
    local function id<a>(x: a): a
      return x
    end

    -- Remove name from cyclic table
    local foo = id({})
    foo.foo = foo
    local almostFoo = id({})
    almostFoo.foo = almostFoo
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_table_two_shifted_circles_are_not_different")
{
    CheckResult result = check(R"(
    local function id<a>(x: a): a
      return x
    end

    -- Remove name from cyclic table
    local foo = id({})
    foo.foo = id({})
    foo.foo.foo = id({})
    foo.foo.foo.foo = id({})
    foo.foo.foo.foo.foo = foo

    local builder = id({})
    builder.foo = id({})
    builder.foo.foo = id({})
    builder.foo.foo.foo = id({})
    builder.foo.foo.foo.foo = builder
    -- Shift
    local almostFoo = builder.foo.foo
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "table_left_circle_right_measuring_tape")
{
    // Left is a circle, right is a measuring tape
    CheckResult result = check(R"(
    local function id<a>(x: a): a
      return x
    end

    -- Remove name from cyclic table
    local foo = id({})
    foo.foo = id({})
    foo.foo.foo = id({})
    foo.foo.foo.foo = id({})
    foo.foo.foo.bar = id({}) -- anchor to pin shape
    foo.foo.foo.foo.foo = foo
    local almostFoo = id({})
    almostFoo.foo = id({})
    almostFoo.foo.foo = id({})
    almostFoo.foo.foo.foo = id({})
    almostFoo.foo.foo.bar = id({}) -- anchor to pin shape
    almostFoo.foo.foo.foo.foo = almostFoo.foo
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.foo.foo.foo.foo.foo is missing the property bar, while the right type at <unlabeled-symbol>.foo.foo.foo.foo.foo.bar has type {  })");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_table_measuring_tapes")
{
    CheckResult result = check(R"(
    local function id<a>(x: a): a
      return x
    end

    -- Remove name from cyclic table
    local foo = id({})
    foo.foo = id({})
    foo.foo.foo = id({})
    foo.foo.foo.foo = id({})
    foo.foo.foo.foo.foo = foo.foo
    local almostFoo = id({})
    almostFoo.foo = id({})
    almostFoo.foo.foo = id({})
    almostFoo.foo.foo.foo = id({})
    almostFoo.foo.foo.foo.foo = almostFoo.foo
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_table_A_B_C")
{
    CheckResult result = check(R"(
    local function id<a>(x: a): a
      return x
    end

    -- Remove name from cyclic table
    local foo = id({})
    foo.foo = id({})
    foo.foo.foo = id({})
    foo.foo.foo.foo = id({})
    foo.foo.foo.foo.foo = foo.foo
    local almostFoo = id({})
    almostFoo.foo = id({})
    almostFoo.foo.foo = id({})
    almostFoo.foo.foo.foo = id({})
    almostFoo.foo.foo.foo.foo = almostFoo.foo
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_table_kind_A")
{
    CheckResult result = check(R"(
    -- Remove name from cyclic table
    local function id<a>(x: a): a
      return x
    end

    local foo = id({})
    foo.left = id({})
    foo.right = id({})
    foo.left.left = id({})
    foo.left.right = id({})
    foo.right.left = id({})
    foo.right.right = id({})
    foo.right.left.left = id({})
    foo.right.left.right = id({})

    foo.right.left.left.child = foo.right

    local almostFoo = id({})
    almostFoo.left = id({})
    almostFoo.right = id({})
    almostFoo.left.left = id({})
    almostFoo.left.right = id({})
    almostFoo.right.left = id({})
    almostFoo.right.right = id({})
    almostFoo.right.left.left = id({})
    almostFoo.right.left.right = id({})

    almostFoo.right.left.left.child = almostFoo.right

    -- Bindings for requireType
    local fooLeft = foo.left
    local fooRight = foo.left.right
    local fooLeftLeft = foo.left.left
    local fooLeftRight = foo.left.right
    local fooRightLeft = foo.right.left
    local fooRightRight = foo.right.right
    local fooRightLeftLeft = foo.right.left.left
    local fooRightLeftRight = foo.right.left.right

    local almostFooLeft = almostFoo.left
    local almostFooRight = almostFoo.left.right
    local almostFooLeftLeft = almostFoo.left.left
    local almostFooLeftRight = almostFoo.left.right
    local almostFooRightLeft = almostFoo.right.left
    local almostFooRightRight = almostFoo.right.right
    local almostFooRightLeftLeft = almostFoo.right.left.left
    local almostFooRightLeftRight = almostFoo.right.left.right
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_table_kind_B")
{
    CheckResult result = check(R"(
    -- Remove name from cyclic table
    local function id<a>(x: a): a
      return x
    end

    local foo = id({})
    foo.left = id({})
    foo.right = id({})
    foo.left.left = id({})
    foo.left.right = id({})
    foo.right.left = id({})
    foo.right.right = id({})
    foo.right.left.left = id({})
    foo.right.left.right = id({})

    foo.right.left.left.child = foo.left

    local almostFoo = id({})
    almostFoo.left = id({})
    almostFoo.right = id({})
    almostFoo.left.left = id({})
    almostFoo.left.right = id({})
    almostFoo.right.left = id({})
    almostFoo.right.right = id({})
    almostFoo.right.left.left = id({})
    almostFoo.right.left.right = id({})

    almostFoo.right.left.left.child = almostFoo.left
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_table_kind_C")
{
    CheckResult result = check(R"(
    -- Remove name from cyclic table
    local function id<a>(x: a): a
      return x
    end

    local foo = id({})
    foo.left = id({})
    foo.right = id({})
    foo.left.left = id({})
    foo.left.right = id({})
    foo.right.left = id({})
    foo.right.right = id({})
    foo.right.left.left = id({})
    foo.right.left.right = id({})

    foo.right.left.left.child = foo

    local almostFoo = id({})
    almostFoo.left = id({})
    almostFoo.right = id({})
    almostFoo.left.left = id({})
    almostFoo.left.right = id({})
    almostFoo.right.left = id({})
    almostFoo.right.right = id({})
    almostFoo.right.left.left = id({})
    almostFoo.right.left.right = id({})

    almostFoo.right.left.left.child = almostFoo
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_table_kind_D")
{
    CheckResult result = check(R"(
    -- Remove name from cyclic table
    local function id<a>(x: a): a
      return x
    end

    local foo = id({})
    foo.left = id({})
    foo.right = id({})
    foo.left.left = id({})
    foo.left.right = id({})
    foo.right.left = id({})
    foo.right.right = id({})
    foo.right.left.left = id({})
    foo.right.left.right = id({})

    foo.right.left.left.child = foo.right.left.left

    local almostFoo = id({})
    almostFoo.left = id({})
    almostFoo.right = id({})
    almostFoo.left.left = id({})
    almostFoo.left.right = id({})
    almostFoo.right.left = id({})
    almostFoo.right.right = id({})
    almostFoo.right.left.left = id({})
    almostFoo.right.left.right = id({})

    almostFoo.right.left.left.child = almostFoo.right.left.left
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_table_cyclic_diamonds_unraveled")
{
    CheckResult result = check(R"(
    -- Remove name from cyclic table
    local function id<a>(x: a): a
      return x
    end

    -- Pattern 1
    local foo = id({})
    foo.child = id({})
    foo.child.left = id({})
    foo.child.right = id({})

    foo.child.left.child = foo
    foo.child.right.child = foo

    -- Pattern 2
    local almostFoo = id({})
    almostFoo.child = id({})
    almostFoo.child.left = id({})
    almostFoo.child.right = id({})

    almostFoo.child.left.child = id({}) -- Use a new table
    almostFoo.child.right.child = almostFoo.child.left.child -- Refer to the same new table

    almostFoo.child.left.child.child = id({})
    almostFoo.child.left.child.child.left = id({})
    almostFoo.child.left.child.child.right = id({})

    almostFoo.child.left.child.child.left.child = almostFoo.child.left.child
    almostFoo.child.left.child.child.right.child = almostFoo.child.left.child

    -- Pattern 3
    local anotherFoo = id({})
    anotherFoo.child = id({})
    anotherFoo.child.left = id({})
    anotherFoo.child.right = id({})

    anotherFoo.child.left.child = id({}) -- Use a new table
    anotherFoo.child.right.child = id({}) -- Use another new table

    anotherFoo.child.left.child.child = id({})
    anotherFoo.child.left.child.child.left = id({})
    anotherFoo.child.left.child.child.right = id({})
    anotherFoo.child.right.child.child = id({})
    anotherFoo.child.right.child.child.left = id({})
    anotherFoo.child.right.child.child.right = id({})

    anotherFoo.child.left.child.child.left.child = anotherFoo.child.left.child
    anotherFoo.child.left.child.child.right.child = anotherFoo.child.left.child
    anotherFoo.child.right.child.child.left.child = anotherFoo.child.right.child
    anotherFoo.child.right.child.child.right.child = anotherFoo.child.right.child

    -- Pattern 4
    local cleverFoo = id({})
    cleverFoo.child = id({})
    cleverFoo.child.left = id({})
    cleverFoo.child.right = id({})

    cleverFoo.child.left.child = id({}) -- Use a new table
    cleverFoo.child.right.child = id({}) -- Use another new table

    cleverFoo.child.left.child.child = id({})
    cleverFoo.child.left.child.child.left = id({})
    cleverFoo.child.left.child.child.right = id({})
    cleverFoo.child.right.child.child = id({})
    cleverFoo.child.right.child.child.left = id({})
    cleverFoo.child.right.child.child.right = id({})
    -- Same as pattern 3, but swapped here
    cleverFoo.child.left.child.child.left.child = cleverFoo.child.right.child -- Swap
    cleverFoo.child.left.child.child.right.child = cleverFoo.child.right.child
    cleverFoo.child.right.child.child.left.child = cleverFoo.child.left.child
    cleverFoo.child.right.child.child.right.child = cleverFoo.child.left.child

    -- Pattern 5
    local cheekyFoo = id({})
    cheekyFoo.child = id({})
    cheekyFoo.child.left = id({})
    cheekyFoo.child.right = id({})

    cheekyFoo.child.left.child = foo -- Use existing pattern
    cheekyFoo.child.right.child = foo -- Use existing pattern
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    std::vector<std::string> symbols{"foo", "almostFoo", "anotherFoo", "cleverFoo", "cheekyFoo"};

    for (auto left : symbols)
    {
        for (auto right : symbols)
        {
            compareTypesEq(left, right);
        }
    }
}

TEST_CASE_FIXTURE(DifferFixture, "equal_function_cyclic")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        function foo()
            return foo
        end
        function almostFoo()
            function bar()
                return bar
            end
            return bar
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_function_table_cyclic")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        function foo()
            return {
                bar = foo
            }
        end
        function almostFoo()
            function bar()
                return {
                    bar = bar
                }
            end
            return {
                bar = bar
            }
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "function_table_self_referential_cyclic")
{
    // Old solver does not correctly infer function typepacks
    // ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
        function foo()
            return {
                bar = foo
            }
        end
        function almostFoo()
            function bar()
                return bar
            end
            return {
                bar = bar
            }
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
        compareTypesNe("foo", "almostFoo",
            R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Ret[1].bar.Ret[1] has type t1 where t1 = { bar: () -> t1 }, while the right type at <unlabeled-symbol>.Ret[1].bar.Ret[1] has type t1 where t1 = () -> t1)");
    else
        compareTypesNe("foo", "almostFoo",
            R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Ret[1].bar.Ret[1] has type t1 where t1 = {| bar: () -> t1 |}, while the right type at <unlabeled-symbol>.Ret[1].bar.Ret[1] has type t1 where t1 = () -> t1)");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_union_cyclic")
{
    TypeArena arena;
    TypeId number = arena.addType(PrimitiveType{PrimitiveType::Number});
    TypeId string = arena.addType(PrimitiveType{PrimitiveType::String});

    TypeId foo = arena.addType(UnionType{std::vector<TypeId>{number, string}});
    UnionType* unionFoo = getMutable<UnionType>(foo);
    unionFoo->options.push_back(foo);

    TypeId almostFoo = arena.addType(UnionType{std::vector<TypeId>{number, string}});
    UnionType* unionAlmostFoo = getMutable<UnionType>(almostFoo);
    unionAlmostFoo->options.push_back(almostFoo);

    compareEq(foo, almostFoo);
}

TEST_CASE_FIXTURE(DifferFixture, "equal_intersection_cyclic")
{
    // Old solver does not correctly refine test types
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo1(x: number)
        return x
    end
    function foo2(x: string)
        return 0
    end
    function bar1(x: number)
        return x
    end
    function bar2(x: string)
        return 0
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
    TypeId foo1 = requireType("foo1");
    TypeId foo2 = requireType("foo2");
    TypeId bar1 = requireType("bar1");
    TypeId bar2 = requireType("bar2");

    TypeArena arena;

    TypeId foo = arena.addType(IntersectionType{std::vector<TypeId>{foo1, foo2}});
    IntersectionType* intersectionFoo = getMutable<IntersectionType>(foo);
    intersectionFoo->parts.push_back(foo);

    TypeId almostFoo = arena.addType(IntersectionType{std::vector<TypeId>{bar1, bar2}});
    IntersectionType* intersectionAlmostFoo = getMutable<IntersectionType>(almostFoo);
    intersectionAlmostFoo->parts.push_back(almostFoo);

    compareEq(foo, almostFoo);
}

TEST_CASE_FIXTURE(DifferFixture, "singleton")
{
    CheckResult result = check(R"(
    local foo: "hello" = "hello"
    local almostFoo: true = true
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> has type "hello", while the right type at <unlabeled-symbol> has type true)");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_singleton")
{
    CheckResult result = check(R"(
    local foo: "hello" = "hello"
    local almostFoo: "hello"
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "singleton_string")
{
    CheckResult result = check(R"(
    local foo: "hello" = "hello"
    local almostFoo: "world" = "world"
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> has type "hello", while the right type at <unlabeled-symbol> has type "world")");
}

TEST_CASE_FIXTURE(DifferFixtureWithBuiltins, "negation")
{
    if (!FFlag::DebugLuauDeferredConstraintResolution)
        return;

    CheckResult result = check(R"(
        local bar: { x: { y: unknown }}
        local almostBar: { x: { y: unknown }}

        local foo
        local almostFoo

        if typeof(bar.x.y) ~= "string" then
            foo = bar
        end

        if typeof(almostBar.x.y) ~= "number" then
            almostFoo = almostBar
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is a union containing type { x: { y: ~string } }, while the right type at <unlabeled-symbol> is a union missing type { x: { y: ~string } })");

    // TODO: a more desirable expected error here is as below, but `Differ` requires improvements to
    // dealing with unions to get something like this (recognizing that the union is identical
    // except in one component where they differ).
    //
    // compareTypesNe("foo", "almostFoo",
    //    R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.x.y.Negation has type string, while the right type at <unlabeled-symbol>.x.y.Negation has type number)");
}

TEST_CASE_FIXTURE(DifferFixture, "union_missing_right")
{
    CheckResult result = check(R"(
    local foo: string | number
    local almostFoo: boolean | string
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is a union containing type number, while the right type at <unlabeled-symbol> is a union missing type number)");
}

TEST_CASE_FIXTURE(DifferFixture, "union_missing_left")
{
    CheckResult result = check(R"(
    local foo: string | number
    local almostFoo: boolean | string | number
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is a union missing type boolean, while the right type at <unlabeled-symbol> is a union containing type boolean)");
}

TEST_CASE_FIXTURE(DifferFixture, "union_missing")
{
    // TODO: this test case produces an error message that is not the most UX-friendly

    CheckResult result = check(R"(
    local foo: { bar: number, pan: string } | { baz: boolean, rot: "singleton" }
    local almostFoo: { bar: number, pan: string } | { baz: string, rot: "singleton" }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
        compareTypesNe("foo", "almostFoo",
            R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is a union containing type { baz: boolean, rot: "singleton" }, while the right type at <unlabeled-symbol> is a union missing type { baz: boolean, rot: "singleton" })");
    else
        compareTypesNe("foo", "almostFoo",
            R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is a union containing type {| baz: boolean, rot: "singleton" |}, while the right type at <unlabeled-symbol> is a union missing type {| baz: boolean, rot: "singleton" |})");
}

TEST_CASE_FIXTURE(DifferFixture, "intersection_missing_right")
{
    CheckResult result = check(R"(
    local foo: (number) -> () & (string) -> ()
    local almostFoo: (string) -> () & (boolean) -> ()
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is an intersection containing type (number) -> (), while the right type at <unlabeled-symbol> is an intersection missing type (number) -> ())");
}

TEST_CASE_FIXTURE(DifferFixture, "intersection_missing_left")
{
    CheckResult result = check(R"(
    local foo: (number) -> () & (string) -> ()
    local almostFoo: (string) -> () & (boolean) -> () & (number) -> ()
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is an intersection missing type (boolean) -> (), while the right type at <unlabeled-symbol> is an intersection containing type (boolean) -> ())");
}

TEST_CASE_FIXTURE(DifferFixture, "intersection_tables_missing_right")
{
    CheckResult result = check(R"(
    local foo: { x: number } & { y: string }
    local almostFoo: { y: string } & { z: boolean }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
        compareTypesNe("foo", "almostFoo",
            R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is an intersection containing type { x: number }, while the right type at <unlabeled-symbol> is an intersection missing type { x: number })");
    else
        compareTypesNe("foo", "almostFoo",
            R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is an intersection containing type {| x: number |}, while the right type at <unlabeled-symbol> is an intersection missing type {| x: number |})");
}

TEST_CASE_FIXTURE(DifferFixture, "intersection_tables_missing_left")
{
    CheckResult result = check(R"(
    local foo: { x: number } & { y: string }
    local almostFoo: { y: string } & { z: boolean } & { x: number }
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    if (FFlag::DebugLuauDeferredConstraintResolution)
        compareTypesNe("foo", "almostFoo",
            R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is an intersection missing type { z: boolean }, while the right type at <unlabeled-symbol> is an intersection containing type { z: boolean })");
    else
        compareTypesNe("foo", "almostFoo",
            R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> is an intersection missing type {| z: boolean |}, while the right type at <unlabeled-symbol> is an intersection containing type {| z: boolean |})");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_function")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo(x: number)
        return x
    end
    function almostFoo(y: number)
        return y + 10
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_function_inferred_ret_length")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function bar(x: number, y: string)
        return x, y
    end
    function almostBar(a: number, b: string)
        return a, b
    end
    function foo(x: number, y: string, z: boolean)
        return z, bar(x, y)
    end
    function almostFoo(a: number, b: string, c: boolean)
        return c, almostBar(a, b)
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_function_inferred_ret_length_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function bar(x: number, y: string)
        return x, y
    end
    function foo(x: number, y: string, z: boolean)
        return bar(x, y), z
    end
    function almostFoo(a: number, b: string, c: boolean)
        return a, c
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "function_arg_normal")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo(x: number, y: number, z: number)
        return x * y * z
    end
    function almostFoo(a: number, b: number, msg: string)
        return a
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Arg[3] has type number, while the right type at <unlabeled-symbol>.Arg[3] has type string)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_arg_normal_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo(x: number, y: number, z: string)
        return x * y
    end
    function almostFoo(a: number, y: string, msg: string)
        return a
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Arg[2] has type number, while the right type at <unlabeled-symbol>.Arg[2] has type string)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_ret_normal")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo(x: number, y: number, z: string)
        return x
    end
    function almostFoo(a: number, b: number, msg: string)
        return msg
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Ret[1] has type number, while the right type at <unlabeled-symbol>.Ret[1] has type string)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_arg_length")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo(x: number, y: number)
        return x
    end
    function almostFoo(x: number, y: number, c: number)
        return x
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 2 or more arguments, while the right type at <unlabeled-symbol> takes 3 or more arguments)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_arg_length_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo(x: number, y: string, z: number)
        return z
    end
    function almostFoo(x: number, y: string)
        return x
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 3 or more arguments, while the right type at <unlabeled-symbol> takes 2 or more arguments)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_arg_length_none")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo()
        return 5
    end
    function almostFoo(x: number, y: string)
        return x
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 0 or more arguments, while the right type at <unlabeled-symbol> takes 2 or more arguments)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_arg_length_none_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo(x: number)
        return x
    end
    function almostFoo()
        return 5
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 1 or more arguments, while the right type at <unlabeled-symbol> takes 0 or more arguments)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_ret_length")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo(x: number, y: number)
        return x
    end
    function almostFoo(x: number, y: number)
        return x, y
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> returns 1 values, while the right type at <unlabeled-symbol> returns 2 values)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_ret_length_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo(x: number, y: string, z: number)
        return y, x, z
    end
    function almostFoo(x: number, y: string, z: number)
        return y, x
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> returns 3 values, while the right type at <unlabeled-symbol> returns 2 values)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_ret_length_none")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo(x: number, y: string)
        return
    end
    function almostFoo(x: number, y: string)
        return x
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> returns 0 values, while the right type at <unlabeled-symbol> returns 1 values)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_ret_length_none_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo()
        return 5
    end
    function almostFoo()
        return
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> returns 1 values, while the right type at <unlabeled-symbol> returns 0 values)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_variadic_arg_normal")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo(x: number, y: string, ...: number)
        return x, y
    end
    function almostFoo(a: number, b: string, ...: string)
        return a, b
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Arg[Variadic] has type number, while the right type at <unlabeled-symbol>.Arg[Variadic] has type string)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_variadic_arg_missing")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo(x: number, y: string, ...: number)
        return x, y
    end
    function almostFoo(a: number, b: string)
        return a, b
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Arg[Variadic] has type number, while the right type at <unlabeled-symbol>.Arg[Variadic] has type any)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_variadic_arg_missing_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo(x: number, y: string)
        return x, y
    end
    function almostFoo(a: number, b: string, ...: string)
        return a, b
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.Arg[Variadic] has type any, while the right type at <unlabeled-symbol>.Arg[Variadic] has type string)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_variadic_oversaturation")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    -- allowed to be oversaturated
    function foo(x: number, y: string)
        return x, y
    end
    -- must not be oversaturated
    local almostFoo: (number, string) -> (number, string) = foo
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 2 or more arguments, while the right type at <unlabeled-symbol> takes 2 arguments)");
}

TEST_CASE_FIXTURE(DifferFixture, "function_variadic_oversaturation_2")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    -- must not be oversaturated
    local foo: (number, string) -> (number, string)
    -- allowed to be oversaturated
    function almostFoo(x: number, y: string)
        return x, y
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> takes 2 arguments, while the right type at <unlabeled-symbol> takes 2 or more arguments)");
}

TEST_CASE_FIXTURE(DifferFixture, "generic")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo(x, y)
        return x, y
    end
    function almostFoo(x, y)
        return y, x
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left generic at <unlabeled-symbol>.Ret[1] cannot be the same type parameter as the right generic at <unlabeled-symbol>.Ret[1])");
}

TEST_CASE_FIXTURE(DifferFixture, "generic_one_vs_two")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo<X>(x: X, y: X)
        return
    end
    function almostFoo<T, U>(x: T, y: U)
        return
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left generic at <unlabeled-symbol>.Arg[2] cannot be the same type parameter as the right generic at <unlabeled-symbol>.Arg[2])");
}

TEST_CASE_FIXTURE(DifferFixture, "generic_three_or_three")
{
    // Old solver does not correctly infer function typepacks
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, true};

    CheckResult result = check(R"(
    function foo<X, Y>(x: X, y: X, z: Y)
        return
    end
    function almostFoo<T, U>(x: T, y: U, z: U)
        return
    end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left generic at <unlabeled-symbol>.Arg[2] cannot be the same type parameter as the right generic at <unlabeled-symbol>.Arg[2])");
}

TEST_CASE_FIXTURE(DifferFixtureWithBuiltins, "equal_metatable")
{
    CheckResult result = check(R"(
    local metaFoo = {
        metaBar = 5
    }
    local metaAlmostFoo = {
        metaBar = 1
    }
    local foo = {
        bar = 3
    }
    setmetatable(foo, metaFoo)
    local almostFoo = {
        bar = 4
    }
    setmetatable(almostFoo, metaAlmostFoo)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixtureWithBuiltins, "metatable_normal")
{
    ScopedFastFlag sff{FFlag::DebugLuauDeferredConstraintResolution, false};

    CheckResult result = check(R"(
    local metaFoo = {
        metaBar = 5
    }
    local metaAlmostFoo = {
        metaBar = 1
    }
    local foo = {
        bar = 3
    }
    setmetatable(foo, metaFoo)
    local almostFoo = {
        bar = "hello"
    }
    setmetatable(almostFoo, metaAlmostFoo)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.bar has type number, while the right type at <unlabeled-symbol>.bar has type string)");
}

TEST_CASE_FIXTURE(DifferFixtureWithBuiltins, "metatable_metanormal")
{
    CheckResult result = check(R"(
    local metaFoo = {
        metaBar = "world"
    }
    local metaAlmostFoo = {
        metaBar = 1
    }
    local foo = {
        bar = "amazing"
    }
    setmetatable(foo, metaFoo)
    local almostFoo = {
        bar = "hello"
    }
    setmetatable(almostFoo, metaAlmostFoo)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.__metatable.metaBar has type string, while the right type at <unlabeled-symbol>.__metatable.metaBar has type number)");
}

TEST_CASE_FIXTURE(DifferFixtureWithBuiltins, "metatable_metamissing_left")
{
    CheckResult result = check(R"(
    local metaFoo = {
        metaBar = "world"
    }
    local metaAlmostFoo = {
        metaBar = 1,
        thisIsOnlyInRight = 2,
    }
    local foo = {
        bar = "amazing"
    }
    setmetatable(foo, metaFoo)
    local almostFoo = {
        bar = "hello"
    }
    setmetatable(almostFoo, metaAlmostFoo)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.__metatable is missing the property thisIsOnlyInRight, while the right type at <unlabeled-symbol>.__metatable.thisIsOnlyInRight has type number)");
}

TEST_CASE_FIXTURE(DifferFixtureWithBuiltins, "metatable_metamissing_right")
{
    CheckResult result = check(R"(
    local metaFoo = {
        metaBar = "world",
        thisIsOnlyInLeft = 2,
    }
    local metaAlmostFoo = {
        metaBar = 1,
    }
    local foo = {
        bar = "amazing"
    }
    setmetatable(foo, metaFoo)
    local almostFoo = {
        bar = "hello"
    }
    setmetatable(almostFoo, metaAlmostFoo)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol>.__metatable.thisIsOnlyInLeft has type number, while the right type at <unlabeled-symbol>.__metatable is missing the property thisIsOnlyInLeft)");
}

TEST_CASE_FIXTURE(DifferFixtureGeneric<ClassFixture>, "equal_class")
{
    CheckResult result = check(R"(
        local foo = BaseClass
        local almostFoo = BaseClass
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixtureGeneric<ClassFixture>, "class_normal")
{
    CheckResult result = check(R"(
        local foo = BaseClass
        local almostFoo = ChildClass
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> has type BaseClass, while the right type at <unlabeled-symbol> has type ChildClass)");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_generictp")
{
    CheckResult result = check(R"(
        local foo: <T...>() -> T...
        local almostFoo: <U...>() -> U...
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "generictp_ne_fn")
{
    CheckResult result = check(R"(
        local foo: <T, U...>(...T) -> U...
        local almostFoo: <U...>(U...) -> U...
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at <unlabeled-symbol> has type <T, U...>(...T) -> (U...), while the right type at <unlabeled-symbol> has type <U...>(U...) -> (U...))");
}

TEST_CASE_FIXTURE(DifferFixture, "generictp_normal")
{
    CheckResult result = check(R"(
        -- trN should be X... -> Y...
        -- s should be X -> Y...
        -- x should be X
        -- bij should be X... -> X...

        -- Intended signature: <X..., Y..., Z>(X... -> Y..., Z -> X..., X... -> Y..., Z, Y... -> Y...) -> ()
        function foo(tr, s, tr2, x, bij)
            bij(bij(tr(s(x))))
            bij(bij(tr2(s(x))))
        end
        -- Intended signature: <X..., Y..., Z>(X... -> X..., Z -> X..., X... -> Y..., Z, Y... -> Y...) -> ()
        function almostFoo(bij, s, tr, x, bij2)
            bij(bij(s(x)))
            bij2(bij2(tr(s(x))))
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    INFO(Luau::toString(requireType("foo")));
    INFO(Luau::toString(requireType("almostFoo")));

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left generic at <unlabeled-symbol>.Arg[1].Ret[Variadic] cannot be the same type parameter as the right generic at <unlabeled-symbol>.Arg[1].Ret[Variadic])");
}

TEST_CASE_FIXTURE(DifferFixture, "generictp_normal_2")
{
    CheckResult result = check(R"(
        -- trN should be X... -> Y...
        -- s should be X -> Y...
        -- x should be X
        -- bij should be X... -> X...

        function foo(s, tr, tr2, x, bij)
            bij(bij(tr(s(x))))
            bij(bij(tr2(s(x))))
        end
        function almostFoo(s, bij, tr, x, bij2)
            bij2(bij2(bij(bij(tr(s(x))))))
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    INFO(Luau::toString(requireType("foo")));
    INFO(Luau::toString(requireType("almostFoo")));

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left generic at <unlabeled-symbol>.Arg[2].Arg[Variadic] cannot be the same type parameter as the right generic at <unlabeled-symbol>.Arg[2].Arg[Variadic])");
}

TEST_CASE_FIXTURE(DifferFixture, "equal_generictp_cyclic")
{
    CheckResult result = check(R"(
        function foo(f, g, s, x)
            f(f(g(g(s(x)))))
            return foo
        end
        function almostFoo(f, g, s, x)
            g(g(f(f(s(x)))))
            return almostFoo
        end
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    INFO(Luau::toString(requireType("foo")));
    INFO(Luau::toString(requireType("almostFoo")));

    compareTypesEq("foo", "almostFoo");
}

TEST_CASE_FIXTURE(DifferFixture, "symbol_forward")
{
    CheckResult result = check(R"(
        local foo = 5
        local almostFoo = "five"
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    INFO(Luau::toString(requireType("foo")));
    INFO(Luau::toString(requireType("almostFoo")));

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at foo has type number, while the right type at almostFoo has type string)",
        true);
}

TEST_CASE_FIXTURE(DifferFixture, "newlines")
{
    CheckResult result = check(R"(
        local foo = 5
        local almostFoo = "five"
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    INFO(Luau::toString(requireType("foo")));
    INFO(Luau::toString(requireType("almostFoo")));

    compareTypesNe("foo", "almostFoo",
        R"(DiffError: these two types are not equal because the left type at
    foo
has type
    number,
while the right type at
    almostFoo
has type
    string)",
        true, true);
}

TEST_SUITE_END();
