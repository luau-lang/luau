// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/TopoSortStatements.h"
#include "Luau/Transpiler.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

static std::vector<AstStat*> toposort(AstStatBlock& block)
{
    std::vector<AstStat*> result{block.body.begin(), block.body.end()};

    Luau::toposort(result);

    return result;
}

TEST_SUITE_BEGIN("TopoSortTests");

TEST_CASE_FIXTURE(Fixture, "sorts")
{
    AstStatBlock* program = parse(R"(
        function A()
            return B("high five!")
        end

        function B(x)
            return x
        end
    )");

    auto sorted = toposort(*program);
    REQUIRE_EQ(2, sorted.size());

    AstStatBlock* block = program->as<AstStatBlock>();
    REQUIRE(block != nullptr);
    REQUIRE_EQ(2, block->body.size);

    // B is sorted ahead of A
    CHECK_EQ(block->body.data[1], sorted[0]);
    CHECK_EQ(block->body.data[0], sorted[1]);
}

TEST_CASE_FIXTURE(Fixture, "cyclic_dependency_terminates")
{
    AstStatBlock* program = parse(R"(
        function A()
            return B()
        end

        function B()
            return A()
        end
    )");

    auto sorted = toposort(*program);
    REQUIRE_EQ(2, sorted.size());
}

TEST_CASE_FIXTURE(Fixture, "doesnt_omit_statements_that_dont_need_sorting")
{
    AstStatBlock* program = parse(R"(
        local X = {}

        function A()
            return B(5), B("Hi")
        end

        local Y = {}

        function B(x)
            return x
        end

        local Z = B()
    )");

    auto sorted = toposort(*program);
    REQUIRE_EQ(5, sorted.size());

    AstStatBlock* block = program->as<AstStatBlock>();
    REQUIRE(block != nullptr);
    REQUIRE_EQ(5, block->body.size);

    AstStat* X = block->body.data[0];
    AstStat* A = block->body.data[1];
    AstStat* Y = block->body.data[2];
    AstStat* B = block->body.data[3];
    AstStat* Z = block->body.data[4];

    CHECK_EQ(sorted[0], X);
    CHECK_EQ(sorted[1], Y);
    CHECK_EQ(sorted[2], B);
    CHECK_EQ(sorted[3], Z);
    CHECK_EQ(sorted[4], A);
}

TEST_CASE_FIXTURE(Fixture, "slightly_more_complex")
{
    AstStatBlock* program = parse(R"(
        local T = {}

        function T:foo()
            return T:bar(999), T:bar("hi")
        end

        function T:bar(i)
            return i
        end
    )");

    auto sorted = toposort(*program);

    REQUIRE_EQ(3, sorted.size());

    CHECK_EQ(sorted[0], program->body.data[0]);
    CHECK_EQ(sorted[1], program->body.data[2]);
    CHECK_EQ(sorted[2], program->body.data[1]);
}

TEST_CASE_FIXTURE(Fixture, "reorder_functions_after_dependent_assigns")
{
    AstStatBlock* program = parse(R"(
        local T = {}                -- 0

        function T.a()              -- 1 depends on (2)
            T.b()
        end

        function T.b()              -- 2 depends on (4)
            T.c()
        end

        function make_function()    -- 3
            return function() end
        end

        T.c = make_function()       -- 4 depends on (3)

        T.a()                       -- 5 depends on (1)
    )");

    auto sorted = toposort(*program);

    REQUIRE_EQ(6, sorted.size());

    CHECK_EQ(sorted[0], program->body.data[0]);
    CHECK_EQ(sorted[1], program->body.data[3]);
    CHECK_EQ(sorted[2], program->body.data[4]);
    CHECK_EQ(sorted[3], program->body.data[2]);
    CHECK_EQ(sorted[4], program->body.data[1]);
    CHECK_EQ(sorted[5], program->body.data[5]);
}

TEST_CASE_FIXTURE(Fixture, "dont_reorder_assigns")
{
    AstStatBlock* program = parse(R"(
        local T = {}                -- 0

        function T.a()              -- 1 depends on (2)
            T.b()
        end

        function T.b()              -- 2 depends on (5)
            T.c()
        end

        function make_function()    -- 3
            return function() end
        end

        T.a()                       -- 4 depends on (1 -> 2 -> 5), but we cannot reorder it after 5!

        T.c = make_function()       -- 5 depends on (3)
    )");

    auto sorted = toposort(*program);

    REQUIRE_EQ(6, sorted.size());

    CHECK_EQ(sorted[0], program->body.data[0]);
    CHECK_EQ(sorted[1], program->body.data[3]);
    CHECK_EQ(sorted[2], program->body.data[2]);
    CHECK_EQ(sorted[3], program->body.data[1]);
    CHECK_EQ(sorted[4], program->body.data[4]);
    CHECK_EQ(sorted[5], program->body.data[5]);
}

TEST_CASE_FIXTURE(Fixture, "dont_reorder_function_after_assignment_to_global")
{
    AstStatBlock* program = parse(R"(
        local f

        function g()
            f()
        end

        f = function() end
    )");

    auto sorted = toposort(*program);

    REQUIRE_EQ(3, sorted.size());

    CHECK_EQ(sorted[0], program->body.data[0]);
    CHECK_EQ(sorted[1], program->body.data[1]);
    CHECK_EQ(sorted[2], program->body.data[2]);
}

TEST_CASE_FIXTURE(Fixture, "local_functions_need_sorting_too")
{
    AstStatBlock* program = parse(R"(
        local a = nil                       -- 0

        local function f()                  -- 1 depends on 4
            a.c = 4
        end

        local function g()                  -- 2 depends on 1
            f()
        end

        a = {}                              -- 3
        a.c = nil                           -- 4
    )");

    auto sorted = toposort(*program);

    REQUIRE_EQ(5, sorted.size());

    CHECK_EQ(sorted[0], program->body.data[0]);
    CHECK_EQ(sorted[1], program->body.data[3]);
    CHECK_EQ(sorted[2], program->body.data[4]);
    CHECK_EQ(sorted[3], program->body.data[1]);
    CHECK_EQ(sorted[4], program->body.data[2]);
}

TEST_CASE_FIXTURE(Fixture, "dont_force_checking_until_an_AstExprCall_needs_the_symbol")
{
    AstStatBlock* program = parse(R"(
        function A(obj)
            C(obj)
        end

        local B = A             -- It would be an error to force checking of A at this point just because the definition of B is an imperative

        function C(player)
        end

        local D = A(nil)        -- The real dependency on A is here, where A is invoked.
    )");

    auto sorted = toposort(*program);

    REQUIRE_EQ(4, sorted.size());

    auto A = program->body.data[0];
    auto B = program->body.data[1];
    auto C = program->body.data[2];
    auto D = program->body.data[3];

    CHECK_EQ(sorted[0], C);
    CHECK_EQ(sorted[1], A);
    CHECK_EQ(sorted[2], B);
    CHECK_EQ(sorted[3], D);
}

TEST_CASE_FIXTURE(Fixture, "dont_reorder_imperatives")
{
    AstStatBlock* program = parse(R"(
        local temp = work
        work = arr
        arr = temp
        width = width * 2
    )");

    auto sorted = toposort(*program);

    REQUIRE_EQ(4, sorted.size());
}

TEST_CASE_FIXTURE(Fixture, "sort_typealias_first")
{
    AstStatBlock* program = parse(R"(
        local foo: A = 1
        type A = number
    )");

    auto sorted = toposort(*program);

    REQUIRE_EQ(2, sorted.size());

    auto A = program->body.data[0];
    auto B = program->body.data[1];

    CHECK_EQ(sorted[0], B);
    CHECK_EQ(sorted[1], A);
}

TEST_CASE_FIXTURE(Fixture, "typealias_of_typeof_is_not_sorted")
{
    AstStatBlock* program = parse(R"(
        type Foo = typeof(foo)
        local function foo(x: number) end
    )");

    auto sorted = toposort(*program);

    REQUIRE_EQ(2, sorted.size());

    auto A = program->body.data[0];
    auto B = program->body.data[1];

    CHECK_EQ(sorted[0], A);
    CHECK_EQ(sorted[1], B);
}

TEST_CASE_FIXTURE(Fixture, "nested_type_annotations_depends_on_later_typealiases")
{
    AstStatBlock* program = parse(R"(
        type Foo = A | B
        type B = number
        type A = string
    )");

    auto sorted = toposort(*program);

    REQUIRE_EQ(3, sorted.size());

    auto Foo = program->body.data[0];
    auto B = program->body.data[1];
    auto A = program->body.data[2];

    CHECK_EQ(sorted[0], B);
    CHECK_EQ(sorted[1], A);
    CHECK_EQ(sorted[2], Foo);
}

TEST_CASE_FIXTURE(Fixture, "function_return_type_depends_on_type_aliases")
{
    AstStatBlock* program = parse(R"(
        type callbackFn<K, V> = (element: V, key: K, map: Map<K, V>) -> ()

        export type Map<K, V> = {
            forEach: (callback: callbackFn<K, V>) -> (),
        }

        function foo<K, V>(key: K, value: V): Map<K, V>
        end
    )");

    auto sorted = toposort(*program);

    REQUIRE_EQ(3, sorted.size());

    auto callbackFn = program->body.data[0];
    auto Map = program->body.data[1];
    auto foo = program->body.data[2];

    CHECK_EQ(sorted[0], callbackFn);
    CHECK_EQ(sorted[1], Map);
    CHECK_EQ(sorted[2], foo);
}

TEST_CASE_FIXTURE(Fixture, "return_comes_last")
{
    AstStatBlock* program = parse(R"(
        local module = {}

        local function confuseCompiler() return module.foo() end

        module.foo = function() return "" end

        function module.bar(x:number)
            confuseCompiler()
            return true
        end

        return module
    )");

    auto sorted = toposort(*program);

    CHECK_EQ(sorted[0], program->body.data[0]);
    CHECK_EQ(sorted[2], program->body.data[1]);
    CHECK_EQ(sorted[1], program->body.data[2]);
    CHECK_EQ(sorted[3], program->body.data[3]);
    CHECK_EQ(sorted[4], program->body.data[4]);
}

TEST_CASE_FIXTURE(Fixture, "break_comes_last")
{
    AstStatBlock* program = parse(R"(
repeat
local module = {}
local function confuseCompiler() return module.foo() end
module.foo = function() return "" end
break
until true
    )");

    REQUIRE(program->body.size == 1);

    auto repeat = program->body.data[0]->as<AstStatRepeat>();
    REQUIRE(repeat);

    REQUIRE(repeat->body->body.size == 4);

    auto sorted = toposort(*repeat->body);

    REQUIRE(sorted.size() == 4);
    CHECK_EQ(sorted[3], repeat->body->body.data[3]);
}

TEST_CASE_FIXTURE(Fixture, "continue_comes_last")
{
    AstStatBlock* program = parse(R"(
repeat
local module = {}
local function confuseCompiler() return module.foo() end
module.foo = function() return "" end
continue
until true
    )");

    REQUIRE(program->body.size == 1);

    auto repeat = program->body.data[0]->as<AstStatRepeat>();
    REQUIRE(repeat);

    REQUIRE(repeat->body->body.size == 4);

    auto sorted = toposort(*repeat->body);

    REQUIRE(sorted.size() == 4);
    CHECK_EQ(sorted[3], repeat->body->body.data[3]);
}

TEST_SUITE_END();
