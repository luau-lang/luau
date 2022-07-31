// This file is part of the lluz programming language and is licensed under MIT License; see LICENSE.txt for details

#include "lluz/AstQuery.h"
#include "lluz/BuiltinDefinitions.h"
#include "lluz/Scope.h"
#include "lluz/TypeInfer.h"
#include "lluz/TypeVar.h"
#include "lluz/VisitTypeVar.h"

#include "Fixture.h"

#include "doctest.h"

using namespace lluz;

TEST_SUITE_BEGIN(XorStr("TypeInferOOP"));

TEST_CASE_FIXTURE(Fixture, "dont_suggest_using_colon_rather_than_dot_if_not_defined_with_colon")
{
    CheckResult result = check(R"(
        local someTable = {}

        someTable.Function1 = function(Arg1)
        end

        someTable.Function1() -- Argument count mismatch
    )");

    lluz_REQUIRE_ERROR_COUNT(1, result);

    REQUIRE(get<CountMismatch>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "dont_suggest_using_colon_rather_than_dot_if_it_wont_help_2")
{
    CheckResult result = check(R"(
        local someTable = {}

        someTable.Function2 = function(Arg1, Arg2)
        end

        someTable.Function2() -- Argument count mismatch
    )");

    lluz_REQUIRE_ERROR_COUNT(1, result);

    REQUIRE(get<CountMismatch>(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "dont_suggest_using_colon_rather_than_dot_if_another_overload_works")
{
    CheckResult result = check(R"(
        type T = {method: ((T, number) -> number) & ((number) -> number)}
        local T: T

        T.method(4)
    )");

    lluz_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "method_depends_on_table")
{
    CheckResult result = check(R"(
        -- This catches a bug where x:m didn't count as a use of x
        -- so toposort would happily reorder a definition of
        -- function x:m before the definition of x.
        function g() f() end
        local x = {}
        function x:m() end
        function f() x:m() end
    )");

    lluz_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "methods_are_topologically_sorted")
{
    CheckResult result = check(R"(
        local T = {}

        function T:foo()
            return T:bar(999), T:bar("hi")
        end

        function T:bar(i)
            return i
        end

        local a, b = T:foo()
    )");

    lluz_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);

    CHECK_EQ(PrimitiveTypeVar::Number, getPrimitiveType(requireType("a")));
    CHECK_EQ(PrimitiveTypeVar::String, getPrimitiveType(requireType("b")));
}

TEST_CASE_FIXTURE(Fixture, "quantify_methods_defined_using_dot_syntax_and_explicit_self_parameter")
{
    check(R"(
        local T = {}

        function T.method(self)
            self:method()
        end

        function T.method2(self)
            self:method()
        end

        T:method2()
    )");
}

TEST_CASE_FIXTURE(Fixture, "inferring_hundreds_of_self_calls_should_not_suffocate_memory")
{
    CheckResult result = check(R"(
        ("foo")
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
            :lower()
    )");

    ModulePtr module = getMainModule();
    CHECK_GE(50, module->internalTypes.typeVars.size());
}

TEST_CASE_FIXTURE(BuiltinsFixture, "object_constructor_can_refer_to_method_of_self")
{
    // CLI-30902
    CheckResult result = check(R"(
        --!strict

        type Foo = {
            fooConn: () -> () | nil
        }

        local Foo = {}
        Foo.__index = Foo

        function Foo.new()
            local self: Foo = {
                fooConn = nil,
            }
            setmetatable(self, Foo)

            self.fooConn = function()
                self:method() -- Key 'method' not found in table self
            end

            return self
        end

        function Foo:method()
            print("foo")
        end

        local foo = Foo.new()

        -- TODO This is the best our current refinement support can offer :(
        local bar = foo.fooConn
        if bar then bar() end

        -- foo.fooConn()
    )");

    lluz_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "CheckMethodsOfSealed")
{
    CheckResult result = check(R"(
local x: {prop: number} = {prop=9999}
function x:y(z: number)
    local s: string = z
end
)");

    lluz_REQUIRE_ERROR_COUNT(2, result);
}

TEST_CASE_FIXTURE(Fixture, "nonstrict_self_mismatch_tail")
{
    CheckResult result = check(R"(
        --!nonstrict
        local f = {}
        function f:foo(a: number, b: number) end

        function bar(...)
            f.foo(f, 1, ...)
        end

        bar(2)
    )");
    lluz_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "inferred_methods_of_free_tables_have_the_same_level_as_the_enclosing_table")
{
    check(R"(
        function Base64FileReader(data)
            local reader = {}
            local index: number

            function reader:PeekByte()
                return data:byte(index)
            end

            function reader:Byte()
                return data:byte(index - 1)
            end

            return reader
        end

        Base64FileReader()

        function ReadMidiEvents(data)

            local reader = Base64FileReader(data)

            while reader:HasMore() do
                (reader:Byte() % 128)
            end
        end
    )");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "table_oop")
{
    CheckResult result = check(R"(
   --!strict
local Class = {}
Class.__index = Class

type Class = typeof(setmetatable({} :: { x: number }, Class))

function Class.new(x: number): Class
    return setmetatable({x = x}, Class)
end

function Class.getx(self: Class)
    return self.x
end

function test()
    local c = Class.new(42)
    local n = c:getx()
    local nn = c.x

    print(string.format(XorStr("%d %d"), n, nn))
end
)");

    lluz_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
