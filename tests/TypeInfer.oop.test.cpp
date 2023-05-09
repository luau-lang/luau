// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"
#include "Luau/VisitType.h"

#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

TEST_SUITE_BEGIN("TypeInferOOP");

TEST_CASE_FIXTURE(Fixture, "dont_suggest_using_colon_rather_than_dot_if_not_defined_with_colon")
{
    CheckResult result = check(R"(
        local someTable = {}

        someTable.Function1 = function(Arg1)
        end

        someTable.Function1() -- Argument count mismatch
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        CHECK(toString(result.errors[0]) == "No overload for function accepts 0 arguments.");
        CHECK(toString(result.errors[1]) == "Available overloads: <a>(a) -> ()");
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        REQUIRE(get<CountMismatch>(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "dont_suggest_using_colon_rather_than_dot_if_it_wont_help_2")
{
    CheckResult result = check(R"(
        local someTable = {}

        someTable.Function2 = function(Arg1, Arg2)
        end

        someTable.Function2() -- Argument count mismatch
    )");

    if (FFlag::DebugLuauDeferredConstraintResolution)
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        CHECK(toString(result.errors[0]) == "No overload for function accepts 0 arguments.");
        CHECK(toString(result.errors[1]) == "Available overloads: <a, b>(a, b) -> ()");
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        REQUIRE(get<CountMismatch>(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(Fixture, "dont_suggest_using_colon_rather_than_dot_if_another_overload_works")
{
    CheckResult result = check(R"(
        type T = {method: ((T, number) -> number) & ((number) -> number)}
        local T: T

        T.method(4)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
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

    LUAU_REQUIRE_NO_ERRORS(result);
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

    LUAU_REQUIRE_NO_ERRORS(result);
    dumpErrors(result);

    CHECK_EQ(PrimitiveType::Number, getPrimitiveType(requireType("a")));
    CHECK_EQ(PrimitiveType::String, getPrimitiveType(requireType("b")));
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
    CHECK_GE(50, module->internalTypes.types.size());
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

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "CheckMethodsOfSealed")
{
    CheckResult result = check(R"(
local x: {prop: number} = {prop=9999}
function x:y(z: number)
    local s: string = z
end
)");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
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
    LUAU_REQUIRE_NO_ERRORS(result);
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

    print(string.format("%d %d", n, nn))
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "set_prop_of_intersection_containing_metatable")
{
    CheckResult result = check(R"(
        export type Set<T> = typeof(setmetatable(
            {} :: {
                add: (self: Set<T>, T) -> Set<T>,
            },
            {}
        ))

        local Set = {} :: Set<any> & {}

        function Set:add(t)
            return self
        end
    )");
}

// DCR once had a bug in the following code where it would erroneously bind the 'self' table to itself.
TEST_CASE_FIXTURE(Fixture, "dont_bind_free_tables_to_themselves")
{
    CheckResult result = check(R"(
        local T = {}
        local b: any

        function T:m()
            local a = b[i]
            if a then
                self:n()
                if self:p(a) then
                    self:n()
                end
            end
        end
    )");
}

// We should probably flag an error on this.  See CLI-68672
TEST_CASE_FIXTURE(BuiltinsFixture, "flag_when_index_metamethod_returns_0_values")
{
    CheckResult result = check(R"(
        local T = {}
        function T.__index()
        end

        local a = setmetatable({}, T)
        local p = a.prop
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("nil" == toString(requireType("p")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "augmenting_an_unsealed_table_with_a_metatable")
{
    CheckResult result = check(R"(
        local A = {number = 8}

        local B = setmetatable({}, A)

        function B:method()
            return "hello!!"
        end
    )");

    CHECK("{ @metatable { number: number }, { method: <a>(a) -> string } }" == toString(requireType("B"), {true}));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "react_style_oo")
{
    CheckResult result = check(R"(
        local Prototype = {}

        local ClassMetatable = {
            __index = Prototype
        }

        local BaseClass = (setmetatable({}, ClassMetatable))

        function BaseClass:extend(name)
            local class = {
                name=name
            }

            class.__index = class

            function class.ctor(props)
                return setmetatable({props=props}, class)
            end

            return setmetatable(class, getmetatable(self))
        end

        local C = BaseClass:extend('C')
        local i = C.ctor({hello='world'})

        local iName = i.name
        local cName = C.name
        local hello = i.props.hello
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("string" == toString(requireType("iName")));
    CHECK("string" == toString(requireType("cName")));
    CHECK("string" == toString(requireType("hello")));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "cycle_between_object_constructor_and_alias")
{
    CheckResult result = check(R"(
        local T = {}
        T.__index = T

        function T.new(): T
            return setmetatable({}, T)
        end

        export type T = typeof(T.new())

        return T
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    auto module = getMainModule();

    REQUIRE(module->exportedTypeBindings.count("T"));

    TypeId aliasType = module->exportedTypeBindings["T"].type;
    CHECK_MESSAGE(get<MetatableType>(follow(aliasType)), "Expected metatable type but got: " << toString(aliasType));
}

TEST_SUITE_END();
