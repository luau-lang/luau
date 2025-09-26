// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/Error.h"
#include "Luau/Frontend.h"
#include "Luau/Type.h"
#include "Luau/VisitType.h"

#include "Fixture.h"

#include "ScopedFlags.h"
#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauTrackFreeInteriorTypePacks)
LUAU_FASTFLAG(LuauIndexInMetatableSubtyping)

TEST_SUITE_BEGIN("TypeInferOOP");

TEST_CASE_FIXTURE(Fixture, "dont_suggest_using_colon_rather_than_dot_if_not_defined_with_colon")
{
    CheckResult result = check(R"(
        local someTable = {}

        someTable.Function1 = function(Arg1)
        end

        someTable.Function1() -- Argument count mismatch
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
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

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    REQUIRE(get<CountMismatch>(result.errors[0]));
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
    if (FFlag::LuauSolverV2)
        CHECK_GE(80, module->internalTypes.types.size());
    else
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
            local index: number = 0

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

    if (FFlag::LuauSolverV2)
        CHECK("{ @metatable { number: number }, { method: (unknown) -> string } }" == toString(requireType("B"), {true}));
    else
        CHECK("{ @metatable {| number: number |}, {| method: <a>(a) -> string |} }" == toString(requireType("B"), {true}));
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

TEST_CASE_FIXTURE(BuiltinsFixture, "promise_type_error_too_complex" * doctest::timeout(2))
{
    getFrontend().options.retainFullTypeGraphs = false;

    // Used `luau-reduce` tool to extract a minimal reproduction.
    // Credit: https://github.com/evaera/roblox-lua-promise/blob/v4.0.0/lib/init.lua
    CheckResult result = check(R"(
        --!strict

        local Promise = {}
        Promise.prototype = {}
        Promise.__index = Promise.prototype

        function Promise._new(traceback, callback, parent)
            if parent ~= nil and not Promise.is(parent)then
            end

            local self = {
                _parent = parent,
            }

            parent._consumers[self] = true
            setmetatable(self, Promise)
            self:_reject()

            return self
        end

        function Promise.resolve(...)
            return Promise._new(debug.traceback(nil, 2), function(resolve)
            end)
        end

        function Promise.reject(...)
            return Promise._new(debug.traceback(nil, 2), function(_, reject)
            end)
        end

        function Promise._try(traceback, callback, ...)
            return Promise._new(traceback, function(resolve)
            end)
        end

        function Promise.try(callback, ...)
            return Promise._try(debug.traceback(nil, 2), callback, ...)
        end

        function Promise._all(traceback, promises, amount)
            if #promises == 0 or amount == 0 then
                return Promise.resolve({})
            end
            return Promise._new(traceback, function(resolve, reject, onCancel)
            end)
        end

        function Promise.all(promises)
            return Promise._all(debug.traceback(nil, 2), promises)
        end

        function Promise.allSettled(promises)
            return Promise.resolve({})
        end

        function Promise.race(promises)
            return Promise._new(debug.traceback(nil, 2), function(resolve, reject, onCancel)
            end)
        end

        function Promise.each(list, predicate)
            return Promise._new(debug.traceback(nil, 2), function(resolve, reject, onCancel)
                local predicatePromise = Promise.resolve(predicate(value, index))
                local success, result = predicatePromise:await()
            end)
        end

        function Promise.is(object)
        end

        function Promise.prototype:_reject(...)
            self:_finalize()
        end
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "method_should_not_create_cyclic_type")
{
    ScopedFastFlag sff(FFlag::LuauSolverV2, true);

    CheckResult result = check(R"(
        local Component = {}

        function Component:__resolveUpdate(incomingState)
            local oldState = self.state
            incomingState = oldState
            self.state = incomingState
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "cross_module_metatable")
{
    fileResolver.source["game/A"] = R"(
        --!strict
        local cls = {}
        cls.__index = cls
        function cls:abc() return 4 end
        return cls
    )";

    fileResolver.source["game/B"] = R"(
        --!strict
        local cls = require(game.A)
        local tbl = {}
        setmetatable(tbl, cls)
    )";

    CheckResult result = getFrontend().check("game/B");
    LUAU_REQUIRE_NO_ERRORS(result);

    ModulePtr b = getFrontend().moduleResolver.getModule("game/B");
    REQUIRE(b);

    std::optional<Binding> clsBinding = b->getModuleScope()->linearSearchForBinding("tbl");
    REQUIRE(clsBinding);

    TypeId clsType = clsBinding->typeId;

    CHECK("{ @metatable cls, tbl }" == toString(clsType));
}

// https://luau.org/typecheck#adding-types-for-faux-object-oriented-programs
TEST_CASE_FIXTURE(BuiltinsFixture, "textbook_class_pattern")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local Account = {}
        Account.__index = Account

        type AccountData = {
            name: string,
            balance: number,
        }

        export type Account = setmetatable<AccountData, typeof(Account)>

        function Account.new(name, balance): Account
            local self = {}
            self.name = name
            self.balance = balance

            return setmetatable(self, Account)
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "textbook_class_pattern_2")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        local Account = {}
        Account.__index = Account

        type AccountData = {
            name: string,
            balance: number,
        }

        export type Account = setmetatable<AccountData, typeof(Account)>

        function Account.new(name, balance): Account
            local self = {}
            self.name = name
            self.balance = balance

            return setmetatable(self, Account)
        end

        function Account.deposit(self: Account, credit: number)
            self.balance += credit
        end

        function Account.withdraw(self: Account, debit: number)
            self.balance -= debit
        end

        function Account.hasBalance(self: Account, amount: number): boolean
            return self.balance >= amount
        end

        local account = Account.new("Hina", 500)

        if account:hasBalance(123) then -- TypeError: Value of type 'unknown' could be nil
        end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "oop_invoke_with_inferred_self_type")
{
    ScopedFastFlag _{FFlag::LuauIndexInMetatableSubtyping, true};

    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local ItemContainer = {}
        ItemContainer.__index = ItemContainer

        function ItemContainer.new()
            local self = {}
            setmetatable(self, ItemContainer)
            return self
        end

        function ItemContainer:removeItem(itemId, itemType)
            self:getItem(itemId, itemType)
        end

        function ItemContainer:getItem(itemId, itemType): ()
        end

        local container = ItemContainer.new()

        container:removeItem(0, "magic")
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "oop_invoke_with_inferred_self_and_property")
{
    ScopedFastFlag _{FFlag::LuauIndexInMetatableSubtyping, true};

    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local ItemContainer = {}
        ItemContainer.__index = ItemContainer

        function ItemContainer.new(name)
            local self = {name = name}
            setmetatable(self, ItemContainer)
            return self
        end

        function ItemContainer:removeItem(itemId, itemType)
            print(self.name)
            self:getItem(itemId, itemType)
        end

        function ItemContainer:getItem(itemId, itemType): ()
        end

        local container = ItemContainer.new("library")

        container:removeItem(0, "magic")
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "metatable_field_allows_upcast")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauIndexInMetatableSubtyping, true},
    };

    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local Foobar = {}
        Foobar.__index = Foobar
        Foobar.const = 42

        local foobar = setmetatable({}, Foobar)

        local _: { read const: number } = foobar
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "metatable_field_disallows_invalid_upcast")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauIndexInMetatableSubtyping, true},
    };

    CheckResult results = check(R"(
        local Foobar = {}
        Foobar.__index = Foobar
        Foobar.const = 42

        local foobar = setmetatable({}, Foobar)

        local _: { const: number } = foobar
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, results);
    auto err = get<TypeMismatch>(results.errors[0]);
    REQUIRE(err);
    CHECK_EQ("{ const: number }", toString(err->wantedType));
    CHECK_EQ("{ @metatable t1, {  } } where t1 = { __index: t1, const: number }", toString(err->givenType, {/* exhaustive */ true}));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "metatable_field_precedence_for_subtyping")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauIndexInMetatableSubtyping, true},
    };

    CheckResult results = check(R"(
        local function foobar1(_: { read foo: number }) end
        local function foobar2(_: { read bar: boolean }) end
        local function foobar3(_: { read foo: string }) end

        local t = { foo = 4 }
        setmetatable(t, { __index = { foo = "heh", bar = true }})
        foobar1(t)
        foobar2(t)
        foobar3(t)
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, results);
    auto err = get<TypeMismatch>(results.errors[0]);
    REQUIRE(err);
    CHECK_EQ("{ read foo: string }", toString(err->wantedType, {/* exhaustive */ true}));
    CHECK_EQ("{ @metatable { __index: { bar: boolean, foo: string } }, { foo: number } }", toString(err->givenType, { /* exhaustive */ true}));
}

TEST_SUITE_END();
