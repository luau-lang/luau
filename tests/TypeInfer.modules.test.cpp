// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/Type.h"

#include "Fixture.h"

#include "Luau/VisitType.h"
#include "doctest.h"

LUAU_FASTFLAG(LuauInstantiateInSubtyping)
LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauReturnMappedGenericPacksFromSubtyping3)
LUAU_FASTFLAG(DebugLuauMagicTypes)
LUAU_FASTFLAG(LuauLimitDynamicConstraintSolving3)
LUAU_FASTINT(LuauSolverConstraintLimit)
LUAU_FASTFLAG(LuauNameConstraintRestrictRecursiveTypes)

using namespace Luau;

TEST_SUITE_BEGIN("TypeInferModules");

TEST_CASE_FIXTURE(BuiltinsFixture, "dcr_require_basic")
{
    fileResolver.source["game/A"] = R"(
        --!strict
        return {
            a = 1,
        }
    )";

    fileResolver.source["game/B"] = R"(
        --!strict
        local A = require(game.A)

        local b = A.a
    )";

    CheckResult aResult = getFrontend().check("game/A");
    LUAU_REQUIRE_NO_ERRORS(aResult);

    CheckResult bResult = getFrontend().check("game/B");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    ModulePtr b = getFrontend().moduleResolver.getModule("game/B");
    REQUIRE(b != nullptr);
    std::optional<TypeId> bType = requireType(b, "b");
    REQUIRE(bType);
    CHECK(toString(*bType) == "number");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "require")
{
    fileResolver.source["game/A"] = R"(
        local function hooty(x: number): string
            return "Hi there!"
        end

        return {hooty=hooty}
    )";

    if (FFlag::LuauSolverV2)
    {
        fileResolver.source["game/B"] = R"(
            local Hooty = require(game.A)

            local h = 4
            local i = Hooty.hooty(h)
        )";
    }
    else
    {
        fileResolver.source["game/B"] = R"(
            local Hooty = require(game.A)

            local h -- free!
            local i = Hooty.hooty(h)
        )";
    }

    CheckResult aResult = getFrontend().check("game/A");
    dumpErrors(aResult);
    LUAU_REQUIRE_NO_ERRORS(aResult);

    CheckResult bResult = getFrontend().check("game/B");
    dumpErrors(bResult);
    LUAU_REQUIRE_NO_ERRORS(bResult);

    ModulePtr b = getFrontend().moduleResolver.getModule("game/B");

    REQUIRE(b != nullptr);

    dumpErrors(bResult);

    std::optional<TypeId> iType = requireType(b, "i");
    REQUIRE_EQ("string", toString(*iType));

    std::optional<TypeId> hType = requireType(b, "h");
    REQUIRE_EQ("number", toString(*hType));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "require_types")
{
    fileResolver.source["workspace/A"] = R"(
        export type Point = {x: number, y: number}

        return {}
    )";

    fileResolver.source["workspace/B"] = R"(
        local Hooty = require(workspace.A)

        local h: Hooty.Point
    )";

    CheckResult bResult = getFrontend().check("workspace/B");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    ModulePtr b = getFrontend().moduleResolver.getModule("workspace/B");
    REQUIRE(b != nullptr);

    TypeId hType = requireType(b, "h");
    REQUIRE_MESSAGE(bool(get<TableType>(hType)), "Expected table but got " << toString(hType));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "require_a_variadic_function")
{
    fileResolver.source["game/A"] = R"(
        local T = {}
        function T.f(...) end
        return T
    )";

    fileResolver.source["game/B"] = R"(
        local A = require(game.A)
        local f = A.f
    )";

    CheckResult result = getFrontend().check("game/B");

    ModulePtr bModule = getFrontend().moduleResolver.getModule("game/B");
    REQUIRE(bModule != nullptr);

    TypeId f = follow(requireType(bModule, "f"));

    const FunctionType* ftv = get<FunctionType>(f);
    REQUIRE(ftv);

    auto iter = begin(ftv->argTypes);
    auto endIter = end(ftv->argTypes);

    REQUIRE(iter == endIter);
    REQUIRE(iter.tail());

    CHECK(get<VariadicTypePack>(*iter.tail()));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "cross_module_table_freeze")
{
    fileResolver.source["game/A"] = R"(
        --!strict
        return {
            a = 1,
        }
    )";

    fileResolver.source["game/B"] = R"(
        --!strict
        return table.freeze(require(game.A))
    )";

    CheckResult aResult = getFrontend().check("game/A");
    LUAU_REQUIRE_NO_ERRORS(aResult);

    CheckResult bResult = getFrontend().check("game/B");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    ModulePtr a = getFrontend().moduleResolver.getModule("game/A");
    REQUIRE(a != nullptr);
    // confirm that no cross-module mutation happened here!
    CHECK(toString(a->returnType) == "{ a: number }");

    ModulePtr b = getFrontend().moduleResolver.getModule("game/B");
    REQUIRE(b != nullptr);
    // confirm that no cross-module mutation happened here!
    if (FFlag::LuauSolverV2)
        CHECK(toString(b->returnType) == "{ read a: number }");
    else
        CHECK(toString(b->returnType) == "{ a: number }");
}

TEST_CASE_FIXTURE(Fixture, "type_error_of_unknown_qualified_type")
{
    CheckResult result = check(R"(
        local p: SomeModule.DoesNotExist
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    REQUIRE_EQ(result.errors[0], (TypeError{Location{{1, 17}, {1, 40}}, UnknownSymbol{"SomeModule.DoesNotExist"}}));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "require_module_that_does_not_export")
{
    const std::string sourceA = R"(
    )";

    const std::string sourceB = R"(
        local Hooty = require(script.Parent.A)
    )";

    fileResolver.source["game/Workspace/A"] = sourceA;
    fileResolver.source["game/Workspace/B"] = sourceB;

    getFrontend().check("game/Workspace/A");
    getFrontend().check("game/Workspace/B");

    ModulePtr aModule = getFrontend().moduleResolver.getModule("game/Workspace/A");
    ModulePtr bModule = getFrontend().moduleResolver.getModule("game/Workspace/B");

    CHECK(aModule->errors.empty());
    REQUIRE_EQ(1, bModule->errors.size());
    CHECK_MESSAGE(get<IllegalRequire>(bModule->errors[0]), "Should be IllegalRequire: " << toString(bModule->errors[0]));

    auto hootyType = requireType(bModule, "Hooty");

    CHECK_EQ("*error-type*", toString(hootyType));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "warn_if_you_try_to_require_a_non_modulescript")
{
    fileResolver.source["Modules/A"] = "";
    fileResolver.sourceTypes["Modules/A"] = SourceCode::Script;

    fileResolver.source["Modules/B"] = R"(
        local M = require(script.Parent.A)
    )";

    CheckResult result = getFrontend().check("Modules/B");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK(get<IllegalRequire>(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "general_require_call_expression")
{
    fileResolver.source["game/A"] = R"(
--!strict
return { def = 4 }
    )";

    fileResolver.source["game/B"] = R"(
--!strict
local tbl = { abc = require(game.A) }
local a : string = ""
a = tbl.abc.def
    )";

    CheckResult result = getFrontend().check("game/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type 'number' could not be converted into 'string'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "general_require_type_mismatch")
{
    fileResolver.source["game/A"] = R"(
return { def = 4 }
    )";

    fileResolver.source["game/B"] = R"(
local tbl: string = require(game.A)
    )";

    CheckResult result = getFrontend().check("game/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type '{ def: number }' could not be converted into 'string'", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(Fixture, "bound_free_table_export_is_ok")
{
    CheckResult result = check(R"(
local n = {}
function n:Clone() end

local m = {}

function m.a(x)
    x:Clone()
end

function m.b()
    m.a(n)
end

return m
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "custom_require_global")
{
    CheckResult result = check(R"(
--!nonstrict
require = function(a) end

local crash = require(game.A)
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "require_failed_module")
{
    fileResolver.source["game/A"] = R"(
return unfortunately()
    )";

    CheckResult aResult = getFrontend().check("game/A");
    LUAU_REQUIRE_ERRORS(aResult);

    CheckResult result = check(R"(
local ModuleA = require(game.A)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    std::optional<TypeId> oty = requireType("ModuleA");

    CHECK_EQ("*error-type*", toString(*oty));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "do_not_modify_imported_types")
{
    fileResolver.source["game/A"] = R"(
export type Type = { unrelated: boolean }
return {}
    )";

    fileResolver.source["game/B"] = R"(
local types = require(game.A)
type Type = types.Type
local x: Type = {}
function x:Destroy(): () end
    )";

    CheckResult result = getFrontend().check("game/B");
    LUAU_REQUIRE_ERROR_COUNT(2, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "do_not_modify_imported_types_2")
{
    fileResolver.source["game/A"] = R"(
export type Type = { x: { a: number } }
return {}
    )";

    fileResolver.source["game/B"] = R"(
local types = require(game.A)
type Type = types.Type
local x: Type = { x = { a = 2 } }
type Rename = typeof(x.x)
    )";

    CheckResult result = getFrontend().check("game/B");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "do_not_modify_imported_types_3")
{
    fileResolver.source["game/A"] = R"(
local y = setmetatable({}, {})
export type Type = { x: typeof(y) }
return { x = y }
    )";

    fileResolver.source["game/B"] = R"(
local types = require(game.A)
type Type = types.Type
local x: Type = types
type Rename = typeof(x.x)
    )";

    CheckResult result = getFrontend().check("game/B");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "do_not_modify_imported_types_4")
{
    fileResolver.source["game/A"] = R"(
export type Array<T> = {T}
local arrayops = {}
function arrayops.foo(x: Array<any>) end
return arrayops
    )";

    CheckResult result = check(R"(
local arrayops = require(game.A)

local tbl = {}
tbl.a = 2
function tbl:foo(b: number, c: number)
    -- introduce BoundType to imported type
    arrayops.foo(self._regions)
end
-- this alias decreases function type level and causes a demotion of its type
type Table = typeof(tbl)
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "do_not_modify_imported_types_5")
{
    fileResolver.source["game/A"] = R"(
export type Type = {x: number, y: number}
local arrayops = {}
function arrayops.foo(x: Type) end
return arrayops
    )";

    CheckResult result = check(R"(
local arrayops = require(game.A)

local tbl = {}
tbl.a = 2
function tbl:foo(b: number, c: number)
    -- introduce boundTo TableType to imported type
    self.x.a = 2
    arrayops.foo(self.x)
end
-- this alias decreases function type level and causes a demotion of its type
type Table = typeof(tbl)
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "module_type_conflict")
{
    fileResolver.source["game/A"] = R"(
export type T = { x: number }
return {}
    )";

    fileResolver.source["game/B"] = R"(
export type T = { x: string }
return {}
    )";

    fileResolver.source["game/C"] = R"(
local A = require(game.A)
local B = require(game.B)
local a: A.T = { x = 2 }
local b: B.T = a
    )";

    CheckResult result = getFrontend().check("game/C");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    if (FFlag::LuauSolverV2)
    {
        const std::string expected = "Type 'T' from 'game/A' could not be converted into 'T' from 'game/B'; \n"
                                     "this is because accessing `x` results in `number` in the former type and `string` in the latter type, and "
                                     "`number` is not exactly `string`";
        CHECK(expected == toString(result.errors[0]));
    }
    else
    {
        const std::string expected = R"(Type 'T' from 'game/A' could not be converted into 'T' from 'game/B'
caused by:
  Property 'x' is not compatible.
Type 'number' could not be converted into 'string' in an invariant context)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "module_type_conflict_instantiated")
{
    fileResolver.source["game/A"] = R"(
export type Wrap<T> = { x: T }
return {}
    )";

    fileResolver.source["game/B"] = R"(
local A = require(game.A)
export type T = A.Wrap<number>
return {}
    )";

    fileResolver.source["game/C"] = R"(
local A = require(game.A)
export type T = A.Wrap<string>
return {}
    )";

    fileResolver.source["game/D"] = R"(
local A = require(game.B)
local B = require(game.C)
local a: A.T = { x = 2 }
local b: B.T = a
    )";

    CheckResult result = getFrontend().check("game/D");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    if (FFlag::LuauSolverV2)
    {
        const std::string expected = "Type 'T' from 'game/B' could not be converted into 'T' from 'game/C'; \n"
                                     "this is because accessing `x` results in `number` in the former type and `string` in the latter type, and "
                                     "`number` is not exactly `string`";
        CHECK(expected == toString(result.errors[0]));
    }
    else
    {
        const std::string expected = R"(Type 'T' from 'game/B' could not be converted into 'T' from 'game/C'
caused by:
  Property 'x' is not compatible.
Type 'number' could not be converted into 'string' in an invariant context)";
        CHECK_EQ(expected, toString(result.errors[0]));
    }
}

TEST_CASE_FIXTURE(BuiltinsFixture, "constrained_anyification_clone_immutable_types")
{
    fileResolver.source["game/A"] = R"(
return function(...) end
    )";

    fileResolver.source["game/B"] = R"(
local l0 = require(game.A)
return l0
    )";

    CheckResult result = getFrontend().check("game/B");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "fuzz_anyify_variadic_return_must_follow")
{
    CheckResult result = check(R"(
return unpack(l0[_])
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "check_imported_module_names")
{
    fileResolver.source["game/A"] = R"(
return function(...) end
    )";

    fileResolver.source["game/B"] = R"(
local l0 = require(game.A)
return l0
    )";

    CheckResult result = check(R"(
local l0 = require(game.B)
if true then
    local l1 = require(game.A)
end
return l0
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    ModulePtr mod = getMainModule();
    REQUIRE(mod);

    REQUIRE(mod->scopes.size() == 4);
    CHECK(mod->scopes[0].second->importedModules["l0"] == "game/B");
    CHECK(mod->scopes[3].second->importedModules["l1"] == "game/A");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "ensure_scope_is_nullptr_after_shallow_copy")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};
    getFrontend().options.retainFullTypeGraphs = false;

    fileResolver.source["game/A"] = R"(
-- Roughly taken from ReactTypes.lua
type CoreBinding<T> = {}
type BindingMap = {}
export type Binding<T> = CoreBinding<T> & BindingMap

return {}
    )";

    LUAU_REQUIRE_NO_ERRORS(check(R"(
local Types = require(game.A)
type Binding<T> = Types.Binding<T>
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "ensure_free_variables_are_generialized_across_function_boundaries")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    fileResolver.source["game/A"] = R"(
-- Roughly taken from react-shallow-renderer
function createUpdater(renderer)
    local updater = {
        _renderer = renderer,
    }

    function updater.enqueueForceUpdate(publicInstance, callback, _callerName)
        updater._renderer.render(
            updater._renderer,
            updater._renderer._element,
            updater._renderer._context
        )
    end

    function updater.enqueueReplaceState(
        publicInstance,
        completeState,
        callback,
        _callerName
    )
        updater._renderer.render(
            updater._renderer,
            updater._renderer._element,
            updater._renderer._context
        )
    end

    function updater.enqueueSetState(publicInstance, partialState, callback, _callerName)
        local currentState = updater._renderer._newState or publicInstance.state
        updater._renderer.render(
            updater._renderer,
            updater._renderer._element,
            updater._renderer._context
        )
    end

    return updater
end

local ReactShallowRenderer = {}

function ReactShallowRenderer:_reset()
    self._updater = createUpdater(self)
end

return ReactShallowRenderer
    )";

    LUAU_REQUIRE_NO_ERRORS(check(R"(
local ReactShallowRenderer = require(game.A);
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "untitled_segfault_number_13")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    fileResolver.source["game/A"] = R"(
        -- minimized from roblox-requests/http/src/response.lua
        local Response = {}
        Response.__index = Response
        function Response.new(content_type)
            -- creates response object from original request and roblox http response
            local self = setmetatable({}, Response)
            self.content_type = content_type
            return self
        end

        function Response:xml(ignore_content_type)
            if ignore_content_type or self.content_type:find("+xml") or self.content_type:find("/xml") then
            else
            end
        end

        ---------------

        return Response
    )";

    LUAU_REQUIRE_NO_ERRORS(check(R"(
        local _ = require(game.A);
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "spooky_blocked_type_laundered_by_bound_type")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    fileResolver.source["game/A"] = R"(
        local Cache = {}

        Cache.settings = {}

        Cache.data = {}

        function Cache.should_cache(url)
            url = url:split("?")[1]

            for key, _ in pairs(Cache.settings) do
                if url:match('') then
                    return key
                end
            end

            return ""
        end

        function Cache.is_cached(url, req_id)
            -- check local server cache first

            local setting_key = Cache.should_cache(url)
            local settings = Cache.settings[setting_key]

            if not setting_key then
                return false
            end

            if Cache.data[req_id] ~= nil then
                return true
            end

            if Cache.settings[setting_key].cache_globally then
                return false
            else
                return true
            end
        end

        function Cache.get_expire(url)
            local setting_key = Cache.should_cache(url)
            return Cache.settings[setting_key].expires or math.huge
        end

        return Cache
    )";

    auto result = check(R"(
        local _ = require(game.A);
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "leaky_generics")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    auto result = check(R"(
        local Cache = {}

        Cache.settings = {}

        function Cache.should_cache(url)
            for key, _ in pairs(Cache.settings) do
                return key
            end

            return ""
        end

        function Cache.is_cached(url)
            local setting_key = Cache.should_cache(url)
            local settings = Cache.settings[setting_key]

            return settings
        end

        return Cache
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK("(unknown) -> unknown" == toString(requireTypeAtPosition({13, 23})));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "cycles_dont_make_everything_any")
{
    fileResolver.source["game/A"] = R"(
        --!strict
        local module = {}

        function module.foo()
            return 2
        end

        function module.bar()
            local m = require(game.B)
            return m.foo() + 1
        end

        return module
    )";

    fileResolver.source["game/B"] = R"(
        --!strict
        local module = {}

        function module.foo()
            return 2
        end

        function module.bar()
            local m = require(game.A)
            return m.foo() + 1
        end

        return module
    )";

    getFrontend().check("game/A");

    CHECK("module" == toString(getFrontend().moduleResolver.getModule("game/B")->returnType));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "cross_module_function_mutation")
{
    ScopedFastFlag _[] = {{FFlag::LuauSolverV2, true}, {FFlag::LuauReturnMappedGenericPacksFromSubtyping3, true}};

    fileResolver.source["game/A"] = R"(
function test2(a: number, b: string)
    return 1
end

return test2
    )";

    fileResolver.source["game/B"] = R"(
function wrapper<A...>(f: (A...) -> number, ...: A...)
end

local test2 = require(game.A)

return wrapper(test2, 1, "")
    )";

    CheckResult result = getFrontend().check("game/B");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "internal_types_are_scrubbed_from_module")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::DebugLuauMagicTypes, true},
        {FFlag::LuauLimitDynamicConstraintSolving3, true},
    };

    fileResolver.source["game/A"] = R"(
return function(): _luau_blocked_type return nil :: any end
    )";

    CheckResult result = getFrontend().check("game/A");
    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(get<ConstraintSolvingIncompleteError>(result.errors[0]));
    CHECK(get<InternalError>(result.errors[1]));
    CHECK("(...any) -> *error-type*" == toString(getFrontend().moduleResolver.getModule("game/A")->returnType));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "internal_type_errors_are_only_reported_once")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::DebugLuauMagicTypes, true},
        {FFlag::LuauLimitDynamicConstraintSolving3, true},
    };

    fileResolver.source["game/A"] = R"(
return function(): { X: _luau_blocked_type, Y: _luau_blocked_type } return nil :: any end
    )";

    CheckResult result = getFrontend().check("game/A");
    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(get<ConstraintSolvingIncompleteError>(result.errors[0]));
    CHECK(get<InternalError>(result.errors[1]));
    CHECK("(...any) -> { X: *error-type*, Y: *error-type* }" == toString(getFrontend().moduleResolver.getModule("game/A")->returnType));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "scrub_unsealed_tables")
{
    ScopedFastFlag sffs[] = {{FFlag::LuauSolverV2, true}, {FFlag::LuauLimitDynamicConstraintSolving3, true}};

    ScopedFastInt sfi{FInt::LuauSolverConstraintLimit, 5};

    fileResolver.source["game/A"] = R"(
        type Array<T> = {T}
        type Hello = Array<Array<Array<Array<Array<Array<Array<Array<Array<Array<number>>>>>>>>>>
        local X = {}
        X.foo = 42
        X.bar = ""
        return X
    )";

    fileResolver.source["game/B"] = R"(
        local x = require(game.A)
        x.lmao = 42
        return {}
    )";

    CheckResult result = getFrontend().check("game/B");
    // This is going to have a _ton_ of errors
    LUAU_REQUIRE_ERRORS(result);
    LUAU_CHECK_ERROR(result, CodeTooComplex);
    LUAU_CHECK_ERROR(result, ConstraintSolvingIncompleteError);
    LUAU_CHECK_ERROR(result, InternalError);
    LUAU_CHECK_ERROR(result, CannotExtendTable);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "invalid_local_alias_shouldnt_shadow_imported_type")
{
    ScopedFastFlag _{FFlag::LuauNameConstraintRestrictRecursiveTypes, true};

    fileResolver.source["game/A"] = R"(
        export type bad<T> = {T}
        return {}
    )";

    fileResolver.source["game/B"] = R"(
        local a_mod = require(game.A)
        type bad<T> = {bad<{T}>}
        type fine<T> = a_mod.bad<T>
        local f: fine<number>
    )";

    CheckResult result = getFrontend().check("game/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(get<RecursiveRestraintViolation>(result.errors[0]));

    ModulePtr b = getFrontend().moduleResolver.getModule("game/B");
    REQUIRE(b != nullptr);
    std::optional<TypeId> fType = requireType(b, "f");
    REQUIRE(fType);
    // The important thing here is that it isn't *error-type*, since that would mean that the local definition of `bad` is shadowing the imported one
    CHECK(toString(*fType) == "fine<number>");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "invalid_alias_should_export_as_error_type")
{
    ScopedFastFlag _{FFlag::LuauNameConstraintRestrictRecursiveTypes, true};

    fileResolver.source["game/A"] = R"(
        export type bad<T> = {bad<{T}>}
        return {}
    )";

    fileResolver.source["game/B"] = R"(
        local a_mod = require(game.A)
        local f: a_mod.bad<number>
    )";

    CheckResult result = getFrontend().check("game/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(get<RecursiveRestraintViolation>(result.errors[0]));

    ModulePtr b = getFrontend().moduleResolver.getModule("game/B");
    REQUIRE(b != nullptr);
    std::optional<TypeId> fType = requireType(b, "f");
    REQUIRE(fType);
    if (FFlag::LuauSolverV2)
        CHECK(toString(*fType) == "*error-type*");
    else
        CHECK(toString(*fType) == "bad<number>");
}

TEST_SUITE_END();
