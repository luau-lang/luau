// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "Luau/AstQuery.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Scope.h"
#include "Luau/TypeInfer.h"
#include "Luau/TypeVar.h"

#include "Fixture.h"

#include "doctest.h"

LUAU_FASTFLAG(LuauInstantiateInSubtyping)

using namespace Luau;

LUAU_FASTFLAG(LuauSpecialTypesAsterisked)

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

    CheckResult aResult = frontend.check("game/A");
    LUAU_REQUIRE_NO_ERRORS(aResult);

    CheckResult bResult = frontend.check("game/B");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    ModulePtr b = frontend.moduleResolver.modules["game/B"];
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

    fileResolver.source["game/B"] = R"(
        local Hooty = require(game.A)

        local h -- free!
        local i = Hooty.hooty(h)
    )";

    CheckResult aResult = frontend.check("game/A");
    dumpErrors(aResult);
    LUAU_REQUIRE_NO_ERRORS(aResult);

    CheckResult bResult = frontend.check("game/B");
    dumpErrors(bResult);
    LUAU_REQUIRE_NO_ERRORS(bResult);

    ModulePtr b = frontend.moduleResolver.modules["game/B"];

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

    CheckResult bResult = frontend.check("workspace/B");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    ModulePtr b = frontend.moduleResolver.modules["workspace/B"];
    REQUIRE(b != nullptr);

    TypeId hType = requireType(b, "h");
    REQUIRE_MESSAGE(bool(get<TableTypeVar>(hType)), "Expected table but got " << toString(hType));
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

    CheckResult result = frontend.check("game/B");

    ModulePtr bModule = frontend.moduleResolver.getModule("game/B");
    REQUIRE(bModule != nullptr);

    TypeId f = follow(requireType(bModule, "f"));

    const FunctionTypeVar* ftv = get<FunctionTypeVar>(f);
    REQUIRE(ftv);

    auto iter = begin(ftv->argTypes);
    auto endIter = end(ftv->argTypes);

    REQUIRE(iter == endIter);
    REQUIRE(iter.tail());

    CHECK(get<VariadicTypePack>(*iter.tail()));
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

    frontend.check("game/Workspace/A");
    frontend.check("game/Workspace/B");

    ModulePtr aModule = frontend.moduleResolver.modules["game/Workspace/A"];
    ModulePtr bModule = frontend.moduleResolver.modules["game/Workspace/B"];

    CHECK(aModule->errors.empty());
    REQUIRE_EQ(1, bModule->errors.size());
    CHECK_MESSAGE(get<IllegalRequire>(bModule->errors[0]), "Should be IllegalRequire: " << toString(bModule->errors[0]));

    auto hootyType = requireType(bModule, "Hooty");

    if (FFlag::LuauSpecialTypesAsterisked)
        CHECK_EQ("*error-type*", toString(hootyType));
    else
        CHECK_EQ("<error-type>", toString(hootyType));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "warn_if_you_try_to_require_a_non_modulescript")
{
    fileResolver.source["Modules/A"] = "";
    fileResolver.sourceTypes["Modules/A"] = SourceCode::Local;

    fileResolver.source["Modules/B"] = R"(
        local M = require(script.Parent.A)
    )";

    CheckResult result = frontend.check("Modules/B");

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

    CheckResult result = frontend.check("game/B");
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

    CheckResult result = frontend.check("game/B");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK_EQ("Type '{| def: number |}' could not be converted into 'string'", toString(result.errors[0]));
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

    CheckResult aResult = frontend.check("game/A");
    LUAU_REQUIRE_ERRORS(aResult);

    CheckResult result = check(R"(
local ModuleA = require(game.A)
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    std::optional<TypeId> oty = requireType("ModuleA");

    if (FFlag::LuauSpecialTypesAsterisked)
        CHECK_EQ("*error-type*", toString(*oty));
    else
        CHECK_EQ("<error-type>", toString(*oty));
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

    CheckResult result = frontend.check("game/B");
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

    CheckResult result = frontend.check("game/B");
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

    CheckResult result = frontend.check("game/B");
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
    -- introduce BoundTypeVar to imported type
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
    -- introduce boundTo TableTypeVar to imported type
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

    CheckResult result = frontend.check("game/C");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(toString(result.errors[0]), R"(Type 'T' from 'game/A' could not be converted into 'T' from 'game/B'
caused by:
  Property 'x' is not compatible. Type 'number' could not be converted into 'string')");
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

    CheckResult result = frontend.check("game/D");
    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK_EQ(toString(result.errors[0]), R"(Type 'T' from 'game/B' could not be converted into 'T' from 'game/C'
caused by:
  Property 'x' is not compatible. Type 'number' could not be converted into 'string')");
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

    CheckResult result = frontend.check("game/B");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
