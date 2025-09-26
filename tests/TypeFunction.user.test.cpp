// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "ClassFixture.h"
#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(DebugLuauEqSatSimplification)
LUAU_FASTFLAG(LuauInstantiateResolvedTypeFunctions)

TEST_SUITE_BEGIN("UserDefinedTypeFunctionTests");

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_nil_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_nil(arg)
            return arg
        end
        type type_being_serialized = nil
        local function ok(idx: serialize_nil<type_being_serialized>): nil return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_nil_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function getnil()
            local ty = types.singleton(nil)
            if ty:is("nil") then
                return ty
            end
            -- this should never be returned
            return types.string
        end
        local function ok(idx: getnil<>): nil return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_unknown_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_unknown(arg)
            return arg
        end
        type type_being_serialized = unknown
        local function ok(idx: serialize_unknown<type_being_serialized>): unknown return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_unknown_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function getunknown()
            local ty = types.unknown
            if ty:is("unknown") then
                return ty
            end
            -- this should never be returned
            return types.string
        end
        local function ok(idx: getunknown<>): unknown return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_never_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_never(arg)
            return arg
        end
        type type_being_serialized = never
        local function ok(idx: serialize_never<type_being_serialized>): never return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_never_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function getnever()
            local ty = types.never
            if ty:is("never") then
                return ty
            end
            -- this should never be returned
            return types.string
        end
        local function ok(idx: getnever<>): never return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_any_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_any(arg)
            return arg
        end
        type type_being_serialized = any
        local function ok(idx: serialize_any<type_being_serialized>): any return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_any_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function getany()
            local ty = types.any
            if ty:is("any") then
                return ty
            end
            -- this should never be returned
            return types.string
        end
        local function ok(idx: getany<>): any return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_boolean_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_bool(arg)
            return arg
        end
        type type_being_serialized = boolean
        local function ok(idx: serialize_bool<type_being_serialized>): boolean return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_boolean_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function getboolean()
            local ty = types.boolean
            if ty:is("boolean") then
                return ty
            end
            -- this should never be returned
            return types.string
        end
        local function ok(idx: getboolean<>): boolean return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_number_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_num(arg)
            return arg
        end
        type type_being_serialized = number
        local function ok(idx: serialize_num<type_being_serialized>): number return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_number_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function getnumber()
            local ty = types.number
            if ty:is("number") then
                return ty
            end
            -- this should never be returned
            return types.string
        end
        local function ok(idx: getnumber<>): number return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "thread_and_buffer_types")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    LUAU_REQUIRE_NO_ERRORS(check(R"(
        type function work_with_thread(x)
            if x:is("thread") then
                return types.thread
            end
            return types.string
        end
        type X = thread
        local function ok(idx: work_with_thread<X>): thread return idx end
    )"));

    LUAU_REQUIRE_NO_ERRORS(check(R"(
        type function work_with_buffer(x)
            if x:is("buffer") then
                return types.buffer
            end
            return types.string
        end
        type X = buffer
        local function ok(idx: work_with_buffer<X>): buffer return idx end
    )"));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_string_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_str(arg)
            return arg
        end
        type type_being_serialized = string
        local function ok(idx: serialize_str<type_being_serialized>): string return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_string_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function getstring()
            local ty = types.string
            if ty:is("string") then
                return ty
            end
            -- this should never be returned
            return types.boolean
        end
        local function ok(idx: getstring<>): string return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_boolsingleton_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_boolsingleton(arg)
            return arg
        end
        type type_being_serialized = true
        local function ok(idx: serialize_boolsingleton<type_being_serialized>): true return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_boolsingleton_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function getboolsingleton()
            local ty = types.singleton(true)
            if ty:is("singleton") and ty:value() then
                return ty
            end
            -- this should never be returned
            return types.string
        end
        local function ok(idx: getboolsingleton<>): true return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_strsingleton_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_strsingleton(arg)
            return arg
        end
        type type_being_serialized = "popcorn and movies!"
        local function ok(idx: serialize_strsingleton<type_being_serialized>): "popcorn and movies!" return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_strsingleton_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function getstrsingleton()
            local ty = types.singleton("hungry hippo")
            if ty:is("singleton") and ty:value() == "hungry hippo" then
                return ty
            end
            -- this should never be returned
            return types.number
        end
        local function ok(idx: getstrsingleton<>): "hungry hippo" return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_union_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_union(arg)
            return arg
        end
        type type_being_serialized = number | string | boolean
        -- forcing an error here to check the exact type of the union
        local function ok(idx: serialize_union<type_being_serialized>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "boolean | number | string");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_optional_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function numberhuh()
            return types.optional(types.number)
        end
        -- forcing an error here to check the exact type of the union
        local function ok(idx: numberhuh<>): never return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "number?");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_optional_works_on_unions")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function foobar()
            local ty = types.unionof(types.string, types.number, types.boolean)
            return types.optional(ty)
        end
        -- forcing an error here to check the exact type of the union
        local function ok(idx: foobar<>): never return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "(boolean | number | string)?");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_union_methods_work")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type function getunion()
            local ty = types.unionof(types.string, types.number, types.boolean)
            if ty:is("union") then
                -- creating a copy of `ty`
                local arr = {}
                for _, value in ty:components() do
                    table.insert(arr, value)
                end
                return types.unionof(table.unpack(arr))
            end
            -- this should never be returned
            return types.number
        end
        -- forcing an error here to check the exact type of the union
        local function ok(idx: getunion<>): never return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "boolean | number | string");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_intersection_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_intersection(arg)
            return arg
        end
        type type_being_serialized = { boolean: boolean, number: number } & { boolean: boolean, string: string }
        -- forcing an error here to check the exact type of the intersection
        local function ok(idx: serialize_intersection<type_being_serialized>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "{ boolean: boolean, number: number } & { boolean: boolean, string: string }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_intersection_methods_work")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type function getintersection()
            local tbl1 = types.newtable(nil, nil, nil)
            tbl1:setproperty(types.singleton("boolean"), types.boolean) -- {boolean: boolean}
            tbl1:setproperty(types.singleton("number"), types.number) -- {boolean: boolean, number: number}
            local tbl2 = types.newtable(nil, nil, nil)
            tbl2:setproperty(types.singleton("boolean"), types.boolean) -- {boolean: boolean}
            tbl2:setproperty(types.singleton("string"), types.string) -- {boolean: boolean, string: string}
            local ty = types.intersectionof(tbl1, tbl2)
            if ty:is("intersection") then
                -- creating a copy of `ty`
                local arr = {}
                for index, value in ty:components() do
                    table.insert(arr, value)
                end
                return types.intersectionof(table.unpack(arr))
            end
            -- this should never be returned
            return types.string
        end
        -- forcing an error here to check the exact type of the intersection
        local function ok(idx: getintersection<>): never return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "{ boolean: boolean, number: number } & { boolean: boolean, string: string }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_negation_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function getnegation()
            local ty = types.negationof(types.string)
            if ty:is("negation") then
                return ty
            end
            -- this should never be returned
            return types.number
        end

        -- forcing an error here to check the exact type of the negation
        local function ok(idx: getnegation<>): never return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "~string");
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_negation_inner")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function pass(t)
    return types.negationof(t):inner()
end

type function fail(t)
    return t:inner()
end

local function ok(idx: pass<number>): number return idx end
local function notok(idx: fail<number>): never return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(
        toString(result.errors[0]) ==
        R"('fail' type function errored at runtime: [string "fail"]:7: type.inner: cannot call inner method on non-negation type: `number` type)"
    );
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_table_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_table(arg)
            return arg
        end
        type type_being_serialized = { boolean: boolean, number: number, [string]: number }
        -- forcing an error here to check the exact type of the table
        local function ok(idx: serialize_table<type_being_serialized>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "{ [string]: number, boolean: boolean, number: number }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_table_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function gettable()
            local indexer = {
                index = types.number,
                readresult = types.boolean,
                writeresult = types.boolean,
            }
            local ty = types.newtable(nil, indexer, nil) -- {[number]: boolean}
            ty:setproperty(types.singleton("string"), types.number) -- {string: number, [number] = boolean}
            ty:setproperty(types.singleton("number"), types.string) -- {string: number, number: string, [number] = boolean}
            ty:setproperty(types.singleton("string"), nil) -- {number: string, [number] = boolean}
            local ret = types.newtable(nil, nil, nil) -- {}
            -- creating a copy of `ty`
            for k, v in ty:properties() do
                ret:setreadproperty(k, v.read)
                ret:setwriteproperty(k, v.write)
            end
            if ret:is("table") then
                ret:setindexer(types.boolean, types.string) -- {number: string, [boolean] = string}
                return ret -- {number: string, [boolean] = string}
            end
            -- this should never be returned
            return types.number
        end
        -- forcing an error here to check the exact type of the table
        local function ok(idx: gettable<>): never return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "{ [boolean]: string, number: string }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_metatable_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function getmetatable()
            local indexer = {
                index = types.number,
                readresult = types.boolean,
                writeresult = types.boolean,
            }
            local ty = types.newtable(nil, indexer, nil) -- {[number]: boolean}
            ty:setproperty(types.singleton("string"), types.number) -- {string: number, [number]: boolean}
            local metatbl = types.newtable(nil, nil, ty) -- { {  }, @metatable { [number]: boolean, string: number } }
            metatbl:setmetatable(types.newtable(nil, indexer, nil)) -- { {  }, @metatable { [number]: boolean } }
            local ret = metatbl:metatable()
            if metatbl:is("table") and metatbl:metatable() then
                return ret -- { @metatable { [number]: boolean } }
            end
            -- this should never be returned
            return types.number
        end
        -- forcing an error here to check the exact type of the metatable
        local function ok(idx: getmetatable<>): never return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "{boolean}");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_function_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_func(arg)
            return arg
        end
        type type_being_serialized = (boolean, number, nil) -> (...string)
        local function ok(idx: serialize_func<type_being_serialized>): (boolean, number, nil) -> (...string) return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_function_methods_work")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type function getfunction()
            local ty = types.newfunction(nil, nil) -- () -> ()
            ty:setparameters({types.string, types.number}, nil) -- (string, number) -> ()
            ty:setreturns(nil, types.boolean) -- (string, number) -> (...boolean)
            if ty:is("function") then
                -- creating a copy of `ty` parameters
                local arr: {type} = {}
                local args = ty:parameters().head
                if args then
                    for index, val in args do
                        table.insert(arr, val)
                    end
                end
                return types.newfunction({head = arr}, ty:returns()) -- (string, number) -> (...boolean)
            end
            -- this should never be returned
            return types.number
        end
        local function ok(idx: getfunction<>): never return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "(string, number) -> (...boolean)");
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_class_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_class(arg)
            return arg
        end
        local function ok(idx: serialize_class<BaseClass>): BaseClass return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_class_serialization_works2")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_class(arg)
            return arg
        end
        local function ok(idx: serialize_class<typeof(confusingBaseClassInstance)>): typeof(confusingBaseClassInstance) return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_class_methods_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function getclass(arg)
            local props = arg:properties()
            local indexer = arg:indexer()
            local metatable = arg:metatable()
            return types.newtable(props, indexer, metatable)
        end
        -- forcing an error here to check the exact type of the metatable
        local function ok(idx: getclass<BaseClass>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "{ BaseField: number, read BaseMethod: (BaseClass, number) -> (), read Touched: Connection }");
}

TEST_CASE_FIXTURE(ExternTypeFixture, "write_of_readonly_is_nil")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function getclass(arg)
            local props = arg:properties()
            local table = types.newtable(props)
            local singleton = types.singleton("BaseMethod")

            if table:writeproperty(singleton) then
                return types.singleton(true)
            else
                return types.singleton(false)
            end
        end
        -- forcing an error here to check the exact type of the metatable
        local function ok(idx: getclass<BaseClass>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "false");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_check_mutability")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function checkmut()
            local indexer = {
                index = types.number,
                readresult = types.boolean,
                writeresult = types.boolean,
            }
            local ty = types.newtable(nil, indexer, nil) -- {[number]: boolean}
            ty:setproperty(types.singleton("string"), types.number) -- {string: number, [number]: boolean}
            local metatbl = types.newtable(nil, nil, ty) -- { {  }, @metatable { [number]: boolean, string: number } }
            -- mutate the table
            ty:setproperty(types.singleton("string"), nil) -- {[number]: boolean}
            if metatbl:is("table") and metatbl:metatable() then
                return metatbl -- { @metatable { [number]: boolean }, { } }
            end
            -- this should never be returned
            return types.number
        end
        local function ok(idx: checkmut<>): never return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "{ @metatable {boolean}, {  } }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_copy_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function getcopy()
            local indexer = {
                index = types.number,
                readresult = types.boolean,
                writeresult = types.boolean,
            }
            local ty = types.newtable(nil, indexer, nil) -- {[number]: boolean}
            ty:setproperty(types.singleton("string"), types.number) -- {string: number, [number]: boolean}
            local metaty = types.newtable(nil, nil, ty) -- { {  }, @metatable { [number]: boolean, string: number } }
            local copy = types.copy(metaty)
            -- mutate the table
            ty:setproperty(types.singleton("string"), nil) -- {[number]: boolean}
            if copy:is("table") and copy:metatable() then
                return copy -- { {  }, @metatable { [number]: boolean, string: number } }
            end
            -- this should never be returned
            return types.number
        end
        local function ok(idx: getcopy<>): never return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "{ @metatable { [number]: boolean, string: number }, {  } }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_simple_cyclic_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_cycle(arg)
            return arg
        end
        type basety = {
            first: basety2
        }
        type basety2 = {
            second: basety
        }
        local function ok(idx: serialize_cycle<basety>): basety return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_createtable_bad_metatable")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function badmetatable()
            return types.newtable(nil, nil, types.number)
        end
        local function bad(arg: badmetatable<>) end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result); // There are 2 type function uninhabited error, 2 user defined type function error
    UserDefinedTypeFunctionError* e = get<UserDefinedTypeFunctionError>(result.errors[0]);
    REQUIRE(e);
    CHECK(
        e->message == "'badmetatable' type function errored at runtime: [string \"badmetatable\"]:3: types.newtable: expected to be given a table "
                      "type as a metatable, but got number instead"
    );
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_complex_cyclic_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function serialize_cycle2(arg)
            return arg
        end
        type Employee = {
            name: string,
            department: Department?
        }
        type Department = {
            name: string,
            manager: Employee?,
            employees: { Employee },
            company: Company?
        }
        type Company = {
            name: string,
            departments: { Department }
        }
        local function ok(idx: serialize_cycle2<Company>): Company return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_user_error_is_reported")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type function errors_if_string(arg)
            if arg:is("string") then
                local a = 1
                error("We are in a math class! not english")
            end
            return arg
        end
        local function ok(idx: errors_if_string<string>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result); // There are 2 type function uninhabited error, 2 user defined type function error
    UserDefinedTypeFunctionError* e = get<UserDefinedTypeFunctionError>(result.errors[0]);
    REQUIRE(e);
    CHECK(e->message == "'errors_if_string' type function errored at runtime: [string \"errors_if_string\"]:5: We are in a math class! not english");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_type_overrides_call_metamethod")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type function hello(arg)
            error(type(arg))
        end
        local function ok(idx: hello<string>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result); // There are 2 type function uninhabited error, 2 user defined type function error
    UserDefinedTypeFunctionError* e = get<UserDefinedTypeFunctionError>(result.errors[0]);
    REQUIRE(e);
    CHECK(e->message == "'hello' type function errored at runtime: [string \"hello\"]:3: userdata");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_type_overrides_eq_metamethod")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function hello()
            local p1 = types.string
            local p2 = types.string
            local t1 = types.newtable(nil, nil, nil)
            t1:setproperty(types.singleton("string"), types.boolean)
            t1:setmetatable(t1)
            local t2 = types.newtable(nil, nil, nil)
            t2:setproperty(types.singleton("string"), types.boolean)
            t1:setmetatable(t1)
            if p1 == p2 and t1 == t2 then
                return types.number
            end
            return types.unknown
        end
        local function ok(idx: hello<>): number return idx end
    )");

    LUAU_CHECK_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_function_type_cant_call_get_props")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function hello(arg)
            local arr = arg:properties()
        end
        local function ok(idx: hello<() -> ()>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result); // There are 2 type function uninhabited error, 2 user defined type function error
    UserDefinedTypeFunctionError* e = get<UserDefinedTypeFunctionError>(result.errors[0]);
    REQUIRE(e);
    CHECK(
        e->message == "'hello' type function errored at runtime: [string \"hello\"]:3: type.properties: expected self to be either a table or class, "
                      "but got function instead"
    );
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_calling_each_other")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function foo()
            return "hi"
        end
        type function bar()
            return types.singleton(foo())
        end
        local function ok(idx: bar<>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "\"hi\"");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_calling_each_other_2")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function first(arg)
            return arg
        end
        type function second(arg)
            return types.singleton(first(arg))
        end
        type function third()
            return second("hi")
        end
        local function ok(idx: third<>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "\"hi\"");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_calling_each_other_3")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        -- this function should not see 'fourth' function when invoked from 'third' that sees it
        type function first(arg)
            return fourth(arg)
        end
        type function second(arg)
            return types.singleton(first(arg))
        end

        do
            type function fourth(arg)
                return arg
            end
            type function third()
                return second("hi")
            end
            local function ok(idx: third<>): nil return idx end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(5, result);
    CHECK(toString(result.errors[0]) == R"(Unknown global 'fourth')");
    CHECK(toString(result.errors[1]) == R"('third' type function errored at runtime: [string "first"]:4: attempt to call a nil value)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_calling_each_other_unordered")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function bar()
            return types.singleton(foo())
        end
        type function foo()
            return "hi"
        end
        local function ok(idx: bar<>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "\"hi\"");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_no_shared_state")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function foo()
            if not glob then
                glob = 'a'
            else
                glob ..= 'b'
            end

            return glob
        end
        type function bar(prefix)
            return types.singleton(prefix:value() .. foo())
        end
        local function ok1(idx: bar<'x'>): nil return idx end
        local function ok2(idx: bar<'y'>): nil return idx end
    )");

    // We are only checking first errors, others are mostly duplicates
    LUAU_REQUIRE_ERROR_COUNT(9, result);
    CHECK(toString(result.errors[0]) == R"(Unknown global 'glob')");
    CHECK(toString(result.errors[1]) == R"('bar' type function errored at runtime: [string "foo"]:4: attempt to modify a readonly table)");
    CHECK(toString(result.errors[2]) == R"(Type function instance bar<"x"> is uninhabited)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_math_reset")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type function foo(x)
            return types.singleton(tostring(math.random(1, 100)))
        end
        local x: foo<'a'> = ('' :: any) :: foo<'b'>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_optionify")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type function optionify(tbl)
            if not tbl:is("table") then
                error("Argument is not a table")
            end
            for k, v in tbl:properties() do
                tbl:setproperty(k, types.unionof(v.read, types.singleton(nil)))
            end
            return tbl
        end
        type Person = {
            name: string,
            age: number,
            alive: boolean
        }
        local function ok(idx: optionify<Person>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "{ age: number?, alive: boolean?, name: string? }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_calling_illegal_global")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function illegal(arg)
            gcinfo() -- this should error

            return arg -- this should not be reached
        end

        local function ok(idx: illegal<number>): nil return idx end
    )");

    // We are only checking first errors, others are mostly duplicates
    LUAU_REQUIRE_ERROR_COUNT(5, result);
    CHECK(toString(result.errors[0]) == R"(Unknown global 'gcinfo')");
    CHECK(
        toString(result.errors[1]) ==
        R"('illegal' type function errored at runtime: [string "illegal"]:3: this function is not supported in type functions)"
    );
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_recursion_and_gc")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type function foo(tbl)
            local count = 0
            for k,v in tbl:properties() do count += 1 end
            if count < 100 then
                tbl:setproperty(types.singleton(`m{count}`), types.string)
                foo(tbl)
            end
            for i = 1,100 do table.create(10000) end
            return tbl
        end
        type Test = {}
        local function ok(idx: foo<Test>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_recovery_no_upvalues")
{
    ScopedFastFlag solverV2{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        local var

        type function save_upvalue(arg)
            var = 1
            return arg
        end

        type test = "test"
        local function ok(idx: save_upvalue<test>): "test"
            return idx
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == R"(Type function cannot reference outer local 'var')");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_follow")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type t0 = any
        type function t0()
            return types.any
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == R"(Redefinition of type 't0', previously defined at line 2)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_strip_indexer")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type function stripindexer(tbl)
            if not tbl:is("table") then
                error("can only strip the indexer on a table!")
            end
            tbl:setindexer(types.never, types.never)
            return tbl
        end

        type map = { [number]: string, foo: string }
        -- forcing an error here to check the exact type
        local function ok(tbl: stripindexer<map>): never return tbl end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypeMismatch* tm = get<TypeMismatch>(result.errors[0]);
    REQUIRE(tm);
    CHECK(toString(tm->givenType) == "{ foo: string }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "no_type_methods_on_types")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type function test(x)
            return if (types :: any).is(x, "number") then types.string else types.boolean
        end
        local function ok(tbl: test<number>): never return tbl end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(toString(result.errors[0]) == R"('test' type function errored at runtime: [string "test"]:3: attempt to call a nil value)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "no_types_functions_on_type")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function test(x)
            return x.singleton("a")
        end
        local function ok(tbl: test<number>): never return tbl end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(toString(result.errors[0]) == R"('test' type function errored at runtime: [string "test"]:3: attempt to call a nil value)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "no_metatable_writes")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function test(x)
            local a = x.__index
            a.is = function() return false end
            return types.singleton(x.is("number"))
        end
        local function ok(tbl: test<number>): never return tbl end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(toString(result.errors[0]) == R"('test' type function errored at runtime: [string "test"]:4: attempt to index nil with 'is')");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "no_eq_field")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function test(x)
            return types.singleton(x.__eq(x, types.number))
        end
        local function ok(tbl: test<number>): never return tbl end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(toString(result.errors[0]) == R"('test' type function errored at runtime: [string "test"]:3: attempt to call a nil value)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "tag_field")
{
    ScopedFastFlag sff{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function test(x)
            return types.singleton(x.tag)
        end

        local function ok1(tbl: test<number>): never return tbl end
        local function ok2(tbl: test<string>): never return tbl end
        local function ok3(tbl: test<{}>): never return tbl end
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);

    CHECK(toString(result.errors[0]) == "Type '\"number\"' could not be converted into 'never'");
    CHECK(toString(result.errors[1]) == "Type '\"string\"' could not be converted into 'never'");
    CHECK(toString(result.errors[2]) == "Type '\"table\"' could not be converted into 'never'");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "metatable_serialization")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function makemttbl()
            local metaprops = {
                [types.singleton("ma")] = types.boolean
            }
            local mt = types.newtable(metaprops)

            local props = {
                [types.singleton("a")] = types.number
            }
            return types.newtable(props, nil, mt)
        end

        type function id(x)
            return x
        end

        local a: number = {} :: id<makemttbl<>>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == R"(Type '{ @metatable { ma: boolean }, { a: number } }' could not be converted into 'number')");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "nonstrict_mode")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
--!nonstrict
type function foo() return types.string end
local a: foo<> = "a"
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "implicit_export")
{
    if (!FFlag::LuauSolverV2)
        return;

    fileResolver.source["game/A"] = R"(
type function concat(a: type, b: type)
    local as = a:value()
    local bs = b:value()
    assert(typeof(as) == "string")
    assert(typeof(bs) == "string")
    return types.singleton(as .. bs)
end
export type Concat<T, U> = concat<T, U>
local a: concat<'first', 'second'>
return {}
    )";

    CheckResult aResult = getFrontend().check("game/A");
    LUAU_REQUIRE_NO_ERRORS(aResult);

    CHECK(toString(requireType("game/A", "a")) == R"("firstsecond")");

    CheckResult bResult = check(R"(
local Test = require(game.A);
local b: Test.Concat<'third', 'fourth'>
    )");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    CHECK(toString(requireType("b")) == R"("thirdfourth")");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "local_scope")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function foo()
    return "hi"
end
local function test()
    type function bar()
        return types.singleton(foo())
    end

    return ("" :: any) :: bar<>
end
local a = test()
    )");
    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(toString(requireType("a")) == R"("hi")");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "explicit_export")
{
    if (!FFlag::LuauSolverV2)
        return;

    fileResolver.source["game/A"] = R"(
export type function concat(a: type, b: type)
    local as = a:value()
    local bs = b:value()
    assert(typeof(as) == "string")
    assert(typeof(bs) == "string")
    return types.singleton(as .. bs)
end
local a: concat<'first', 'second'>
return {}
    )";

    CheckResult aResult = getFrontend().check("game/A");
    LUAU_REQUIRE_NO_ERRORS(aResult);

    CHECK(toString(requireType("game/A", "a")) == R"("firstsecond")");

    CheckResult bResult = check(R"(
local Test = require(game.A);
local b: Test.concat<'third', 'fourth'>
    )");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    CHECK(toString(requireType("b")) == R"("thirdfourth")");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "print_to_error")
{
    ScopedFastFlag solverV2{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function t0(a)
            print("Where does this go")
            print(a.tag)
            return types.any
        end
        local a: t0<string>
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    CHECK(toString(result.errors[0]) == R"(Where does this go)");
    CHECK(toString(result.errors[1]) == R"(string)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "print_to_error_plus_error")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
        type function t0(a)
            print("Where does this go")
            print(a.tag)
            error("test")
        end
        local a: t0<string>
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(toString(result.errors[0]) == R"(Where does this go)");
    CHECK(toString(result.errors[1]) == R"(string)");
    CHECK(toString(result.errors[2]) == R"('t0' type function errored at runtime: [string "t0"]:5: test)");
    CHECK(toString(result.errors[3]) == R"(Type function instance t0<string> is uninhabited)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "print_to_error_plus_no_result")
{
    ScopedFastFlag solverV2{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function t0(a)
            print("Where does this go")
            print(a.tag)
        end
        local a: t0<string>
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(toString(result.errors[0]) == R"(Where does this go)");
    CHECK(toString(result.errors[1]) == R"(string)");
    CHECK(toString(result.errors[2]) == R"('t0' type function: returned a non-type value)");
    CHECK(toString(result.errors[3]) == R"(Type function instance t0<string> is uninhabited)");
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_serialization_1")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function pass(arg)
    return arg
end

type test = <T, U>(T, { x: <T>(y: T) -> (), y: U }, U) -> ()

local function ok(idx: pass<test>): test return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_serialization_2")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function pass(arg)
    return arg
end

type test = <T, U...>(T) -> (T, U...)

local function ok(idx: pass<test>): test return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_serialization_3")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function pass(arg)
    return arg
end

local function m(a, b)
    return {x = a, y = b}
end

type test = typeof(m)

local function ok(idx: pass<test>): test return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_cloning_1")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function pass(arg)
    return types.copy(arg)
end

type test = <T, U>(T, { x: <T>(y: T) -> (), y: U }, U) -> ()

local function ok(idx: pass<test>): test return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_cloning_2")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function pass(arg)
    return types.copy(arg)
end

type test = <T, U...>(T) -> (T, U...)

local function ok(idx: pass<test>): test return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_equality")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function pass(arg)
    return types.singleton(types.copy(arg) == arg)
end

type test = <T, U...>(T) -> (T, U...)

local function ok(idx: pass<test>): true return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_api_1")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function pass(arg)
    local generics = arg:generics()
    local T = generics[1]
    return types.newfunction({ head = {T} }, { head = {T} }, {T})
end

type test = <T, U>(T, { x: <T>(y: T) -> (), y: U }, U) -> ()

local function ok(idx: pass<test>): <T>(T) -> (T) return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_api_2")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function pass(arg)
    local generics = arg:generics()
    local T = generics[1]
    local f = types.newfunction()
    f:setparameters({T, T});
    f:setreturns({T});
    f:setgenerics({T});
    return f
end

type test = <T, U>(T, { x: <T>(y: T) -> (), y: U }, U) -> ()

local function ok(idx: pass<test>): <T>(T, T) -> (T) return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_api_3")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
type function pass()
    local T = types.generic("T")
    assert(T.tag == "generic")
    assert(T:name() == "T")
    assert(T:ispack() == false)

    local Us, Vs = types.generic("U", true), types.generic("V", true)
    assert(Us.tag == "generic")
    assert(Us:name() == "U")
    assert(Us:ispack() == true)

    local f = types.newfunction()
    f:setparameters({T}, Us);
    f:setreturns({T}, Vs);
    f:setgenerics({T, Us, Vs});
    return f
end

local function ok(idx: pass<>): <T, U..., V...>(T, U...) -> (T, V...) return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_api_4")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function pass()
    local T, U = types.generic("T"), types.generic("U")

    -- <T>(T) -> ()
    local func = types.newfunction({ head = {T} }, {}, {T});

    -- { x: <T>(T) -> (), y: U }
    local tbl = types.newtable({ [types.singleton("x")] = func, [types.singleton("y")] = U })

    -- <T, U>(T, { x: <T>(T) -> (), y: U }, U) -> ()
    return types.newfunction({ head = {T, tbl, U } }, {}, {T, U})
end

type test = <T, U>(T, { x: <T>(y: T) -> (), y: U }, U) -> ()

local function ok(idx: pass<>): test return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_api_5")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function pass()
    local T = types.generic("T")
    return types.newfunction({ head = {T} }, {}, {types.copy(T)})
end

local function ok(idx: pass<>): <T>(T) -> () return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_api_6")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function pass(arg)
    local generics = arg:generics()
    local T, U = generics[1], generics[2]
    local f = types.newfunction()
    f:setparameters({T});
    f:setreturns({U});
    f:setgenerics({T, U});
    return f
end

local function m(a, b)
    return {x = a, y = b}
end

type test = typeof(m)

local function ok(idx: pass<test>): <T, U>(T) -> (U) return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_api_7")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function pass(arg)
    local p, r = arg:parameters(), arg:returns()
    local f = types.newfunction()
    f:setparameters(p.head, p.tail);
    f:setreturns(r.head, r.tail);
    f:setgenerics(arg:generics());
    return f
end

type test = <T, U...>(T, U...) -> (T, U...)

local function ok(idx: pass<test>): <T, U...>(T, U...) -> (T, U...) return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_api_8")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function pass(arg)
    local p, r = arg:parameters(), arg:returns()
    local f = types.newfunction()
    f:setparameters(p.head, p.tail);
    f:setreturns(r.head, r.tail);
    f:setgenerics(arg:generics());
    return f
end

type test = <U...>(U...) -> (U...)

local function ok(idx: pass<test>): <T>(T, T) -> (T, T) return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_equality_2")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function get()
    local T, Us = types.generic("T"), types.generic("U", true)

    local tbl1 = types.newtable({ [types.singleton("x")] = T })
    local tbl2 = types.newtable({ [types.singleton("x")] = Us }) -- it is possible to have invalid types in-flight

    return types.singleton(tbl1 == tbl2)
end

local function ok(idx: get<>): false return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_api_error_1")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function get()
    local T, Us = types.generic("T"), types.generic("U", true)
    return types.newfunction({}, {}, {Us, T})
end
local function ok(idx: get<>): false return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(
        toString(result.errors[0]) ==
        R"('get' type function errored at runtime: [string "get"]:4: types.newfunction: generic type cannot follow a generic pack)"
    );
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_api_error_2")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function get()
    local T, Us = types.generic("T"), types.generic("U", true)
    return types.newfunction({ head = {T} }, {}, {})
end
local function ok(idx: get<>): false return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(toString(result.errors[0]) == R"(Generic type 'T' is not in a scope of the active generic function)");
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_api_error_3")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function get()
    local T, U = types.generic("T"), types.generic("U")

    -- <U>(U) -> ()
    local func = types.newfunction({ head = {U} }, {}, {U});

    -- broken: <T>(T, <U>(U) -> (), U) -> ()
    return types.newfunction({ head = {T, func, U } }, {}, {T})
end
local function ok(idx: get<>): false return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(toString(result.errors[0]) == R"(Generic type 'U' is not in a scope of the active generic function)");
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_api_error_4")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function get()
    local T, Us = types.generic("T"), types.generic("U", true)
    return types.newfunction({ head = {T} }, { tail = Us }, {T, T})
end
local function ok(idx: get<>): false return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(toString(result.errors[0]) == R"(Duplicate type parameter 'T')");
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_api_error_5")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function get()
    local T, Ts = types.generic("T"), types.generic("T", true)
    return types.newfunction({ head = {T} }, { tail = Ts }, {T, Ts})
end
local function ok(idx: get<>): false return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(toString(result.errors[0]) == R"(Duplicate type parameter 'T')");
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_api_error_6")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function get()
    local T, Us = types.generic("T"), types.generic("U", true)
    return types.newfunction({ head = {Us} }, {}, {T, Us})
end
local function ok(idx: get<>): false return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(toString(result.errors[0]) == R"(Generic type pack 'U...' cannot be placed in a type position)");
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_generic_api_error_7")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function get()
    local T, Us = types.generic("T"), types.generic("U", true)
    return types.newfunction({ tail = Us }, {}, {T})
end
local function ok(idx: get<>): false return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(toString(result.errors[0]) == R"(Generic type pack 'U...' is not in a scope of the active generic function)");
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_variadic_api")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function pass(arg)
    local p, r = arg:parameters(), arg:returns()
    local f = types.newfunction()
    f:setparameters({p.tail}, p.head[1]);
    f:setreturns({r.tail}, r.head[1]);
    return f
end

type test = (string, ...number) -> (number, ...string)

local function ok(idx: pass<test>): (number, ...string) -> (string, ...number) return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_eqsat_opaque")
{
    if (!FFlag::LuauSolverV2)
        return;

    ScopedFastFlag sffs[] = {{FFlag::DebugLuauEqSatSimplification, true}};

    CheckResult _ = check(R"(
        type function t0(a)
            error("test")
        end
        local v: t0<string & number>
    )");
    TypeArena arena;
    auto ty = requireType("v");
    auto simplifier = EqSatSimplification::newSimplifier(NotNull{&arena}, getBuiltins());
    auto simplified = eqSatSimplify(NotNull{simplifier.get()}, ty);
    REQUIRE(simplified);
    CHECK_EQ("t0<number & string>", toString(simplified->result)); // NOLINT(bugprone-unchecked-optional-access)
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_singleton_equality_bool")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    if (false) // FFlag::LuauEagerGeneralization4)
    {
        // FIXME: CLI-151985
        // This test breaks because we can't see that eq<type?, b> is already fully reduced.
        return;
    }

    CheckResult result = check(R"(
type function compare(arg)
    return types.singleton(types.singleton(false) == arg)
end

local function ok1(idx: compare<false>): true return idx end
local function ok2(idx: compare<true>): false return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_singleton_equality_string")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    if (false) // FFlag::LuauEagerGeneralization4)
    {
        // FIXME: CLI-151985
        // This test breaks because we can't see that eq<type?, b> is already fully reduced.
        return;
    }

    CheckResult result = check(R"(
type function compare(arg)
    return types.singleton(types.singleton("") == arg)
end

local function ok(idx: compare<"">): true return idx end
local function ok(idx: compare<"a">): false return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typeof_type_userdata_returns_type")
{
    ScopedFastFlag solverV2{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function test(t)
    print(typeof(t))
    return t
end

local _:test<number>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == R"(type)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_print_tab_char_fix")
{
    ScopedFastFlag solverV2{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function test(t)
            print(1,2)

            return t
        end

        local _:test<number>
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    // It should be \t and not \x1
    CHECK_EQ("1\t2", toString(result.errors[0]));
}

TEST_CASE_FIXTURE(ExternTypeFixture, "udtf_class_parent_ops")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function readparentof(arg)
            return arg:readparent()
        end

        type function writeparentof(arg)
            return arg:writeparent()
        end

        local function ok1(idx: readparentof<ChildClass>): BaseClass return idx end
        local function ok2(idx: writeparentof<ChildClass>): BaseClass return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typecheck_success")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function foo(x: type)
    return types.singleton(x.tag)
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "typecheck_failure")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function foo()
    return types.singleton({1})
end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "outer_generics_irreducible")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function func(t)
    return t
end

type wrap<T> = { a: func<T?> }

local x: wrap<string> = nil :: any
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(toString(requireType("x"), ToStringOptions{true}) == "{ a: string? }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "inner_generics_reducible")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function func(t)
    return t
end

type wrap<T> = { a: func<<T>(T) -> number>, b: T }

local x: wrap<string> = nil :: any
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(toString(requireType("x"), ToStringOptions{true}) == "{ a: <T>(T) -> number, b: string }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "blocking_nested_pending_expansions")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
type function func(t)
    return t
end

type test<T> = { x: T, y: T? }
type wrap<T> = { a: func<(string, keyof<test<T>>) -> number>, b: T }
local x: wrap<string>
local y: keyof<typeof(x)>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(toString(requireType("x"), ToStringOptions{true}) == R"({ a: (string, "x" | "y") -> number, b: string })");
    CHECK(toString(requireType("y"), ToStringOptions{true}) == R"("a" | "b")");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "blocking_nested_pending_expansions_2")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function foo(t)
    return types.unionof(t, types.singleton(nil))
end

local x: foo<{a: foo<string>, b: foo<number>}> = nil
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(toString(requireType("x"), ToStringOptions{true}) == "{ a: string?, b: number? }?");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "irreducible_pending_expansions")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
type function foo(t)
    return types.unionof(t, types.singleton(nil))
end

type table<T> = { a: index<T, "a"> }
type wrap<T> = foo<table<T>>

local x: wrap<{a: number}> = { a = 2 }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(toString(requireType("x"), ToStringOptions{true}) == "{ a: number }?");
}

TEST_CASE_FIXTURE(Fixture, "typeof_is_not_a_valid_type_function_name")
{
    ScopedFastFlag _{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
        type function typeof(t)
	        return t
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    CHECK("typeof cannot be used as an identifier for a type function or alias" == toString(result.errors[0]));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_alias_call")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type Test<T> = T?

type function foo(t)
    return Test(t)
end

local x: foo<{a: number}> = { a = 2 }
local y: foo<{b: number}> = { b = 2 }
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(toString(requireType("x"), ToStringOptions{true}) == "{ a: number }?");
    CHECK(toString(requireType("y"), ToStringOptions{true}) == "{ b: number }?");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_alias_values")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type Test = { a: number }

type function foo(t)
    return types.unionof(Test, t)
end

local x: foo<nil> = { a = 2 }
local y: foo<string> = "a"
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(toString(requireType("x"), ToStringOptions{true}) == "{ a: number }?");
    CHECK(toString(requireType("y"), ToStringOptions{true}) == "string | { a: number }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_alias_call_with_reduction")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
type Test<T> = rawget<T, "a">

type function foo(t)
    return Test(t)
end

local x: foo<{ a: number }> = 2
local y: foo<{ a: string }> = "x"
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(toString(requireType("x"), ToStringOptions{true}) == "number");
    CHECK(toString(requireType("y"), ToStringOptions{true}) == "string");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_alias_implicit_export")
{
    if (!FFlag::LuauSolverV2)
        return;

    fileResolver.source["game/A"] = R"(
type Test<T> = rawget<T, "a">

export type function foo(t)
    return Test(t)
end
local x: foo<{ a: number }> = 2
return {}
    )";

    CheckResult aResult = getFrontend().check("game/A");
    LUAU_REQUIRE_NO_ERRORS(aResult);

    CHECK(toString(requireType("game/A", "x")) == R"(number)");

    CheckResult bResult = check(R"(
local Test = require(game.A);
local y: Test.foo<{ a: string }> = "x"
    )");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    CHECK(toString(requireType("y")) == R"(string)");
}

TEST_CASE_FIXTURE(ExternTypeFixture, "type_alias_not_too_many_globals")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function get()
    return number
end
local function ok(idx: get<>): number return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(5, result);
    CHECK(toString(result.errors[0]) == R"(Unknown global 'number')");
}

TEST_CASE_FIXTURE(ExternTypeFixture, "type_alias_not_enough_arguments")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type Test<A, B> = (a: A, b: B) -> A

type function get()
    return Test(types.number)
end

local function ok(idx: get<>): number return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(toString(result.errors[0]) == R"('get' type function errored at runtime: [string "get"]:5: not enough arguments to call)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_alias_can_call_packs")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type Test<T, U...> = (U...) -> T

type function foo(t)
    return Test(types.number, types.string, t)
end

local x: foo<boolean>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(toString(requireType("x"), ToStringOptions{true}) == "(string, boolean) -> number");
}

TEST_CASE_FIXTURE(ExternTypeFixture, "type_alias_reduction_errors")
{
    if (!FFlag::LuauSolverV2)
        return;

    CheckResult result = check(R"(
type Test<T, U> = setmetatable<T, U>

type function get()
    return Test(types.number, types.string)
end

local function ok(idx: get<>): number return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(toString(result.errors[1]) == R"(Type function instance get<> is uninhabited)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "type_alias_unreferenced_do_not_block")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function foo(t)
    return types.unionof(types.number, t)
end

type Test = foo<string>

local x: foo<boolean>
    )");

    LUAU_REQUIRE_NO_ERRORS(result);

    CHECK(toString(requireType("x"), ToStringOptions{true}) == "boolean | number");
}

TEST_CASE_FIXTURE(Fixture, "udtf_type_alias_registration_follows")
{
    LUAU_REQUIRE_ERRORS(check(R"(
export type t110 = ""type--"
function _<t32...,t0...,t0...,t0...>(...):(any)&(any)
end
if _ then
else
    export type t110 = ""type--"
    function _<t32...,t0...,t0...,t0...>(...):(any)&(any)
    end
end
    )"));
}

TEST_CASE_FIXTURE(Fixture, "udtf_metatable_serialization_follows")
{
    LUAU_REQUIRE_ERRORS(check(R"(
_ = setmetatable(_(),_),(_) or _ == _ or f
while _() do
export type function t0<O,I>(l0,l0)
end
type t39<A> = t0<{write [Vector3]:any},typeof(_),l0.t0,any>
end
    )"));
}

TEST_CASE_FIXTURE(Fixture, "udtf_double_definition")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
type function t0<A>()
end
type function t0<A>()
end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == R"(Redefinition of type 't0', previously defined at line 2)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_fuzz_environment_scope_crash")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};

    CheckResult result = check(R"(
local _, running = ...
type function t255() end
if _ then
    type function t1() end
    type function t6(l0,...) end
    type function t255<A...>() end
    export type function t0<A>() end
else
    type function t1(...) end
    type function t66<A...>(...) end
    type function t255() end
    if running then
        export type function t255() end
        type function t0(l0) end
    end
end
type function t0(l0,...) end
export type function t66(...)
    export type function t255() end
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "oss_1887_udtf_with_optional_missing")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauInstantiateResolvedTypeFunctions, true},
    };

    CheckResult results = check(R"(
        type function create_table_with_key()
            local tbl = types.newtable()
            tbl:setproperty(types.singleton "key", types.unionof(types.string, types.singleton(nil)))
            return tbl
        end
        local a: create_table_with_key = {}
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, results);
    LUAU_REQUIRE_ERROR(results, UnappliedTypeFunction);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "oss_1887_udtf_with_optional_present")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauInstantiateResolvedTypeFunctions, true},
    };

    CheckResult results = check(R"(
        type function create_table_with_key()
            local tbl = types.newtable()
            tbl:setproperty(types.singleton "key", types.unionof(types.string, types.singleton(nil)))
            return tbl
        end
        local a: create_table_with_key = { key = "123" }
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, results);
    LUAU_REQUIRE_ERROR(results, UnappliedTypeFunction);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "oss_1887_udtf_table_mismatch")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauInstantiateResolvedTypeFunctions, true},
    };

    CheckResult results = check(R"(
        type function create_table_with_key()
            local tbl = types.newtable()
            tbl:setproperty(types.singleton "key", types.optional(types.number))
            return tbl
        end
        local my_tbl: create_table_with_key = {key = "123"}
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, results);
    CHECK(get<UnappliedTypeFunction>(results.errors[0]));
    auto err = get<TypeMismatch>(results.errors[1]);
    REQUIRE(err);
    CHECK_EQ("string", toString(err->givenType));
    CHECK_EQ("number?", toString(err->wantedType));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "oss_1887_basic_mismatch")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauInstantiateResolvedTypeFunctions, true},
    };

    CheckResult results = check(R"(
        type function foo()
            return types.number
        end
        local f: foo = "123"
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, results);
    CHECK(get<UnappliedTypeFunction>(results.errors[0]));
    auto err = get<TypeMismatch>(results.errors[1]);
    REQUIRE(err);
    CHECK_EQ("string", toString(err->givenType));
    CHECK_EQ("number", toString(err->wantedType));
}

TEST_CASE_FIXTURE(BuiltinsFixture, "oss_1887_basic_match")
{
    ScopedFastFlag sffs[] = {
        {FFlag::LuauSolverV2, true},
        {FFlag::LuauInstantiateResolvedTypeFunctions, true},
    };

    CheckResult results = check(R"(
        type function foo()
            return types.string
        end
        local f: foo = "123"
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, results);
    LUAU_REQUIRE_ERROR(results, UnappliedTypeFunction);
}

TEST_SUITE_END();
