// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details

#include "ClassFixture.h"
#include "Fixture.h"

#include "doctest.h"

using namespace Luau;

LUAU_FASTFLAG(LuauSolverV2)
LUAU_FASTFLAG(LuauUserDefinedTypeFunctionsSyntax2)
LUAU_FASTFLAG(LuauUserTypeFunFixRegister)
LUAU_FASTFLAG(LuauUserTypeFunFixNoReadWrite)
LUAU_FASTFLAG(LuauUserTypeFunFixMetatable)
LUAU_FASTFLAG(LuauUserDefinedTypeFunctionResetState)
LUAU_FASTFLAG(LuauUserTypeFunNonstrict)
LUAU_FASTFLAG(LuauUserTypeFunExportedAndLocal)
LUAU_FASTFLAG(LuauUserDefinedTypeFunParseExport)
LUAU_FASTFLAG(LuauUserTypeFunThreadBuffer)

TEST_SUITE_BEGIN("UserDefinedTypeFunctionTests");

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_nil_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};
    ScopedFastFlag luauUserTypeFunFixRegister{FFlag::LuauUserTypeFunFixRegister, true};
    ScopedFastFlag luauUserTypeFunThreadBuffer{FFlag::LuauUserTypeFunThreadBuffer, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

    CheckResult result = check(R"(
        type function serialize_union(arg)
            return arg
        end
        type type_being_serialized = number | string | boolean
        -- forcing an error here to check the exact type of the union
        local function ok(idx: serialize_union<type_being_serialized>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "boolean | number | string");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_union_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "boolean | number | string");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_intersection_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

    CheckResult result = check(R"(
        type function serialize_intersection(arg)
            return arg
        end
        type type_being_serialized = { boolean: boolean, number: number } & { boolean: boolean, string: string }
        -- forcing an error here to check the exact type of the intersection
        local function ok(idx: serialize_intersection<type_being_serialized>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "{ boolean: boolean, number: number } & { boolean: boolean, string: string }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_intersection_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "{ boolean: boolean, number: number } & { boolean: boolean, string: string }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_negation_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "~string");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_table_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

    CheckResult result = check(R"(
        type function serialize_table(arg)
            return arg
        end
        type type_being_serialized = { boolean: boolean, number: number, [string]: number }
        -- forcing an error here to check the exact type of the table
        local function ok(idx: serialize_table<type_being_serialized>): nil return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "{ [string]: number, boolean: boolean, number: number }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_table_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "{ [boolean]: string, number: string }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_metatable_methods_work")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "{boolean}");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_function_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

    CheckResult result = check(R"(
        type function getfunction()
            local ty = types.newfunction(nil, nil) -- () -> ()
            ty:setparameters({types.string, types.number}, nil) -- (string, number) -> ()
            ty:setreturns(nil, types.boolean) -- (string, number) -> (...boolean)
            if ty:is("function") then
                -- creating a copy of `ty` parameters
                local arr = {}
                for index, val in ty:parameters().head do
                    table.insert(arr, val)
                end
                return types.newfunction({head = arr}, ty:returns()) -- (string, number) -> (...boolean)
            end
            -- this should never be returned
            return types.number
        end
        local function ok(idx: getfunction<>): never return idx end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "(string, number) -> (...boolean)");
}

TEST_CASE_FIXTURE(ClassFixture, "udtf_class_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

    CheckResult result = check(R"(
        type function serialize_class(arg)
            return arg
        end
        local function ok(idx: serialize_class<BaseClass>): BaseClass return idx end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(ClassFixture, "udtf_class_methods_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};


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
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "{ BaseField: number, read BaseMethod: (BaseClass, number) -> (), read Touched: Connection }");
}

TEST_CASE_FIXTURE(ClassFixture, "write_of_readonly_is_nil")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};
    ScopedFastFlag udtfRwFix{FFlag::LuauUserTypeFunFixNoReadWrite, true};


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
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "false");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_check_mutability")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

    CheckResult result = check(R"(
        type function checkmut()
            local indexer = {
                index = types.number,
                readresult = types.boolean,
                writeresult = types.boolean,
            }
            local ty = types.newtable(props, indexer, nil) -- {[number]: boolean}
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
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "{ @metatable {boolean}, {  } }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_copy_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "{ @metatable { [number]: boolean, string: number }, {  } }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_simple_cyclic_serialization_works")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

    CheckResult result = check(R"(
        type function badmetatable()
            return types.newtable(nil, nil, types.number)
        end
        local function bad(arg: badmetatable<>) end
    )");

    LUAU_CHECK_ERROR_COUNT(4, result); // There are 2 type function uninhabited error, 2 user defined type function error
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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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

    LUAU_CHECK_ERROR_COUNT(4, result); // There are 2 type function uninhabited error, 2 user defined type function error
    UserDefinedTypeFunctionError* e = get<UserDefinedTypeFunctionError>(result.errors[0]);
    REQUIRE(e);
    CHECK(e->message == "'errors_if_string' type function errored at runtime: [string \"errors_if_string\"]:5: We are in a math class! not english");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_type_overrides_call_metamethod")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

    CheckResult result = check(R"(
        type function hello(arg)
            error(type(arg))
        end
        local function ok(idx: hello<string>): nil return idx end
    )");

    LUAU_CHECK_ERROR_COUNT(4, result); // There are 2 type function uninhabited error, 2 user defined type function error
    UserDefinedTypeFunctionError* e = get<UserDefinedTypeFunctionError>(result.errors[0]);
    REQUIRE(e);
    CHECK(e->message == "'hello' type function errored at runtime: [string \"hello\"]:3: userdata");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_type_overrides_eq_metamethod")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
        end
        local function ok(idx: hello<>): number return idx end
    )");

    LUAU_CHECK_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_function_type_cant_call_get_props")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

    CheckResult result = check(R"(
        type function hello(arg)
            local arr = arg:properties()
        end
        local function ok(idx: hello<() -> ()>): nil return idx end
    )");

    LUAU_CHECK_ERROR_COUNT(4, result); // There are 2 type function uninhabited error, 2 user defined type function error
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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

    CheckResult result = check(R"(
        type function foo()
            return "hi"
        end
        type function bar()
            return types.singleton(foo())
        end
        local function ok(idx: bar<>): nil return idx end
    )");

    LUAU_CHECK_ERROR_COUNT(1, result);
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "\"hi\"");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_no_shared_state")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    LUAU_CHECK_ERROR_COUNT(8, result);
    CHECK(toString(result.errors[0]) == R"('bar' type function errored at runtime: [string "foo"]:4: attempt to modify a readonly table)");
    CHECK(toString(result.errors[1]) == R"(Type function instance bar<"x"> is uninhabited)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_math_reset")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};
    ScopedFastFlag luauUserDefinedTypeFunctionResetState{FFlag::LuauUserDefinedTypeFunctionResetState, true};

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
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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

    LUAU_CHECK_ERROR_COUNT(1, result);
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "{ age: number?, alive: boolean?, name: string? }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_calling_illegal_global")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

    CheckResult result = check(R"(
        type function illegal(arg)
            gcinfo() -- this should error

            return arg -- this should not be reached
        end
        
        local function ok(idx: illegal<number>): nil return idx end
    )");

    LUAU_CHECK_ERROR_COUNT(4, result); // There are 2 type function uninhabited error, 2 user defined type function error
    UserDefinedTypeFunctionError* e = get<UserDefinedTypeFunctionError>(result.errors[0]);
    REQUIRE(e);
    CHECK(e->message == "'illegal' type function errored at runtime: [string \"illegal\"]:3: this function is not supported in type functions");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_recursion_and_gc")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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

    LUAU_CHECK_ERROR_COUNT(1, result);
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_recovery_no_upvalues")
{
    ScopedFastFlag solverV2{FFlag::LuauSolverV2, true};
    ScopedFastFlag userDefinedTypeFunctionsSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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

    LUAU_CHECK_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == R"(Type function cannot reference outer local 'var')");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_follow")
{
    ScopedFastFlag solverV2{FFlag::LuauSolverV2, true};
    ScopedFastFlag userDefinedTypeFunctionsSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

    CheckResult result = check(R"(
        type t0 = any
        type function t0()
            return types.any
        end
    )");

    LUAU_CHECK_ERROR_COUNT(1, result);
    CHECK(toString(result.errors[0]) == R"(Redefinition of type 't0', previously defined at line 2)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "udtf_strip_indexer")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};

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
    TypePackMismatch* tpm = get<TypePackMismatch>(result.errors[0]);
    REQUIRE(tpm);
    CHECK(toString(tpm->givenTp) == "{ foo: string }");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "no_type_methods_on_types")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};
    ScopedFastFlag luauUserTypeFunFixRegister{FFlag::LuauUserTypeFunFixRegister, true};

    CheckResult result = check(R"(
        type function test(x)
            return if types.is(x, "number") then types.string else types.boolean
        end
        local function ok(tbl: test<number>): never return tbl end
    )");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    CHECK(toString(result.errors[0]) == R"('test' type function errored at runtime: [string "test"]:3: attempt to call a nil value)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "no_types_functions_on_type")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};
    ScopedFastFlag luauUserTypeFunFixRegister{FFlag::LuauUserTypeFunFixRegister, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};
    ScopedFastFlag luauUserTypeFunFixRegister{FFlag::LuauUserTypeFunFixRegister, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};
    ScopedFastFlag luauUserTypeFunFixRegister{FFlag::LuauUserTypeFunFixRegister, true};

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
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};
    ScopedFastFlag luauUserTypeFunFixRegister{FFlag::LuauUserTypeFunFixRegister, true};

    CheckResult result = check(R"(
        type function test(x)
            return types.singleton(x.tag)
        end

        local function ok1(tbl: test<number>): never return tbl end
        local function ok2(tbl: test<string>): never return tbl end
        local function ok3(tbl: test<{}>): never return tbl end
    )");

    LUAU_REQUIRE_ERROR_COUNT(3, result);
    CHECK(toString(result.errors[0]) == R"(Type pack '"number"' could not be converted into 'never'; at [0], "number" is not a subtype of never)");
    CHECK(toString(result.errors[1]) == R"(Type pack '"string"' could not be converted into 'never'; at [0], "string" is not a subtype of never)");
    CHECK(toString(result.errors[2]) == R"(Type pack '"table"' could not be converted into 'never'; at [0], "table" is not a subtype of never)");
}

TEST_CASE_FIXTURE(BuiltinsFixture, "metatable_serialization")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};
    ScopedFastFlag luauUserTypeFunFixRegister{FFlag::LuauUserTypeFunFixRegister, true};
    ScopedFastFlag luauUserTypeFunFixMetatable{FFlag::LuauUserTypeFunFixMetatable, true};

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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};
    ScopedFastFlag luauUserTypeFunFixRegister{FFlag::LuauUserTypeFunFixRegister, true};
    ScopedFastFlag luauUserTypeFunNonstrict{FFlag::LuauUserTypeFunNonstrict, true};

    CheckResult result = check(R"(
--!nonstrict
type function foo() return types.string end
local a: foo<> = "a"
    )");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "implicit_export")
{
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};
    ScopedFastFlag luauUserTypeFunFixRegister{FFlag::LuauUserTypeFunFixRegister, true};
    ScopedFastFlag luauUserTypeFunExportedAndLocal{FFlag::LuauUserTypeFunExportedAndLocal, true};

    fileResolver.source["game/A"] = R"(
type function concat(a, b)
    return types.singleton(a:value() .. b:value())
end
export type Concat<T, U> = concat<T, U>
local a: concat<'first', 'second'>
return {}
    )";

    CheckResult aResult = frontend.check("game/A");
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
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};
    ScopedFastFlag luauUserTypeFunFixRegister{FFlag::LuauUserTypeFunFixRegister, true};
    ScopedFastFlag luauUserTypeFunExportedAndLocal{FFlag::LuauUserTypeFunExportedAndLocal, true};

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
    ScopedFastFlag newSolver{FFlag::LuauSolverV2, true};
    ScopedFastFlag udtfSyntax{FFlag::LuauUserDefinedTypeFunctionsSyntax2, true};
    ScopedFastFlag luauUserTypeFunFixRegister{FFlag::LuauUserTypeFunFixRegister, true};
    ScopedFastFlag luauUserTypeFunExportedAndLocal{FFlag::LuauUserTypeFunExportedAndLocal, true};
    ScopedFastFlag luauUserDefinedTypeFunParseExport{FFlag::LuauUserDefinedTypeFunParseExport, true};

    fileResolver.source["game/A"] = R"(
export type function concat(a, b)
    return types.singleton(a:value() .. b:value())
end
local a: concat<'first', 'second'>
return {}
    )";

    CheckResult aResult = frontend.check("game/A");
    LUAU_REQUIRE_NO_ERRORS(aResult);

    CHECK(toString(requireType("game/A", "a")) == R"("firstsecond")");

    CheckResult bResult = check(R"(
local Test = require(game.A);
local b: Test.concat<'third', 'fourth'>
    )");
    LUAU_REQUIRE_NO_ERRORS(bResult);

    CHECK(toString(requireType("b")) == R"("thirdfourth")");
}

TEST_SUITE_END();
