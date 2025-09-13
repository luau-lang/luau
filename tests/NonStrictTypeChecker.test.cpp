// This file is part of the Luau programming language and is licensed under MIT License; see LICENSE.txt for details
#include "Luau/NonStrictTypeChecker.h"

#include "Fixture.h"

#include "Luau/Ast.h"
#include "Luau/BuiltinDefinitions.h"
#include "Luau/Common.h"
#include "Luau/Error.h"
#include "Luau/IostreamHelpers.h"
#include "Luau/ModuleResolver.h"
#include "Luau/VisitType.h"

#include "ScopedFlags.h"
#include "doctest.h"
#include <iostream>

LUAU_FASTFLAG(LuauNewNonStrictMoreUnknownSymbols)
LUAU_FASTFLAG(LuauNewNonStrictNoErrorsPassingNever)
LUAU_FASTFLAG(LuauNewNonStrictSuppressesDynamicRequireErrors)
LUAU_FASTFLAG(LuauNewNonStrictReportsOneIndexedErrors)
LUAU_FASTFLAG(LuauUnreducedTypeFunctionsDontTriggerWarnings)

using namespace Luau;

#define NONSTRICT_REQUIRE_ERR_AT_POS(pos, result, idx) \
    do \
    { \
        auto pos_ = (pos); \
        bool foundErr = false; \
        int index = 0; \
        for (const auto& err : result.errors) \
        { \
            if (err.location.begin == pos_) \
            { \
                foundErr = true; \
                break; \
            } \
            index++; \
        } \
        REQUIRE_MESSAGE(foundErr, "Expected error at " << pos_); \
        idx = index; \
    } while (false)

#define NONSTRICT_REQUIRE_CHECKED_ERR(pos, name, result) \
    do \
    { \
        int errIndex; \
        NONSTRICT_REQUIRE_ERR_AT_POS(pos, result, errIndex); \
        auto err = get<CheckedFunctionCallError>(result.errors[errIndex]); \
        REQUIRE(err != nullptr); \
        CHECK_EQ(err->checkedFunctionName, name); \
    } while (false)

#define NONSTRICT_REQUIRE_FUNC_DEFINITION_ERR(pos, argname, result) \
    do \
    { \
        int errIndex; \
        NONSTRICT_REQUIRE_ERR_AT_POS(pos, result, errIndex); \
        auto err = get<NonStrictFunctionDefinitionError>(result.errors[errIndex]); \
        REQUIRE(err != nullptr); \
        CHECK_EQ(err->argument, argname); \
    } while (false)


struct NonStrictTypeCheckerFixture : Fixture
{

    NonStrictTypeCheckerFixture() {}

    CheckResult checkNonStrict(const std::string& code)
    {
        ScopedFastFlag flags[] = {
            {FFlag::LuauSolverV2, true},
        };
        LoadDefinitionFileResult res = loadDefinition(definitions);
        LUAU_ASSERT(res.success);
        return check(Mode::Nonstrict, code);
    }

    CheckResult checkNonStrictModule(const std::string& moduleName)
    {
        ScopedFastFlag flags[] = {
            {FFlag::LuauSolverV2, true},
        };
        LoadDefinitionFileResult res = loadDefinition(definitions);
        LUAU_ASSERT(res.success);
        return getFrontend().check(moduleName);
    }

    Frontend& getFrontend() override
    {
        if (frontend)
            return *frontend;

        Frontend& f = Fixture::getFrontend();
        registerHiddenTypes(f);
        registerTestTypes();
        return *frontend;
    }

    std::string definitions = R"BUILTIN_SRC(
@checked declare function abs(n: number): number
@checked declare function lower(s: string): string
declare function cond() : boolean
@checked declare function contrived(n : Not<number>) : number

-- interesting types of things that we would like to mark as checked
@checked declare function onlyNums(...: number) : number
@checked declare function mixedArgs(x: string, ...: number) : number
@checked declare function optionalArg(x: string?) : number
declare foo: {
    bar: @checked (number) -> number,
}

@checked declare function optionalArgsAtTheEnd1(x: string, y: number?, z: number?) : number
@checked declare function optionalArgsAtTheEnd2(x: string, y: number?, z: string) : number

type DateTypeArg = {
    year: number,
    month: number,
    day: number,
    hour: number?,
    min: number?,
    sec: number?,
    isdst: boolean?,
}

declare os : {
    time: @checked (time: DateTypeArg?) -> number
}

@checked declare function require(target : any) : any
@checked declare function getAllTheArgsWrong(one: string, two: number, three: boolean) : any
)BUILTIN_SRC";
};

TEST_SUITE_BEGIN("NonStrictTypeCheckerTest");

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "interesting_checked_functions")
{
    CheckResult result = checkNonStrict(R"(
onlyNums(1,1,1)
onlyNums(1, "a")

mixedArgs("a", 1, 2)
mixedArgs(1, 1, 1)
mixedArgs("a", true)

optionalArg(nil)
optionalArg("a")
optionalArg(3)
)");

    LUAU_REQUIRE_ERROR_COUNT(4, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 12), "onlyNums", result); // onlyNums(1, "a")

    NONSTRICT_REQUIRE_CHECKED_ERR(Position(5, 10), "mixedArgs", result); // mixedArgs(1, 1, 1)
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(6, 15), "mixedArgs", result); // mixedArgs("a", true)

    NONSTRICT_REQUIRE_CHECKED_ERR(Position(10, 12), "optionalArg", result); // optionalArg(3)
}


TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "simple_negation_caching_example")
{
    CheckResult result = checkNonStrict(R"(
local x = 3
abs(x)
abs(x)
)");

    LUAU_REQUIRE_NO_ERRORS(result);

    result = checkNonStrict(R"(
local x = 3
contrived(x)
contrived(x)
			      )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 10), "contrived", result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(3, 10), "contrived", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "simple_non_strict_failure")
{
    CheckResult result = checkNonStrict(R"BUILTIN_SRC(
abs("hi")
)BUILTIN_SRC");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(1, 4), "abs", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "nested_function_calls_constant")
{
    CheckResult result = checkNonStrict(R"(
local x
abs(lower(x))
)");

    if (FFlag::LuauNewNonStrictMoreUnknownSymbols)
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 4), "abs", result);
        NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 10), "lower", result);
    }
    else
    {

        LUAU_REQUIRE_ERROR_COUNT(1, result);
        NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 4), "abs", result);
    }
}


TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_else_does_not_warn_with_never_local")
{
    CheckResult result = checkNonStrict(R"(
local x : never
if cond() then
    abs(x)
else
    lower(x)
end
)");
    if (FFlag::LuauNewNonStrictNoErrorsPassingNever)
        LUAU_REQUIRE_NO_ERRORS(result);
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        NONSTRICT_REQUIRE_CHECKED_ERR(Position(3, 8), "abs", result);
        NONSTRICT_REQUIRE_CHECKED_ERR(Position(5, 10), "lower", result);
    }
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_else_warns_nil_branches")
{
    auto result = checkNonStrict(R"(
local x
if cond() then
    abs(x)
else
    lower(x)
end
)");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(3, 8), "abs", result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(5, 10), "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_else_doesnt_warn_else_branch")
{
    auto result = checkNonStrict(R"(
local x : string = "hi"
if cond() then
    abs(x)
else
    lower(x)
end
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(3, 8), "abs", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_no_else")
{
    CheckResult result = checkNonStrict(R"(
local x : string
if cond() then
    abs(x)
end
)");

    if (FFlag::LuauNewNonStrictMoreUnknownSymbols)
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        NONSTRICT_REQUIRE_CHECKED_ERR(Position(3, 8), "abs", result);
    }
    else
        LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_no_else_err_in_cond")
{
    CheckResult result = checkNonStrict(R"(
local x : string = ""
if abs(x) then
    lower(x)
end
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 7), "abs", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_else_expr_should_warn")
{
    CheckResult result = checkNonStrict(R"(
local x = 42
local y = if cond() then abs(x) else lower(x)
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 43), "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_else_expr_should_not_warn_for_never")
{
    CheckResult result = checkNonStrict(R"(
local x : never
local y = if cond() then abs(x) else lower(x)
)");

    if (FFlag::LuauNewNonStrictNoErrorsPassingNever)
        LUAU_REQUIRE_NO_ERRORS(result);
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 29), "abs", result);
        NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 43), "lower", result);
    }
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "if_then_else_expr_doesnt_warn_else_branch")
{
    CheckResult result = checkNonStrict(R"(
local x : string = "hi"
local y = if cond() then abs(x) else lower(x)
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 29), "abs", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "sequencing_if_checked_call")
{
    CheckResult result = checkNonStrict(R"(
local x
if cond() then
  x = 5
else
  x = nil
end
lower(x)
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(7, 6), "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_unrelated_checked_calls")
{
    CheckResult result = checkNonStrict(R"(
function h(x, y)
    abs(x)
    lower(y)
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_basic_no_errors")
{
    CheckResult result = checkNonStrict(R"(
function f(x)
    abs(x)
end
)");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_basic_errors")
{
    CheckResult result = checkNonStrict(R"(
function f(x : string)
    abs(x)
end
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 8), "abs", result);
}


TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_failure")
{
    CheckResult result = checkNonStrict(R"(
function f(x)
    abs(lower(x))
end
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 8), "abs", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_sequencing_errors")
{
    CheckResult result = checkNonStrict(R"(
function f(x)
    abs(x)
    lower(x)
end
)");


    if (FFlag::LuauNewNonStrictNoErrorsPassingNever)
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        NONSTRICT_REQUIRE_FUNC_DEFINITION_ERR(Position(1, 11), "x", result);
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(3, result);
        NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 8), "abs", result);
        NONSTRICT_REQUIRE_CHECKED_ERR(Position(3, 10), "lower", result);
        NONSTRICT_REQUIRE_FUNC_DEFINITION_ERR(Position(1, 11), "x", result);
    }
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_sequencing_errors_2")
{
    CheckResult result = checkNonStrict(R"(
local t = {function(x)
    abs(x)
    lower(x)
end}
)");

    if (FFlag::LuauNewNonStrictNoErrorsPassingNever)
    {
        LUAU_REQUIRE_ERROR_COUNT(1, result);
        CHECK(toString(result.errors[0]) == "Argument x with type 'unknown' is used in a way that will run time error");
    }
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(3, result);
        NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 8), "abs", result);
        NONSTRICT_REQUIRE_CHECKED_ERR(Position(3, 10), "lower", result);
        CHECK(toString(result.errors[2]) == "Argument x with type 'unknown' is used in a way that will run time error");
    }
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "local_fn_produces_error")
{
    CheckResult result = checkNonStrict(R"(
local x = 5
local function y() lower(x) end
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 25), "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "fn_expr_produces_error")
{
    CheckResult result = checkNonStrict(R"(
local x = 5
local y = function() lower(x) end
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 27), "lower", result);
}


TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_if_warns_never")
{
    CheckResult result = checkNonStrict(R"(
function f(x: never)
    if cond() then
        abs(x)
    else
        lower(x)
    end
end
)");

    if (FFlag::LuauNewNonStrictNoErrorsPassingNever)
        LUAU_REQUIRE_NO_ERRORS(result);
    else
    {
        LUAU_REQUIRE_ERROR_COUNT(2, result);
        NONSTRICT_REQUIRE_CHECKED_ERR(Position(3, 12), "abs", result);
        NONSTRICT_REQUIRE_CHECKED_ERR(Position(5, 14), "lower", result);
    }
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_if_no_else")
{
    CheckResult result = checkNonStrict(R"(
function f(x)
    if cond() then
        abs(x)
    end
end
)");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_if_assignment_errors")
{
    CheckResult result = checkNonStrict(R"(
function f(x)
    if cond() then
        x = 5
    else
        x = nil
    end
    lower(x)
end
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(7, 10), "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "function_def_if_assignment_no_errors")
{
    CheckResult result = checkNonStrict(R"(
function f(x : string | number)
    if cond() then
        x = 5
    else
        x = "hi"
    end
    abs(x)
end
)");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "local_only_one_warning")
{
    CheckResult result = checkNonStrict(R"(
local x = 5
lower(x)
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(2, 6), "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "phi_node_assignment")
{
    CheckResult result = checkNonStrict(R"(
local x = "a" -- x1
if cond() then
    x = 3 -- x2
end
lower(x) -- phi {x1, x2}
)");

    LUAU_REQUIRE_NO_ERRORS(result);
} //

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "phi_node_assignment_err")
{
    CheckResult result = checkNonStrict(R"(
local x = nil
if cond() then
    if cond() then
        x = 5
    end
    abs(x)
else
    lower(x)
end
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(8, 10), "lower", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "tblprop_is_checked")
{
    CheckResult result = checkNonStrict(R"(
foo.bar("hi")
)");
    LUAU_REQUIRE_ERROR_COUNT(1, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(1, 8), "foo.bar", result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "exprgroup_is_checked")
{
    CheckResult result = checkNonStrict(R"(
        local foo = (abs("foo"))
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto r1 = get<CheckedFunctionCallError>(result.errors[0]);
    LUAU_ASSERT(r1);
    CHECK_EQ("abs", r1->checkedFunctionName);
    CHECK_EQ("number", toString(r1->expected));
    CHECK_EQ("string", toString(r1->passed));
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "binop_is_checked")
{
    CheckResult result = checkNonStrict(R"(
        local foo = 4 + abs("foo")
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);

    auto r1 = get<CheckedFunctionCallError>(result.errors[0]);
    LUAU_ASSERT(r1);
    CHECK_EQ("abs", r1->checkedFunctionName);
    CHECK_EQ("number", toString(r1->expected));
    CHECK_EQ("string", toString(r1->passed));
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "incorrect_arg_count")
{
    CheckResult result = checkNonStrict(R"(
foo.bar(1,2,3)
abs(3, "hi");
)");
    LUAU_REQUIRE_ERROR_COUNT(2, result);
    auto r1 = get<CheckedFunctionIncorrectArgs>(result.errors[0]);
    auto r2 = get<CheckedFunctionIncorrectArgs>(result.errors[1]);
    LUAU_ASSERT(r1);
    LUAU_ASSERT(r2);
    CHECK_EQ("abs", r1->functionName);
    CHECK_EQ("foo.bar", r2->functionName);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "optionals_in_checked_function_can_be_omitted")
{
    CheckResult result = checkNonStrict(R"(
optionalArgsAtTheEnd1("a")
optionalArgsAtTheEnd1("a", 3)
optionalArgsAtTheEnd1("a", nil, 3)
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "generic_type_packs_in_non_strict")
{
    CheckResult result = checkNonStrict(R"(
        --!nonstrict
        local test: <T...>(T...) -> () -- TypeError: Unknown type 'T'
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "optionals_in_checked_function_in_middle_cannot_be_omitted")
{
    CheckResult result = checkNonStrict(R"(
optionalArgsAtTheEnd2("a", "a") -- error
optionalArgsAtTheEnd2("a", nil, "b")
optionalArgsAtTheEnd2("a", 3, "b")
optionalArgsAtTheEnd2("a", "b", "c") -- error
)");
    LUAU_REQUIRE_ERROR_COUNT(3, result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(1, 27), "optionalArgsAtTheEnd2", result);
    NONSTRICT_REQUIRE_CHECKED_ERR(Position(4, 27), "optionalArgsAtTheEnd2", result);
    auto r1 = get<CheckedFunctionIncorrectArgs>(result.errors[2]);
    LUAU_ASSERT(r1);
    CHECK_EQ(3, r1->expected);
    CHECK_EQ(2, r1->actual);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "non_testable_type_throws_ice")
{
    CHECK_THROWS_AS(
        checkNonStrict(R"(
os.time({year = 0, month = 0, day = 0, min = 0, isdst = nil})
)"),
        Luau::InternalCompilerError
    );
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "non_strict_shouldnt_warn_on_require_module")
{
    fileResolver.source["Modules/A"] = R"(
--!strict
type t = {x : number}
local e : t = {x = 3}
return e
)";
    fileResolver.sourceTypes["Modules/A"] = SourceCode::Module;

    fileResolver.source["Modules/B"] = R"(
--!nonstrict
local E = require(script.Parent.A)
)";

    CheckResult result = checkNonStrictModule("Modules/B");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "nonstrict_shouldnt_warn_on_valid_buffer_use")
{
    loadDefinition(R"(
declare buffer: {
    create: @checked (size: number) -> buffer,
    readi8: @checked (b: buffer, offset: number) -> number,
    writef64: @checked (b: buffer, offset: number, value: number) -> (),
}
)");

    CheckResult result = checkNonStrict(R"(
local b = buffer.create(100)
buffer.writef64(b, 0, 5)
buffer.readi8(b, 0)
)");
    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "nonstrict_method_calls")
{
    Luau::unfreeze(getFrontend().globals.globalTypes);
    Luau::unfreeze(getFrontend().globalsForAutocomplete.globalTypes);

    registerBuiltinGlobals(getFrontend(), getFrontend().globals);
    registerTestTypes();

    Luau::freeze(getFrontend().globals.globalTypes);
    Luau::freeze(getFrontend().globalsForAutocomplete.globalTypes);

    CheckResult result = checkNonStrict(R"(
        local test = "test"
        test:lower()
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "unknown_globals_in_non_strict_1")
{
    CheckResult result = check(Mode::Nonstrict, R"(
        foo = 5
        local wrong1 = foob

        local x = 12
        local wrong2 = x + foblm
    )");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "unknown_types_in_non_strict")
{
    CheckResult result = check(Mode::Nonstrict, R"(
        --!nonstrict
        local foo: Foo = 1
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const UnknownSymbol* err = get<UnknownSymbol>(result.errors[0]);
    CHECK_EQ(err->name, "Foo");
    CHECK_EQ(err->context, UnknownSymbol::Context::Type);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "unknown_types_in_non_strict_2")
{
    CheckResult result = check(Mode::Nonstrict, R"(
        --!nonstrict
        local foo = 1 :: Foo
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const UnknownSymbol* err = get<UnknownSymbol>(result.errors[0]);
    CHECK_EQ(err->name, "Foo");
    CHECK_EQ(err->context, UnknownSymbol::Context::Type);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "buffer_is_not_unknown")
{
    CheckResult result = check(Mode::Nonstrict, R"(
local function wrap(b: buffer, i: number, v: number)
    buffer.writeu32(b, i * 4, v)
end
    )");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "incomplete_function_annotation")
{
    CheckResult result = check(Mode::Nonstrict, R"(
        local x: () ->
    )");

    LUAU_REQUIRE_ERRORS(result);
}

TEST_CASE_FIXTURE(Fixture, "unknown_globals_in_function_calls")
{
    ScopedFastFlag sff{FFlag::LuauNewNonStrictMoreUnknownSymbols, true};

    CheckResult result = check(Mode::Nonstrict, R"(
        local function foo() : ()
            bar()
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const UnknownSymbol* err = get<UnknownSymbol>(result.errors[0]);
    CHECK_EQ(err->name, "bar");
    CHECK_EQ(err->context, UnknownSymbol::Context::Binding);
}

TEST_CASE_FIXTURE(Fixture, "unknown_globals_in_one_sided_conditionals")
{
    ScopedFastFlag sff{FFlag::LuauNewNonStrictMoreUnknownSymbols, true};

    CheckResult result = check(Mode::Nonstrict, R"(
        local function foo(cond) : ()
            if cond then
                bar()
            end
        end
    )");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const UnknownSymbol* err = get<UnknownSymbol>(result.errors[0]);
    CHECK_EQ(err->name, "bar");
    CHECK_EQ(err->context, UnknownSymbol::Context::Binding);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "new_non_strict_should_suppress_dynamic_require_errors")
{
    ScopedFastFlag sffs[] = {{FFlag::LuauSolverV2, true}, {FFlag::LuauNewNonStrictSuppressesDynamicRequireErrors, true}};
    // Avoid warning about dynamic requires in new nonstrict mode
    CheckResult result = check(Mode::Nonstrict, R"(
function passThrough(module)
    require(module)
end
    )");

    LUAU_REQUIRE_ERROR_COUNT(0, result);
    // We should still warn about dynamic requires in strict mode
    result = check(Mode::Strict, R"(
function passThrough(module)
    require(module)
end
)");

    LUAU_REQUIRE_ERROR_COUNT(1, result);
    const UnknownRequire* req = get<UnknownRequire>(result.errors[0]);
    CHECK(req != nullptr);
}

TEST_CASE_FIXTURE(BuiltinsFixture, "new_non_strict_should_suppress_unknown_require_errors")
{
    ScopedFastFlag sffs[] = {{FFlag::LuauSolverV2, true}, {FFlag::LuauNewNonStrictSuppressesDynamicRequireErrors, true}};

    // Avoid warning about dynamic requires in new nonstrict mode
    CheckResult result = check(Mode::Nonstrict, R"(
require(script.NonExistent)
require("@self/NonExistent")
    )");

    LUAU_REQUIRE_ERROR_COUNT(0, result);
    // We should still warn about dynamic requires in strict mode
    result = check(Mode::Strict, R"(
require(script.NonExistent)
require("@self/NonExistent")
)");

    LUAU_REQUIRE_ERROR_COUNT(2, result);
    const UnknownRequire* req1 = get<UnknownRequire>(result.errors[0]);
    CHECK(req1 != nullptr);
    const UnknownRequire* req2 = get<UnknownRequire>(result.errors[1]);
    CHECK(req2 != nullptr);
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "new_non_strict_stringifies_checked_function_errors_as_one_indexed")
{
    ScopedFastFlag sff = {FFlag::LuauNewNonStrictReportsOneIndexedErrors, true};
    CheckResult result = checkNonStrict(R"(
getAllTheArgsWrong(3, true, "what")
)");
    LUAU_REQUIRE_ERROR_COUNT(3, result);
    const CheckedFunctionCallError* err1 = get<CheckedFunctionCallError>(result.errors[0]);
    const CheckedFunctionCallError* err2 = get<CheckedFunctionCallError>(result.errors[1]);
    const CheckedFunctionCallError* err3 = get<CheckedFunctionCallError>(result.errors[2]);
    CHECK(err1 != nullptr);
    CHECK(err2 != nullptr);
    CHECK(err3 != nullptr);
    CHECK_EQ("Function 'getAllTheArgsWrong' expects 'string' at argument #1, but got 'number'", toString(result.errors[0]));
    CHECK_EQ("Function 'getAllTheArgsWrong' expects 'number' at argument #2, but got 'boolean'", toString(result.errors[1]));
    CHECK_EQ("Function 'getAllTheArgsWrong' expects 'boolean' at argument #3, but got 'string'", toString(result.errors[2]));
}

TEST_CASE_FIXTURE(NonStrictTypeCheckerFixture, "new_non_strict_skips_warnings_on_unreduced_typefunctions")
{
    ScopedFastFlag sff{FFlag::LuauUnreducedTypeFunctionsDontTriggerWarnings, true};
    CheckResult result = checkNonStrict(R"(
function foo(x)
    local y = x + 1
    return abs(y)
end
)");

    LUAU_REQUIRE_NO_ERRORS(result);
}

TEST_SUITE_END();
